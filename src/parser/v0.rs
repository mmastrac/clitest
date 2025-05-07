use std::{path::PathBuf, process::Output, sync::Arc};

use grok::Grok;

use crate::{
    command::CommandLine,
    script::{
        CommandExit, GrokPattern, OutputPattern, OutputPatternType, Script, ScriptCommand,
        ScriptError, ScriptErrorType, ScriptFile, ScriptLine, ScriptLocation,
    },
};

#[derive(Debug, Clone, derive_more::IsVariant, derive_more::Unwrap)]
enum BlockType {
    /// A command block.
    Command(CommandLine),
    /// Comments and whitespace lines.
    Ineffectual,
    /// Pattern lines.
    Pattern,
    /// Meta lines (`%EXPECT_FAILURE`, `%EXIT`, etc.).
    Meta,
    /// Any (`*`) block
    Any,
}

impl BlockType {
    fn is_same_type_as(&self, other: &Self) -> bool {
        match (self, other) {
            (BlockType::Command(_), BlockType::Command(_)) => true,
            (BlockType::Ineffectual, BlockType::Ineffectual) => true,
            (BlockType::Pattern, BlockType::Pattern) => true,
            (BlockType::Meta, BlockType::Meta) => true,
            (BlockType::Any, BlockType::Any) => true,
            _ => false,
        }
    }
}

struct ScriptV0Block {
    location: ScriptLocation,
    block_type: BlockType,
    lines: Vec<ScriptLine>,
}

impl ScriptV0Block {
    /// Take the current block, replacing with an empty block at the given location.
    pub fn take(&mut self, location: ScriptLocation, block_type: BlockType) -> Self {
        Self {
            location: std::mem::replace(&mut self.location, location),
            block_type: std::mem::replace(&mut self.block_type, block_type),
            lines: std::mem::take(&mut self.lines),
        }
    }

    /// Split the first pattern line from the rest. If not a pattern block,
    /// return None. May leave an empty block if the first line is the only line.
    pub fn split_first(&mut self) -> Option<Self> {
        match self.block_type {
            BlockType::Pattern => {
                let lines = &mut self.lines;
                if lines.is_empty() {
                    debug_assert!(false, "split_first called on empty pattern block");
                    return None;
                }
                let first = lines.remove(0);
                Some(Self {
                    location: first.location.clone(),
                    block_type: BlockType::Pattern,
                    lines: vec![first],
                })
            }
            _ => None,
        }
    }
}

impl std::fmt::Debug for ScriptV0Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            let indent = f.width().unwrap_or_default();
            let indent = " ".repeat(indent);
            // HACK: Repurpose width as indent
            // Left-pad by "indent" spaces
            let c = match self.block_type {
                BlockType::Command(_) => "$",
                BlockType::Ineffectual => "#",
                BlockType::Pattern => "",
                BlockType::Meta => "%",
                BlockType::Any => "*",
            };
            writeln!(f, "{indent}:{} {c}[", self.location.line)?;
            for line in &self.lines {
                writeln!(f, "{indent}  {:?}", line.text())?;
            }
            write!(f, "{indent}]")?;
            Ok(())
        } else {
            f.debug_struct("ScriptBlock")
                .field("location", &self.location)
                .field("block_type", &self.block_type)
                .field("lines", &self.lines)
                .finish()
        }
    }
}

/// A segment of a script. This is the first stage of parsing, where we split
/// the script.
enum ScriptV0Segment {
    Block(ScriptV0Block),
    SubBlock(ScriptLocation, String, Vec<ScriptV0Segment>),
}

impl std::fmt::Debug for ScriptV0Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            let indent = f.width().unwrap_or_default();
            let indent_str = " ".repeat(indent);
            // HACK: Indent the segments by using width, but don't print indent here
            match self {
                ScriptV0Segment::Block(block) => writeln!(f, "{:#indent$?}", block),
                ScriptV0Segment::SubBlock(location, text, segments) => {
                    writeln!(f, "{indent_str}:{} {text:?} {{", location.line)?;
                    for segment in segments {
                        write!(f, "{segment:#indent$?}", indent = indent + 2)?;
                    }
                    writeln!(f, "{indent_str}}}")?;
                    Ok(())
                }
            }
        } else {
            match self {
                ScriptV0Segment::Block(block) => f
                    .debug_struct("Block")
                    .field("location", &block.location)
                    .field("block_type", &block.block_type)
                    .field("lines", &block.lines)
                    .finish(),
                ScriptV0Segment::SubBlock(location, text, segments) => f
                    .debug_struct("SubBlock")
                    .field("location", &location)
                    .field("text", &text)
                    .field("segments", &segments)
                    .finish(),
            }
        }
    }
}

impl ScriptV0Segment {
    fn is_empty(&self) -> bool {
        match self {
            ScriptV0Segment::Block(block) => block.lines.is_empty(),
            ScriptV0Segment::SubBlock(_, text, segments) => {
                text != "*"
                    && (segments.is_empty() || segments.iter().all(|segment| segment.is_empty()))
            }
        }
    }

    fn split_first(&mut self) -> Option<Self> {
        match self {
            ScriptV0Segment::Block(block) => block.split_first().map(ScriptV0Segment::Block),
            &mut ScriptV0Segment::SubBlock(ref location, ..) => {
                if self.is_command_block() {
                    None
                } else {
                    Some(std::mem::replace(
                        self,
                        ScriptV0Segment::Block(ScriptV0Block {
                            location: location.clone(),
                            block_type: BlockType::Ineffectual,
                            lines: vec![],
                        }),
                    ))
                }
            }
        }
    }

    /// Remove all ineffectual blocks, and merge consecutive blocks that are of the same type.
    fn normalize(segments: Vec<Self>) -> Vec<Self> {
        let mut new_segments = vec![];
        let mut first_command = true;

        let Some(last_line) = segments.last().map(|segment| segment.location().clone()) else {
            return segments;
        };

        for mut segment in segments {
            match segment {
                ScriptV0Segment::Block(ref mut block) => {
                    debug_assert!(
                        !block.lines.is_empty(),
                        "empty blocks should not exist here"
                    );
                    if block.block_type.is_ineffectual() {
                        continue;
                    }
                    if block.block_type.is_command() {
                        // Add a virtual "end" block to the end of the last command block.
                        if first_command {
                            first_command = false;
                        } else {
                            let line = ScriptLine::new(
                                block.location.file.clone(),
                                block.location.line - 1,
                                "end".to_string(),
                            );
                            if let Some(ScriptV0Segment::Block(last_block)) =
                                new_segments.last_mut()
                            {
                                if last_block.block_type.is_pattern() {
                                    last_block.lines.push(line);
                                } else {
                                    new_segments.push(ScriptV0Segment::Block(ScriptV0Block {
                                        location: line.location.clone(),
                                        block_type: BlockType::Pattern,
                                        lines: vec![line],
                                    }));
                                }
                            } else {
                                new_segments.push(ScriptV0Segment::Block(ScriptV0Block {
                                    location: line.location.clone(),
                                    block_type: BlockType::Pattern,
                                    lines: vec![line],
                                }));
                            }
                        }
                    }
                    if let Some(ScriptV0Segment::Block(last_block)) = new_segments.last_mut() {
                        if block.block_type.is_command() {
                            new_segments.push(segment);
                        } else if block.block_type.is_same_type_as(&last_block.block_type) {
                            last_block.lines.extend(std::mem::take(&mut block.lines));
                        } else {
                            new_segments.push(segment);
                        }
                    } else {
                        new_segments.push(segment);
                    }
                }
                ScriptV0Segment::SubBlock(location, text, segments) => {
                    let normalized = ScriptV0Segment::normalize(segments);
                    if !normalized.is_empty() {
                        new_segments.push(ScriptV0Segment::SubBlock(location, text, normalized));
                    }
                }
            }
        }

        // Add a virtual "end" block to the end of the last command block.
        if !first_command {
            let line = ScriptLine::new(last_line.file.clone(), last_line.line, "end".to_string());
            if let Some(ScriptV0Segment::Block(last_block)) = new_segments.last_mut() {
                if last_block.block_type.is_pattern() {
                    last_block.lines.push(line);
                } else {
                    new_segments.push(ScriptV0Segment::Block(ScriptV0Block {
                        location: line.location.clone(),
                        block_type: BlockType::Pattern,
                        lines: vec![line],
                    }));
                }
            } else {
                new_segments.push(ScriptV0Segment::Block(ScriptV0Block {
                    location: line.location.clone(),
                    block_type: BlockType::Pattern,
                    lines: vec![line],
                }));
            }
        }

        // Pass 2: Convert any "any"-type blocks to sub-blocks and steal the next line or non-command subblock.
        let mut i = 0;
        while i < new_segments.len() {
            if let ScriptV0Segment::Block(block) = &mut new_segments[i] {
                if block.block_type.is_any() {
                    let location = block.location.clone();
                    new_segments[i] =
                        ScriptV0Segment::SubBlock(location.clone(), "*".to_string(), vec![]);

                    if i + 1 < new_segments.len() {
                        if let Some(first) = new_segments[i + 1].split_first() {
                            new_segments[i] = ScriptV0Segment::SubBlock(
                                location.clone(),
                                "*".to_string(),
                                vec![first],
                            );
                        }
                    }
                }
            }
            if new_segments[i].is_empty() {
                new_segments.remove(i);
            } else {
                i += 1;
            }
        }

        new_segments
    }

    /// Returns true if this segment is a command block, or the first block it
    /// contains is a command block. Note that this should only be called on
    /// normalized segments.
    fn is_command_block(&self) -> bool {
        match self {
            ScriptV0Segment::Block(block) => block.block_type.is_command(),
            ScriptV0Segment::SubBlock(_, _, segments) => segments
                .first()
                .map_or(false, |segment| segment.is_command_block()),
        }
    }

    fn is_meta_block(&self) -> bool {
        match self {
            ScriptV0Segment::Block(block) => block.block_type.is_meta(),
            _ => false,
        }
    }

    fn location(&self) -> &ScriptLocation {
        match self {
            ScriptV0Segment::Block(block) => &block.location,
            ScriptV0Segment::SubBlock(location, _, _) => location,
        }
    }

    fn last_location(&self) -> &ScriptLocation {
        match self {
            ScriptV0Segment::Block(block) => &block.lines.last().unwrap().location,
            ScriptV0Segment::SubBlock(location, _, segments) => {
                if let Some(last) = segments.last() {
                    last.last_location()
                } else {
                    location
                }
            }
        }
    }
}

pub fn parse_script(file_name: ScriptFile, script: &str) -> Result<Script, ScriptError> {
    let lines = ScriptLine::parse(file_name.clone(), script);
    let segments = segment_script(true, &mut lines.as_slice())?;
    let normalized = ScriptV0Segment::normalize(segments);
    parse_normalized_script_v0(&normalized)
}

/// Split the script into parsing segments. These allow us to more easily parse
/// in later phases because we avoid having to check for block boundaries.
fn segment_script(
    top_level: bool,
    lines_slice: &mut &[ScriptLine],
) -> Result<Vec<ScriptV0Segment>, ScriptError> {
    let mut segments = Vec::new();
    let mut current_segment = None;

    fn is_subblock(text: &str) -> Option<&str> {
        // Workaround for missing let chains
        if text
            .chars()
            .into_iter()
            .next()
            .unwrap_or_default()
            .is_alphabetic()
        {
            text.strip_suffix("{")
        } else {
            None
        }
    }

    let mut lines = lines_slice.into_iter();
    let orig_slice = *lines_slice;
    let mut multiline_terminator = None;
    while let Some(line) = lines.next() {
        if let Some(terminator) = multiline_terminator {
            if line.text() == terminator {
                multiline_terminator = None;
            }
        } else {
            if line.text() == "!!!" {
                multiline_terminator = Some("!!!");
            } else if line.text() == "???" {
                multiline_terminator = Some("???");
            }
        }

        // For commands, we greedily consume all lines until we successfully
        // parse a command (or fail to parse).
        if line.starts_with("$") {
            if let Some(segment) = current_segment.take() {
                segments.push(ScriptV0Segment::Block(segment));
            }
            let mut block_lines = vec![line.clone()];
            let mut command = line.text()[1..].trim().to_string();
            let mut line_count = 1;
            let command = loop {
                match parse_command_line(line.location.clone(), line_count, &command) {
                    Ok(command) => break command,
                    Err(e @ ScriptErrorType::UnclosedQuote)
                    | Err(e @ ScriptErrorType::UnclosedBackslash) => match lines.next() {
                        Some(line) => {
                            block_lines.push(line.clone());
                            command.push('\n');
                            command.push_str(&line.text());
                            line_count += 1;
                        }
                        None => {
                            return Err(ScriptError::new(e, line.location.clone()));
                        }
                    },
                    Err(e) => {
                        return Err(ScriptError::new(e, line.location.clone()));
                    }
                }
            };

            segments.push(ScriptV0Segment::Block(ScriptV0Block {
                block_type: BlockType::Command(command),
                lines: block_lines,
                location: line.location.clone(),
            }));
        } else if let Some(trimmed) = is_subblock(line.text()) {
            if let Some(segment) = current_segment.take() {
                segments.push(ScriptV0Segment::Block(segment));
            }
            let text = trimmed.trim_end().to_owned();
            // Temporaraliy swap from iterator to slice
            let mut rest = lines.as_slice();
            if rest.is_empty() {
                return Err(ScriptError::new(
                    ScriptErrorType::InvalidBlockEnd,
                    line.location.clone(),
                ));
            }
            segments.push(ScriptV0Segment::SubBlock(
                line.location.clone(),
                text,
                segment_script(false, &mut rest)?,
            ));
            lines = rest.into_iter();
        } else if line.text() == "}" {
            // Note that the closing brace is not included in the current
            // segment, we omit these lines from the segment tree.
            if top_level {
                return Err(ScriptError::new(
                    ScriptErrorType::InvalidBlockEnd,
                    line.location.clone(),
                ));
            }
            *lines_slice = lines.as_slice();
            if let Some(segment) = current_segment.take() {
                segments.push(ScriptV0Segment::Block(segment));
            }
            return Ok(segments);
        } else {
            // Split into ineffectual and non-ineffectual lines
            let block_type = if multiline_terminator.is_some() {
                BlockType::Pattern
            } else if line.starts_with("#") || line.is_empty() {
                BlockType::Ineffectual
            } else if line.starts_with("%") {
                BlockType::Meta
            } else if line.starts_with("*") {
                BlockType::Any
            } else {
                BlockType::Pattern
            };

            let segment = current_segment.get_or_insert(ScriptV0Block {
                block_type: block_type.clone(),
                lines: Vec::new(),
                location: line.location.clone(),
            });
            if !segment.block_type.is_same_type_as(&block_type) {
                segments.push(ScriptV0Segment::Block(
                    segment.take(line.location.clone(), block_type),
                ));
            }
            segment.lines.push(line.clone());
        }
    }

    if !top_level {
        return Err(ScriptError::new(
            ScriptErrorType::InvalidBlockEnd,
            orig_slice.last().unwrap().location.clone(),
        ));
    }

    if let Some(segment) = current_segment.take() {
        segments.push(ScriptV0Segment::Block(segment));
    }

    Ok(segments)
}

pub fn parse_command_line(
    location: ScriptLocation,
    line_count: usize,
    command: &str,
) -> Result<CommandLine, ScriptErrorType> {
    let command_str = command.to_string();
    // Process the accumulated command
    const SEPARATORS: &[&str] = &[
        "&&", "||", "1>&2", "2>&1", "1>", "2>", "&", "|", ";", "(", ")", ">", "<", "=",
    ];
    let command = match shellish_parse::multiparse(
        &command,
        shellish_parse::ParseOptions::default(),
        SEPARATORS,
    ) {
        Ok(command) => command,
        Err(shellish_parse::ParseError::DanglingString) => {
            return Err(ScriptErrorType::UnclosedQuote);
        }
        Err(shellish_parse::ParseError::DanglingBackslash) => {
            return Err(ScriptErrorType::UnclosedBackslash);
        }
        _ => {
            return Err(ScriptErrorType::IllegalShellCommand);
        }
    };
    let mut command_bits = vec![];
    for (_, seperator) in command {
        if let Some(seperator) = seperator {
            if SEPARATORS[seperator] == "&" {
                return Err(ScriptErrorType::BackgroundProcessNotAllowed);
            }
            if SEPARATORS[seperator] == ">&" {
                return Err(ScriptErrorType::UnsupportedRedirection);
            }
            command_bits.push(SEPARATORS[seperator].to_string());
        }
    }

    Ok(CommandLine::new(command_str, location, line_count))
}

#[derive(Default)]
struct OutputPatternBuilder {
    ignore: Vec<OutputPattern>,
    reject: Vec<OutputPattern>,
    patterns: Vec<OutputPattern>,
}

impl OutputPatternBuilder {
    fn push(&mut self, location: ScriptLocation, pattern: OutputPatternType) {
        self.patterns.push(OutputPattern {
            pattern,
            ignore: Default::default(),
            reject: Default::default(),
            location,
        });
    }
}

fn parse_normalized_script_v0(segments: &[ScriptV0Segment]) -> Result<Script, ScriptError> {
    // Handle the preamble before the first command block

    let preamble_index = segments
        .iter()
        .position(|segment| segment.is_command_block())
        .unwrap_or(segments.len());
    let (preamble, mut rest) = segments.split_at(preamble_index);

    let mut grok = Grok::with_default_patterns();

    let builder = parse_script_v0_segments(preamble, &mut grok)?;
    if let Some(pattern) = builder.patterns.get(0) {
        return Err(ScriptError::new(
            ScriptErrorType::InvalidGlobalPattern,
            pattern.location.clone(),
        ));
    }
    let global_ignore = builder.ignore;
    let global_reject = builder.reject;

    let mut commands = vec![];

    while let Some((command, remaining)) = rest.split_first() {
        debug_assert!(command.is_command_block());
        let location = command.location().clone();
        let mut command = ScriptCommand {
            command: match command {
                ScriptV0Segment::Block(block) => block.block_type.clone().unwrap_command(),
                _ => unreachable!(),
            },
            pattern: OutputPattern {
                pattern: OutputPatternType::None,
                ignore: Default::default(),
                reject: Default::default(),
                location: location.clone(),
            },
            exit: CommandExit::Success,
            expect_failure: false,
            set_var: None,
        };

        let next_command = remaining
            .iter()
            .position(|segment| segment.is_command_block())
            .unwrap_or(remaining.len());
        let mut pattern;
        (pattern, rest) = remaining.split_at(next_command);

        if let Some(ScriptV0Segment::Block(maybe_meta)) = pattern.first() {
            if maybe_meta.block_type.is_meta() {
                pattern = pattern.split_first().unwrap().1;

                for line in maybe_meta.lines.iter() {
                    if line.starts_with("%SET") {
                        if let Some(var) = line.text()[4..].split_whitespace().next() {
                            command.set_var = Some(var.to_string());
                        } else {
                            return Err(ScriptError::new(
                                ScriptErrorType::InvalidSetVariable,
                                line.location.clone(),
                            ));
                        }
                    } else if line.starts_with("%EXPECT_FAILURE") {
                        command.expect_failure = true;
                    } else if line.starts_with("%EXIT any") {
                        command.exit = CommandExit::Any;
                    } else if line.starts_with("%EXIT ") {
                        if let Some(status) = line.text()[6..].parse::<i32>().ok() {
                            command.exit = CommandExit::Failure(status);
                        } else {
                            return Err(ScriptError::new(
                                ScriptErrorType::InvalidExitStatus,
                                line.location.clone(),
                            ));
                        }
                    }
                }
            }
        }

        let builder = parse_script_v0_segments(pattern, &mut grok)?;
        command.pattern = OutputPattern::new_sequence(location, builder.patterns);
        command.pattern.ignore = global_ignore
            .iter()
            .cloned()
            .chain(builder.ignore.iter().cloned())
            .collect::<Vec<_>>()
            .into();
        command.pattern.reject = global_reject
            .iter()
            .cloned()
            .chain(builder.reject.iter().cloned())
            .collect::<Vec<_>>()
            .into();
        commands.push(command);
    }

    Ok(Script { commands, grok })
}

fn parse_script_v0_segments(
    segments: &[ScriptV0Segment],
    grok: &mut Grok,
) -> Result<OutputPatternBuilder, ScriptError> {
    let mut builder = OutputPatternBuilder::default();
    for segment in segments {
        if segment.is_command_block() {
            return Err(ScriptError::new(
                ScriptErrorType::UnsupportedCommandPosition,
                segment.location().clone(),
            ));
        }
        match segment {
            ScriptV0Segment::Block(block) => {
                let mut pattern = block.lines.as_slice();
                while let Some((line, rest)) = pattern.split_first() {
                    pattern = rest;
                    if line.text() == "!!!" {
                        while let Some((line, rest)) = pattern.split_first() {
                            pattern = rest;
                            if line.text() == "!!!" {
                                break;
                            } else {
                                builder.patterns.push(parse_pattern_line(
                                    grok,
                                    line.location.clone(),
                                    &line.text(),
                                    '!',
                                )?);
                            }
                        }
                    } else if line.text() == "???" {
                        while let Some((line, rest)) = pattern.split_first() {
                            pattern = rest;
                            if line.text() == "???" {
                                break;
                            } else {
                                builder.patterns.push(parse_pattern_line(
                                    grok,
                                    line.location.clone(),
                                    &line.text(),
                                    '?',
                                )?);
                            }
                        }
                    } else if line.starts_with("!") || line.starts_with("?") {
                        builder.patterns.push(parse_pattern_line(
                            grok,
                            line.location.clone(),
                            &line.text()[1..],
                            line.first_char().unwrap(),
                        )?);
                    } else if let Some(pattern) = line.strip_prefix("pattern ") {
                        if let Some((name, pattern)) = pattern.split_once(' ') {
                            grok.add_pattern(name, pattern);
                        } else {
                            return Err(ScriptError::new(
                                ScriptErrorType::InvalidPatternDefinition,
                                line.location.clone(),
                            ));
                        }
                    } else if line.text() == "end" {
                        builder.patterns.push(OutputPattern {
                            pattern: OutputPatternType::End,
                            ignore: Default::default(),
                            reject: Default::default(),
                            location: line.location.clone(),
                        });
                    } else if line.text() == "none" {
                        builder.patterns.push(OutputPattern {
                            pattern: OutputPatternType::None,
                            ignore: Default::default(),
                            reject: Default::default(),
                            location: line.location.clone(),
                        });
                    } else {
                        return Err(ScriptError::new_with_data(
                            ScriptErrorType::InvalidPattern,
                            line.location.clone(),
                            format!("{:?}", line.text()),
                        ));
                    }
                }
            }
            ScriptV0Segment::SubBlock(location, text, segments) => {
                if text == "reject" {
                    let next = parse_script_v0_segments(&segments, grok)?;
                    if !next.ignore.is_empty() || !next.reject.is_empty() {
                        return Err(ScriptError::new(
                            ScriptErrorType::InvalidPattern,
                            location.clone(),
                        ));
                    }
                    builder.reject.extend(next.patterns);
                } else if text == "ignore" {
                    let next = parse_script_v0_segments(&segments, grok)?;
                    if !next.ignore.is_empty() || !next.reject.is_empty() {
                        return Err(ScriptError::new(
                            ScriptErrorType::InvalidPattern,
                            location.clone(),
                        ));
                    }
                    builder.ignore.extend(next.patterns);
                } else {
                    let factory: &dyn Fn(&ScriptLocation, Vec<OutputPattern>) -> OutputPatternType;
                    factory = match text.as_str() {
                        "repeat" => &|location, patterns| {
                            OutputPatternType::Repeat(Box::new(OutputPattern::new_sequence(
                                location.clone(),
                                patterns,
                            )))
                        },
                        "choice" => &|_location, patterns| OutputPatternType::Choice(patterns),
                        "unordered" => {
                            &|_location, patterns| OutputPatternType::Unordered(patterns)
                        }
                        "sequence" => &|_location, patterns| OutputPatternType::Sequence(patterns),
                        "optional" => &|location, patterns| {
                            OutputPatternType::Optional(Box::new(OutputPattern::new_sequence(
                                location.clone(),
                                patterns,
                            )))
                        },
                        "*" => &|location, patterns| {
                            OutputPatternType::Any(Box::new(OutputPattern::new_sequence(
                                location.clone(),
                                patterns,
                            )))
                        },
                        _ => {
                            return Err(ScriptError::new(
                                ScriptErrorType::InvalidBlockType,
                                location.clone(),
                            ));
                        }
                    };

                    let new_builder = parse_script_v0_segments(&segments, grok)?;
                    let pattern = OutputPattern {
                        pattern: factory(location, new_builder.patterns),
                        ignore: Arc::new(new_builder.ignore),
                        reject: Arc::new(new_builder.reject),
                        location: location.clone(),
                    };
                    builder.patterns.push(pattern);
                }
            }
        }
    }
    Ok(builder)
}

fn parse_pattern_line(
    grok: &mut Grok,
    location: ScriptLocation,
    text: &str,
    line_start: char,
) -> Result<OutputPattern, ScriptError> {
    if text.is_empty() {
        return Ok(OutputPattern {
            pattern: OutputPatternType::Literal("".to_string()),
            ignore: Default::default(),
            reject: Default::default(),
            location,
        });
    }

    let text = text.trim();

    if line_start == '!' {
        if !text.contains("%") {
            return Ok(OutputPattern {
                pattern: OutputPatternType::Literal(text.to_string()),
                ignore: Default::default(),
                reject: Default::default(),
                location,
            });
        }

        let pattern = GrokPattern::compile(grok, text, true).map_err(|e| {
            ScriptError::new_with_data(
                ScriptErrorType::InvalidPattern,
                location.clone(),
                e.to_string(),
            )
        })?;
        Ok(OutputPattern {
            pattern: OutputPatternType::Pattern(Arc::new(pattern)),
            ignore: Default::default(),
            reject: Default::default(),
            location,
        })
    } else if line_start == '?' {
        let pattern = GrokPattern::compile(grok, text, false).map_err(|e| {
            ScriptError::new_with_data(
                ScriptErrorType::InvalidPattern,
                location.clone(),
                e.to_string(),
            )
        })?;
        Ok(OutputPattern {
            pattern: OutputPatternType::Pattern(Arc::new(pattern)),
            ignore: Default::default(),
            reject: Default::default(),
            location,
        })
    } else {
        unreachable!("Invalid line start: {line_start}");
    }
}

#[cfg(test)]
mod tests {
    use crate::script::{Lines, OutputMatchContext};

    use super::*;

    fn parse_pattern(pattern: &str) -> Result<OutputPattern, ScriptError> {
        let lines = ScriptLine::parse(ScriptFile::new("test.cli".into()), pattern);
        let segments = segment_script(true, &mut lines.as_slice()).unwrap();
        let normalized = ScriptV0Segment::normalize(segments);
        Ok(
            parse_script_v0_segments(&normalized, &mut Grok::with_default_patterns())?
                .patterns
                .first()
                .unwrap()
                .clone(),
        )
    }

    fn parse_lines(lines: &str) -> Result<Lines, ScriptError> {
        Ok(Lines::new(
            lines.lines().map(|l| l.to_string()).collect::<Vec<_>>(),
        ))
    }

    #[test]
    fn test_v0_patterns() {
        let mut patterns = vec![];
        patterns.push(parse_pattern("! a\n! b\n! c\n").unwrap());
        patterns.push(parse_pattern("!!!\na\nb\nc\n!!!\n").unwrap());

        let context = OutputMatchContext::new();
        let output = parse_lines("a\nb\nc\n").unwrap();

        for pattern in patterns {
            let result = pattern.matches(context.clone(), output.clone());
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_v0_block_pattern() {
        let pattern = r#"
        repeat {
            choice {
    ? pattern1 %{DATA}
    ? pattern2 %{DATA}
    ? pattern3 %{DATA}
            }
        }
        "#;
        let mut grok = Grok::with_default_patterns();
        let file = ScriptFile::new("test.cli".into());
        let pattern = parse_pattern(pattern).unwrap();
        eprintln!("{pattern:?}");
    }
}
