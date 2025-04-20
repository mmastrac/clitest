use std::sync::Arc;

use grok::Grok;

use crate::{
    command::CommandLine,
    script::{
        CommandExit, GrokPattern, OutputPattern, OutputPatternType, Script, ScriptCommand,
        ScriptError, ScriptErrorType, ScriptLine,
    },
};

pub fn parse_script(script: &str) -> Result<Script, ScriptError> {
    // First split by lines, and then split by script commands ($)
    let lines = ScriptLine::parse(script);
    let mut commands = Vec::new();
    let mut current_command: Option<CommandLine> = None;
    let mut output_pattern_lines = Vec::new();
    let mut grok = Grok::with_default_patterns();
    let mut global_ignore = Vec::new();
    let mut global_reject = Vec::new();
    let mut exit = CommandExit::Success;
    let mut expect_failure = false;
    let mut set_var = None;

    let mut lines = lines.into_iter();
    while let Some(line) = lines.next() {
        if !line.text.starts_with("$") && current_command.is_none() {
            if line.text.starts_with("#") {
                if let Some(pattern) = line.text.strip_prefix("#pattern ") {
                    if let Some((name, pattern)) = pattern.split_once(' ') {
                        grok.add_pattern(name, pattern);
                    } else {
                        return Err(ScriptError::new(
                            ScriptErrorType::InvalidPatternDefinition,
                            line.line,
                        ));
                    }
                }
                // This is a comment
                continue;
            } else if line.text.is_empty() {
                continue;
            } else {
                output_pattern_lines.push(line.clone());
            }
        } else {
            if line.text.starts_with("#") {
                if let Some(pattern) = line.text.strip_prefix("#pattern ") {
                    return Err(ScriptError::new(
                        ScriptErrorType::InvalidPatternDefinition,
                        line.line,
                    ));
                }
                // This is a comment
                continue;
            } else if line.text.starts_with("$") {
                // Process the accumulated command. If we receive UnclosedQuote or UnclosedBackslash,
                // feed it another line.
                let mut command = line.text[2..].to_string();
                let command = loop {
                    match parse_command_line(line.line, &command) {
                        Ok(command) => break command,
                        Err(e @ ScriptErrorType::UnclosedQuote)
                        | Err(e @ ScriptErrorType::UnclosedBackslash) => match lines.next() {
                            Some(line) => {
                                command.push('\n');
                                command.push_str(&line.text);
                            }
                            None => {
                                return Err(ScriptError::new(e, line.line));
                            }
                        },
                        Err(e) => {
                            return Err(ScriptError::new(e, line.line));
                        }
                    }
                };

                let pattern = std::mem::take(&mut output_pattern_lines);
                if let Some(command) = current_command.take() {
                    let mut pattern = parse_output_pattern(
                        command.line,
                        &pattern,
                        &global_ignore,
                        &global_reject,
                        &mut grok,
                    )?;
                    commands.push(ScriptCommand {
                        command,
                        pattern,
                        exit: std::mem::take(&mut exit),
                        expect_failure: std::mem::take(&mut expect_failure),
                        set_var: std::mem::take(&mut set_var),
                    });
                } else {
                    let (pattern, _) = parse_maybe_block(false, &pattern, &mut grok)?;
                    global_ignore = pattern.ignore;
                    global_reject = pattern.reject;
                }

                // Reset for next command
                current_command = Some(command);
            } else if line.text.starts_with("%SET") {
                if let Some(var) = line.text[4..].split_whitespace().next() {
                    set_var = Some(var.to_string());
                } else {
                    return Err(ScriptError::new(
                        ScriptErrorType::InvalidSetVariable,
                        line.line,
                    ));
                }
            } else if line.text.starts_with("%EXPECT_FAILURE") {
                expect_failure = true;
            } else if line.text.starts_with("%EXIT any") {
                exit = CommandExit::Any;
            } else if line.text.starts_with("%EXIT ") {
                if let Some(status) = line.text[6..].parse::<i32>().ok() {
                    exit = CommandExit::Failure(status);
                } else {
                    return Err(ScriptError::new(
                        ScriptErrorType::InvalidExitStatus,
                        line.line,
                    ));
                }
            } else {
                output_pattern_lines.push(line.clone());
            }
        }
    }

    // Handle the last command if there are remaining lines
    if let Some(command) = current_command.take() {
        let mut pattern = parse_output_pattern(
            command.line,
            &output_pattern_lines,
            &global_ignore,
            &global_reject,
            &mut grok,
        )?;
        commands.push(ScriptCommand {
            command,
            pattern,
            exit,
            expect_failure,
            set_var,
        });
    } else {
        return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
    }

    Ok(Script { commands, grok })
}

pub fn parse_command_line(line: usize, command: &str) -> Result<CommandLine, ScriptErrorType> {
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

    Ok(CommandLine {
        command: command_str,
        line,
    })
}

#[derive(Default)]
struct OutputPatternBuilder {
    ignore: Vec<OutputPattern>,
    reject: Vec<OutputPattern>,
    patterns: Vec<OutputPattern>,
}

impl OutputPatternBuilder {
    fn push(&mut self, pattern: OutputPatternType) {
        self.patterns.push(OutputPattern {
            pattern,
            ignore: Default::default(),
            reject: Default::default(),
            line: 0,
        });
    }
}

/// Parse the outer part of a pattern.
pub fn parse_output_pattern(
    line: usize,
    pattern: &[ScriptLine],
    ignore: &[OutputPattern],
    reject: &[OutputPattern],
    grok: &mut Grok,
) -> Result<OutputPattern, ScriptError> {
    let (mut builder, rest) = parse_maybe_block(false, pattern, grok)?;
    builder.push(OutputPatternType::End);

    builder.ignore.extend(ignore.iter().cloned());
    builder.reject.extend(reject.iter().cloned());

    Ok(OutputPattern {
        ignore: Arc::new(builder.ignore),
        reject: Arc::new(builder.reject),
        pattern: OutputPatternType::Sequence(builder.patterns),
        line,
    })
}

fn parse_next(
    block: bool,
    line: usize,
    pattern: &mut &[ScriptLine],
    grok: &mut Grok,
    make_pattern: impl Fn(Vec<OutputPattern>) -> OutputPatternType,
) -> Result<OutputPattern, ScriptError> {
    let (builder, rest) = parse_maybe_block(block, pattern, grok)?;
    let new_pattern = make_pattern(builder.patterns);
    *pattern = rest;
    Ok(OutputPattern {
        line,
        ignore: Arc::new(builder.ignore),
        reject: Arc::new(builder.reject),
        pattern: new_pattern,
    })
}

/// Parse the inner part of a block, collecting ignore/reject blocks and other
/// patterns.
fn parse_maybe_block<'s, 't>(
    block: bool,
    mut pattern: &'s [ScriptLine],
    grok: &mut Grok,
) -> Result<(OutputPatternBuilder, &'s [ScriptLine]), ScriptError> {
    let mut builder = OutputPatternBuilder::default();
    while let Some((line, rest)) = pattern.split_first() {
        let text = line.text.trim();
        pattern = rest;
        if text.is_empty() {
            continue;
        } else if text == "*" {
            // Recursively parse the rest of the pattern, stealing the first item for the Any pattern
            let (mut next, rest) = parse_maybe_block(block, pattern, grok)?;
            builder.ignore.extend(next.ignore);
            builder.reject.extend(next.reject);
            if next.patterns.is_empty() {
                if block {
                    return Err(ScriptError::new(ScriptErrorType::InvalidAnyPattern, 0));
                }
                builder.push(OutputPatternType::Any(None));
            } else {
                let pattern = next.patterns.remove(0);
                match pattern.pattern {
                    OutputPatternType::Choice(..)
                    | OutputPatternType::Unordered(..)
                    | OutputPatternType::Literal(..)
                    | OutputPatternType::Pattern(..) => {
                        builder.push(OutputPatternType::Any(Some(Box::new(pattern))));
                    }
                    _ => return Err(ScriptError::new(ScriptErrorType::InvalidAnyPattern, 0)),
                }
                builder.patterns.extend(next.patterns);
            }
            return Ok((builder, rest));
        } else if text.starts_with("!!!") {
            while let Some((line, rest)) = pattern.split_first() {
                pattern = rest;
                if line.text.starts_with("!!!") {
                    break;
                } else {
                    builder
                        .patterns
                        .push(parse_pattern_line(grok, line.line, &line.text, '!')?);
                }
            }
        } else if text.starts_with("???") {
            while let Some((line, rest)) = pattern.split_first() {
                pattern = rest;
                if line.text.starts_with("???") {
                    break;
                } else {
                    builder
                        .patterns
                        .push(parse_pattern_line(grok, line.line, &line.text, '?')?);
                }
            }
        } else if text == "!" || text == "?" {
            builder
                .patterns
                .push(parse_pattern_line(grok, line.line, "", '!')?);
        } else if text.starts_with("! ") {
            builder
                .patterns
                .push(parse_pattern_line(grok, line.line, &text[2..], '!')?);
        } else if text.starts_with("? ") {
            builder
                .patterns
                .push(parse_pattern_line(grok, line.line, &text[2..], '?')?);
        } else if text == "}" {
            return Ok((builder, rest));
        } else if text.starts_with("repeat {") {
            let new_pattern = parse_next(true, line.line, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Repeat(Box::new(OutputPattern {
                    pattern: OutputPatternType::Sequence(subpatterns),
                    ignore: Default::default(),
                    reject: Default::default(),
                    line: line.line,
                }))
            })?;
            builder.patterns.push(new_pattern);
        } else if text.starts_with("choice {") {
            let new_pattern = parse_next(true, line.line, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Choice(subpatterns)
            })?;
            builder.patterns.push(new_pattern);
        } else if text.starts_with("unordered {") {
            let new_pattern = parse_next(true, line.line, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Unordered(subpatterns)
            })?;
            builder.patterns.push(new_pattern);
        } else if text.starts_with("ignore {") {
            let (next, rest) = parse_maybe_block(true, rest, grok)?;
            if !next.ignore.is_empty() || !next.reject.is_empty() {
                return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
            }
            builder.ignore.extend(next.patterns);
            pattern = rest;
        } else if text.starts_with("reject {") {
            let (next, rest) = parse_maybe_block(true, rest, grok)?;
            if !next.ignore.is_empty() || !next.reject.is_empty() {
                return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
            }
            builder.reject.extend(next.patterns);
            pattern = rest;
        } else if text.starts_with("sequence {") {
            let new_pattern = parse_next(true, line.line, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Sequence(subpatterns)
            })?;
            builder.patterns.push(new_pattern);
        } else if text.starts_with("optional {") {
            let new_pattern = parse_next(true, line.line, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Optional(Box::new(OutputPattern {
                    pattern: OutputPatternType::Sequence(subpatterns),
                    ignore: Default::default(),
                    reject: Default::default(),
                    line: line.line,
                }))
            })?;
            builder.patterns.push(new_pattern);
        } else {
            eprintln!("invalid line: {text}");
            return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
        }
    }
    if block {
        return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
    }
    Ok((builder, pattern))
}

fn parse_pattern_line(
    grok: &mut Grok,
    line: usize,
    text: &str,
    line_start: char,
) -> Result<OutputPattern, ScriptError> {
    if text.is_empty() {
        return Ok(OutputPattern {
            pattern: OutputPatternType::Literal("".to_string()),
            ignore: Default::default(),
            reject: Default::default(),
            line,
        });
    }

    if line_start == '!' {
        if !text.contains("%") {
            return Ok(OutputPattern {
                pattern: OutputPatternType::Literal(text.to_string()),
                ignore: Default::default(),
                reject: Default::default(),
                line,
            });
        }

        let pattern = GrokPattern::compile(grok, text, true)
            .map_err(|e| ScriptError::new(ScriptErrorType::InvalidPattern, line))?;
        Ok(OutputPattern {
            pattern: OutputPatternType::Pattern(Arc::new(pattern)),
            ignore: Default::default(),
            reject: Default::default(),
            line,
        })
    } else if line_start == '?' {
        let pattern = GrokPattern::compile(grok, text, false)
            .map_err(|e| ScriptError::new(ScriptErrorType::InvalidPattern, line))?;
        Ok(OutputPattern {
            pattern: OutputPatternType::Pattern(Arc::new(pattern)),
            ignore: Default::default(),
            reject: Default::default(),
            line,
        })
    } else {
        Err(ScriptError::new(ScriptErrorType::InvalidPattern, line))
    }
}

#[cfg(test)]
mod tests {
    use crate::script::{Lines, OutputMatchContext};

    use super::*;

    fn parse_pattern(pattern: &str) -> Result<OutputPattern, ScriptError> {
        let lines = ScriptLine::parse(pattern);
        parse_output_pattern(0, &lines, &[], &[], &mut Grok::with_default_patterns())
    }

    fn parse_lines(lines: &str) -> Result<Lines, ScriptError> {
        Ok(Lines::new(
            lines.lines().map(|l| l.to_string()).collect::<Vec<_>>(),
        ))
    }

    #[test]
    fn test_v0_patterns() {
        let pattern = parse_pattern(
            r#"
! a
! b
! c
        "#,
        )
        .unwrap();

        let context = OutputMatchContext::new();
        let output = parse_lines(
            r#"
a
b
c
        "#
            .trim(),
        )
        .unwrap();
        let result = pattern.matches(context, output);
        assert!(result.is_ok());
    }
}
