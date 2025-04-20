use std::sync::Arc;

use grok::Grok;

use crate::{
    command::CommandLine,
    script::{
        CommandExit, GrokPattern, OutputPattern, OutputPatternType, Script, ScriptCommand,
        ScriptError, ScriptErrorType,
    },
};

pub fn parse_script(script: &str) -> Result<Script, ScriptError> {
    // First split by lines, and then split by script commands ($)
    let lines = script.lines().collect::<Vec<_>>();
    let mut commands = Vec::new();
    let mut current_command = None;
    let mut output_pattern_lines = Vec::new();
    let mut grok = Grok::with_default_patterns();
    let mut global_ignore = Vec::new();
    let mut global_reject = Vec::new();
    let mut exit = CommandExit::Success;
    let mut expect_failure = false;
    let mut set_var = None;

    let mut iter = lines.into_iter().enumerate();
    while let Some((mut line_number, mut line)) = iter.next() {
        if !line.starts_with("$") && current_command.is_none() {
            if line.starts_with("#") {
                if let Some(pattern) = line.strip_prefix("#pattern ") {
                    if let Some((name, pattern)) = pattern.split_once(' ') {
                        grok.add_pattern(name, pattern);
                    } else {
                        return Err(ScriptError::new(
                            ScriptErrorType::InvalidPatternDefinition,
                            line_number,
                        ));
                    }
                }
                // This is a comment
                continue;
            } else if line.is_empty() {
                continue;
            } else {
                output_pattern_lines.push(line.to_string());
            }
        } else {
            if line.starts_with("#") {
                if let Some(pattern) = line.strip_prefix("#pattern ") {
                    return Err(ScriptError::new(
                        ScriptErrorType::InvalidPatternDefinition,
                        line_number,
                    ));
                }
                // This is a comment
                continue;
            } else if line.starts_with("$") {
                // Process the accumulated command. If we receive UnclosedQuote or UnclosedBackslash,
                // feed it another line.
                let mut command = line[2..].to_string();
                let command = loop {
                    match parse_command_line(&command) {
                        Ok(command) => break command,
                        Err(e @ ScriptErrorType::UnclosedQuote)
                        | Err(e @ ScriptErrorType::UnclosedBackslash) => match iter.next() {
                            Some((next_line_number, next_line)) => {
                                line_number = next_line_number;
                                line = next_line;
                                command.push('\n');
                                command.push_str(&line);
                            }
                            None => {
                                return Err(ScriptError::new(e, line_number));
                            }
                        },
                        Err(e) => {
                            return Err(ScriptError::new(e, line_number));
                        }
                    }
                };

                let pattern = std::mem::take(&mut output_pattern_lines);
                if let Some(command) = current_command.take() {
                    let mut pattern =
                        parse_output_pattern(&pattern, &global_ignore, &global_reject, &mut grok)?;
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
            } else if line.starts_with("%SET") {
                if let Some(var) = line[4..].split_whitespace().next() {
                    set_var = Some(var.to_string());
                } else {
                    return Err(ScriptError::new(
                        ScriptErrorType::InvalidSetVariable,
                        line_number,
                    ));
                }
            } else if line.starts_with("%EXPECT_FAILURE") {
                expect_failure = true;
            } else if line.starts_with("%EXIT any") {
                exit = CommandExit::Any;
            } else if line.starts_with("%EXIT ") {
                if let Some(status) = line[6..].parse::<i32>().ok() {
                    exit = CommandExit::Failure(status);
                } else {
                    return Err(ScriptError::new(
                        ScriptErrorType::InvalidExitStatus,
                        line_number,
                    ));
                }
            } else {
                output_pattern_lines.push(line.to_string());
            }
        }
    }

    // Handle the last command if there are remaining lines
    if let Some(command) = current_command.take() {
        let mut pattern = parse_output_pattern(
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

pub fn parse_command_line(command: &str) -> Result<CommandLine, ScriptErrorType> {
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
    pattern: &[String],
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
        line: 0,
    })
}

fn parse_next(
    block: bool,
    pattern: &mut &[String],
    grok: &mut Grok,
    make_pattern: impl Fn(Vec<OutputPattern>) -> OutputPatternType,
) -> Result<OutputPattern, ScriptError> {
    let (builder, rest) = parse_maybe_block(block, pattern, grok)?;
    let new_pattern = make_pattern(builder.patterns);
    *pattern = rest;
    Ok(OutputPattern {
        line: 0,
        ignore: Arc::new(builder.ignore),
        reject: Arc::new(builder.reject),
        pattern: new_pattern,
    })
}

/// Parse the inner part of a block, collecting ignore/reject blocks and other
/// patterns.
fn parse_maybe_block<'s, 't>(
    block: bool,
    mut pattern: &'s [String],
    grok: &mut Grok,
) -> Result<(OutputPatternBuilder, &'s [String]), ScriptError> {
    let mut builder = OutputPatternBuilder::default();
    while let Some((line, rest)) = pattern.split_first() {
        let line = line.trim();
        pattern = rest;
        if line.is_empty() {
            continue;
        } else if line == "*" {
            // Recursively parse the rest of the pattern, stealing the first item for the Any pattern
            let (mut next, rest) = parse_maybe_block(block, pattern, grok)?;
            builder.ignore.extend(next.ignore);
            builder.reject.extend(next.reject);
            if next.patterns.is_empty() {
                builder.push(OutputPatternType::Any(None));
            } else {
                builder.push(OutputPatternType::Any(Some(Box::new(
                    next.patterns.remove(0),
                ))));
                builder.patterns.extend(next.patterns);
            }
            return Ok((builder, rest));
        } else if line.starts_with("!!!") {
            while let Some((line, rest)) = pattern.split_first() {
                pattern = rest;
                if line.starts_with("!!!") {
                    break;
                } else {
                    builder.push(parse_pattern_line(grok, line, '!')?);
                }
            }
        } else if line.starts_with("???") {
            while let Some((line, rest)) = pattern.split_first() {
                pattern = rest;
                if line.starts_with("???") {
                    break;
                } else {
                    builder.push(parse_pattern_line(grok, line, '?')?);
                }
            }
        } else if line == "!" {
            builder.push(OutputPatternType::Literal("".to_string()));
        } else if line.starts_with("! ") {
            builder.push(parse_pattern_line(grok, &line[2..], '!')?);
        } else if line.starts_with("? ") {
            builder.push(parse_pattern_line(grok, &line[2..], '?')?);
        } else if line == "}" {
            return Ok((builder, rest));
        } else if line.starts_with("repeat {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Repeat(Box::new(OutputPattern {
                    pattern: OutputPatternType::Sequence(subpatterns),
                    ignore: Default::default(),
                    reject: Default::default(),
                    line: 0,
                }))
            })?;
            builder.patterns.push(new_pattern);
        } else if line.starts_with("choice {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Choice(subpatterns)
            })?;
            builder.patterns.push(new_pattern);
        } else if line.starts_with("unordered {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Unordered(subpatterns)
            })?;
            builder.patterns.push(new_pattern);
        } else if line.starts_with("ignore {") {
            let (next, rest) = parse_maybe_block(true, rest, grok)?;
            if !next.ignore.is_empty() || !next.reject.is_empty() {
                return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
            }
            builder.ignore.extend(next.patterns);
            pattern = rest;
        } else if line.starts_with("reject {") {
            let (next, rest) = parse_maybe_block(true, rest, grok)?;

            if !next.ignore.is_empty() || !next.reject.is_empty() {
                return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
            }
            builder.reject.extend(next.patterns);
            pattern = rest;
        } else if line.starts_with("sequence {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Sequence(subpatterns)
            })?;
            builder.patterns.push(new_pattern);
        } else if line.starts_with("optional {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPatternType::Optional(Box::new(OutputPattern {
                    pattern: OutputPatternType::Sequence(subpatterns),
                    ignore: Default::default(),
                    reject: Default::default(),
                    line: 0,
                }))
            })?;
            builder.patterns.push(new_pattern);
        } else {
            eprintln!("invalid line: {line}");
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
    line: &str,
    line_start: char,
) -> Result<OutputPatternType, ScriptError> {
    if line_start == '!' {
        if !line.contains("%") {
            return Ok(OutputPatternType::Literal(line.to_string()));
        }

        let pattern = GrokPattern::compile(grok, line, true)
            .map_err(|e| ScriptError::new(ScriptErrorType::InvalidPattern, 0))?;
        Ok(OutputPatternType::Pattern(Arc::new(pattern)))
    } else if line_start == '?' {
        let pattern = GrokPattern::compile(grok, line, false)
            .map_err(|e| ScriptError::new(ScriptErrorType::InvalidPattern, 0))?;
        Ok(OutputPatternType::Pattern(Arc::new(pattern)))
    } else {
        Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0))
    }
}

#[cfg(test)]
mod tests {
    use crate::script::{Lines, OutputMatchContext};

    use super::*;

    fn parse_pattern(pattern: &str) -> Result<OutputPattern, ScriptError> {
        parse_output_pattern(
            &pattern.lines().map(|l| l.to_string()).collect::<Vec<_>>(),
            &[],
            &[],
            &mut Grok::with_default_patterns(),
        )
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
