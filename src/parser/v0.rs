use std::sync::Arc;

use grok::Grok;

use crate::{
    command::CommandLine,
    script::{CommandExit, OutputPattern, Script, ScriptCommand, ScriptError, ScriptErrorType},
};

pub fn parse_script(script: &str) -> Result<Script, ScriptError> {
    // First split by lines, and then split by script commands ($)
    let lines = script.lines().collect::<Vec<_>>();
    let mut commands = Vec::new();
    let mut current_command = None;
    let mut output_pattern_lines = Vec::new();
    let mut grok = Grok::with_default_patterns();
    let mut global_ignore = Arc::new(Vec::new());
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
                    let mut pattern = parse_output_pattern(&pattern, &mut grok)?;
                    if !global_ignore.is_empty() {
                        pattern =
                            OutputPattern::Ignore(0, global_ignore.clone(), Box::new(pattern));
                    }
                    commands.push(ScriptCommand {
                        command,
                        pattern,
                        exit: std::mem::take(&mut exit),
                        expect_failure: std::mem::take(&mut expect_failure),
                        set_var: std::mem::take(&mut set_var),
                    });
                } else {
                    let pattern = parse_output_pattern(&pattern, &mut grok)?;
                    if let OutputPattern::Ignore(_, ignored, _) = pattern {
                        eprintln!("ignored lines: {ignored:?}");
                        global_ignore = ignored;
                    } else {
                        // ?
                    }
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
        let mut pattern = parse_output_pattern(&output_pattern_lines, &mut grok)?;
        if !global_ignore.is_empty() {
            pattern = OutputPattern::Ignore(0, global_ignore.clone(), Box::new(pattern));
        }
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

pub fn parse_output_pattern(
    pattern: &[String],
    grok: &mut Grok,
) -> Result<OutputPattern, ScriptError> {
    let (mut patterns, ignore, reject, rest) = parse_maybe_block(false, pattern, grok)?;
    patterns.push(OutputPattern::End);
    let pattern = if patterns.len() == 1 {
        patterns.into_iter().next().unwrap()
    } else {
        OutputPattern::Sequence(0, patterns)
    };

    if ignore.is_empty() {
        Ok(pattern)
    } else {
        Ok(OutputPattern::Ignore(
            0,
            Arc::new(ignore),
            Box::new(pattern),
        ))
    }
}

fn parse_next(
    block: bool,
    pattern: &mut &[String],
    grok: &mut Grok,
    make_pattern: impl Fn(Vec<OutputPattern>) -> OutputPattern,
) -> Result<OutputPattern, ScriptError> {
    let (subpatterns, ignore, reject, rest) = parse_maybe_block(block, pattern, grok)?;
    let new_pattern = make_pattern(subpatterns);
    *pattern = rest;
    if ignore.is_empty() {
        Ok(new_pattern)
    } else {
        Ok(OutputPattern::Ignore(
            0,
            Arc::new(ignore),
            Box::new(new_pattern),
        ))
    }
}

fn parse_maybe_block<'s, 't>(
    block: bool,
    mut pattern: &'s [String],
    grok: &mut Grok,
) -> Result<
    (
        Vec<OutputPattern>,
        Vec<OutputPattern>,
        Vec<OutputPattern>,
        &'s [String],
    ),
    ScriptError,
> {
    let mut patterns = vec![];
    let mut ignore = vec![];
    let mut reject = vec![];
    while let Some((line, rest)) = pattern.split_first() {
        let line = line.trim();
        pattern = rest;
        if line.is_empty() {
            continue;
        } else if line == "*" {
            // Recursively parse the rest of the pattern, stealing the first item for the Any pattern
            let (mut subpatterns, next_ignore, next_reject, rest) =
                parse_maybe_block(block, pattern, grok)?;
            ignore.extend(next_ignore);
            reject.extend(next_reject);
            if subpatterns.is_empty() {
                patterns.push(OutputPattern::Any(0, None));
            } else {
                patterns.push(OutputPattern::Any(0, Some(Box::new(subpatterns.remove(0)))));
                patterns.extend(subpatterns);
            }
            return Ok((patterns, ignore, reject, rest));
        } else if line.starts_with("!!!") {
            while let Some((line, rest)) = pattern.split_first() {
                pattern = rest;
                if line.starts_with("!!!") {
                    break;
                } else {
                    patterns.push(parse_pattern_line(grok, line, '!')?);
                }
            }
        } else if line.starts_with("???") {
            while let Some((line, rest)) = pattern.split_first() {
                pattern = rest;
                if line.starts_with("???") {
                    break;
                } else {
                    patterns.push(parse_pattern_line(grok, line, '?')?);
                }
            }
        } else if line == "!" {
            patterns.push(OutputPattern::Literal(0, "".to_string()));
        } else if line.starts_with("! ") {
            patterns.push(parse_pattern_line(grok, &line[2..], '!')?);
        } else if line.starts_with("? ") {
            patterns.push(parse_pattern_line(grok, &line[2..], '?')?);
        } else if line == "}" {
            return Ok((patterns, ignore, reject, rest));
        } else if line.starts_with("repeat {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPattern::Repeat(0, Box::new(OutputPattern::Sequence(0, subpatterns)))
            })?;
            patterns.push(new_pattern);
        } else if line.starts_with("choice {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPattern::Choice(0, subpatterns)
            })?;
            patterns.push(new_pattern);
        } else if line.starts_with("unordered {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPattern::Unordered(0, subpatterns)
            })?;
            patterns.push(new_pattern);
        } else if line.starts_with("ignore {") {
            let (subpatterns, nest_ignore, nest_reject, rest) =
                parse_maybe_block(true, rest, grok)?;
            if !nest_ignore.is_empty() {
                return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
            }
            ignore.extend(subpatterns);
            pattern = rest;
        } else if line.starts_with("reject {") {
            let (subpatterns, nest_ignore, nest_reject, rest) =
                parse_maybe_block(true, rest, grok)?;

            if !nest_ignore.is_empty() {
                return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
            }
            reject.extend(subpatterns);
            pattern = rest;
        } else if line.starts_with("sequence {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPattern::Sequence(0, subpatterns)
            })?;
            patterns.push(new_pattern);
        } else if line.starts_with("optional {") {
            let new_pattern = parse_next(true, &mut pattern, grok, |subpatterns| {
                OutputPattern::Optional(0, Box::new(OutputPattern::Sequence(0, subpatterns)))
            })?;
            patterns.push(new_pattern);
        } else {
            eprintln!("invalid line: {line}");
            return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
        }
    }
    if block {
        return Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0));
    }
    Ok((patterns, ignore, reject, pattern))
}

fn parse_pattern_line(
    grok: &mut Grok,
    line: &str,
    line_start: char,
) -> Result<OutputPattern, ScriptError> {
    if line_start == '!' {
        if !line.contains("%") {
            return Ok(OutputPattern::Literal(0, line.to_string()));
        }

        // Borrowed from grok crate
        const GROK_PATTERN: &str = r"%\{(?<name>(?<pattern>[A-z0-9]+)(?::(?<alias>[A-z0-9_:;\/\s\.]+))?)(?:=(?<definition>(?:(?:[^{}]+|\.+)+)+))?\}";
        let re = onig::Regex::new(GROK_PATTERN).expect("Failed to compile Grok metapattern");
        let mut prev_start = 0;

        // Escape the text between grok pattern matches to make it easier to write
        // literals-with-grok patterns.
        let mut escaped_string = String::with_capacity(line.len() * 2);
        for re_match in re.find_iter(line) {
            let text = &line[prev_start..re_match.0];
            text.chars().for_each(|c| {
                if c.is_ascii() && !c.is_alphanumeric() {
                    escaped_string.push('\\');
                    escaped_string.push(c);
                } else {
                    escaped_string.push(c);
                }
            });
            escaped_string.push_str(&line[re_match.0..re_match.1]);
            prev_start = re_match.1;
        }
        let text = &line[prev_start..];
        text.chars().for_each(|c| {
            if c.is_ascii() && !c.is_alphanumeric() {
                escaped_string.push('\\');
                escaped_string.push(c);
            } else {
                escaped_string.push(c);
            }
        });
        let eol = format!("{escaped_string}$");
        return Ok(OutputPattern::Pattern(
            0,
            escaped_string.clone(),
            Arc::new(grok.compile(&eol, false).unwrap()),
        ));
    }

    if line_start == '?' {
        let eol = format!("{line}$");
        return Ok(OutputPattern::Pattern(
            0,
            line.to_string(),
            Arc::new(grok.compile(&eol, false).unwrap()),
        ));
    }

    Err(ScriptError::new(ScriptErrorType::InvalidPattern, 0))
}

#[cfg(test)]
mod tests {
    use crate::script::{Lines, OutputMatchContext};

    use super::*;

    fn parse_pattern(pattern: &str) -> Result<OutputPattern, ScriptError> {
        parse_output_pattern(
            &pattern.lines().map(|l| l.to_string()).collect::<Vec<_>>(),
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
