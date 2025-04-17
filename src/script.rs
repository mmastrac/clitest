use std::{
    collections::HashSet,
    process::ExitStatus,
    sync::{Arc, Mutex},
};

use grok::Grok;

use crate::command::CommandLine;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Line {
    pub number: usize,
    pub text: String,
}

#[derive(Clone)]
pub struct Lines {
    lines: Arc<Vec<String>>,
    current_line: usize,
    ignored_patterns: Vec<Arc<Vec<OutputPattern>>>,
    negative_disabled: bool,
    rejected_patterns: Vec<Arc<Vec<OutputPattern>>>,
}

impl std::fmt::Display for Lines {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lines[self.current_line..].join("\n"))
    }
}

impl Lines {
    pub fn new(lines: Vec<String>) -> Self {
        Self {
            lines: Arc::new(lines),
            current_line: 0,
            ignored_patterns: Default::default(),
            negative_disabled: false,
            rejected_patterns: Default::default(),
        }
    }

    pub fn is_exhausted(&self) -> bool {
        self.current_line >= self.lines.len()
    }

    pub fn next(
        &self,
        context: OutputMatchContext,
    ) -> Result<(Option<Line>, Lines), OutputPatternMatchFailure> {
        let mut next = self.clone();
        'outer: while next.current_line < next.lines.len() {
            if !self.negative_disabled {
                let ignore_check = next.without_negatives();
                for ignored in &next.ignored_patterns {
                    for ignored_pattern in ignored.iter() {
                        if let Ok(next_next) =
                            ignored_pattern.matches(context.ignore(), ignore_check.clone())
                        {
                            next = next_next.with_negatives();
                            continue 'outer;
                        }
                    }
                }
                for rejected in &next.rejected_patterns {
                    for rejected_pattern in rejected.iter() {
                        if let Ok(_) =
                            rejected_pattern.matches(context.ignore(), ignore_check.clone())
                        {
                            return Err(OutputPatternMatchFailure {
                                script_line: 0,
                                pattern_type: "reject",
                                output_line: None,
                            });
                        }
                    }
                }
            }
            let line = Line {
                number: next.current_line,
                text: next.lines[next.current_line].clone(),
            };
            next.current_line += 1;
            return Ok((Some(line), next));
        }
        Ok((None, next))
    }

    pub fn with_ignore(&self, ignore: &Arc<Vec<OutputPattern>>) -> Self {
        let mut ignored_patterns = self.ignored_patterns.clone();
        ignored_patterns.push(ignore.clone());
        Self {
            ignored_patterns,
            ..self.clone()
        }
    }

    pub fn with_reject(&self, reject: &Arc<Vec<OutputPattern>>) -> Self {
        let mut rejected_patterns = self.rejected_patterns.clone();
        rejected_patterns.push(reject.clone());
        Self {
            rejected_patterns,
            ..self.clone()
        }
    }

    fn without_negatives(&self) -> Self {
        Self {
            negative_disabled: true,
            ..self.clone()
        }
    }

    fn with_negatives(&self) -> Self {
        Self {
            negative_disabled: false,
            ..self.clone()
        }
    }

    pub fn into_inner(self) -> Vec<String> {
        Arc::unwrap_or_clone(self.lines).split_off(self.current_line)
    }

    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }
}

#[derive(derive_more::Debug)]
pub struct Script {
    pub commands: Vec<ScriptCommand>,
    #[debug(skip)]
    pub grok: Grok,
}

pub struct ScriptRunArgs {
    pub delay_steps: Option<u64>,
    pub ignore_exit_codes: bool,
    pub ignore_matches: bool,
    pub quiet: bool,
}

#[derive(Debug, thiserror::Error, derive_more::Display)]
#[display("{error} at line {line}")]
pub struct ScriptError {
    pub error: ScriptErrorType,
    pub line: usize,
}

impl ScriptError {
    pub fn new(error: ScriptErrorType, line: usize) -> Self {
        Self { error, line }
    }
}

#[derive(Debug, thiserror::Error, Eq, PartialEq)]
pub enum ScriptErrorType {
    #[error("background process not allowed")]
    BackgroundProcessNotAllowed,
    #[error("unclosed quote")]
    UnclosedQuote,
    #[error("unclosed backslash")]
    UnclosedBackslash,
    #[error("illegal shell command format")]
    IllegalShellCommand,
    #[error("unsupported redirection")]
    UnsupportedRedirection,
    #[error("invalid pattern definition")]
    InvalidPatternDefinition,
    #[error("invalid pattern")]
    InvalidPattern,
    #[error("invalid exit status")]
    InvalidExitStatus,
    #[error("invalid set variable")]
    InvalidSetVariable,
    #[error("invalid version")]
    InvalidVersion,
}

#[derive(Clone)]
pub enum OutputPattern {
    /// The end of the output
    End,
    /// Any lines, followed by a pattern.
    Any(usize, Option<Box<OutputPattern>>),
    /// A literal string
    Literal(usize, String),
    /// A grok pattern
    Pattern(usize, String, Arc<grok::Pattern>),
    /// A pattern that matches one or more of the given pattern
    Repeat(usize, Box<OutputPattern>),
    /// A pattern that matches zero or one of the given pattern
    Optional(usize, Box<OutputPattern>),
    /// A pattern that all of its subpatterns, but in any order
    Unordered(usize, Vec<OutputPattern>),
    /// A pattern that matches one of the given patterns
    Choice(usize, Vec<OutputPattern>),
    /// A pattern that matches a sequence of patterns
    Sequence(usize, Vec<OutputPattern>),
    /// Ignore any lines matching the given patterns when parsing the other pattern.
    Ignore(usize, Arc<Vec<OutputPattern>>, Box<OutputPattern>),
    /// Reject any lines matching the given patterns when parsing the other pattern.
    Reject(usize, Arc<Vec<OutputPattern>>, Box<OutputPattern>),
}

impl Default for OutputPattern {
    fn default() -> Self {
        Self::Sequence(0, vec![])
    }
}

impl std::fmt::Debug for OutputPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputPattern::Literal(_, literal) => write!(f, "Literal({literal})"),
            OutputPattern::Pattern(_, pattern, _) => write!(f, "Pattern({pattern:?})"),
            OutputPattern::Repeat(_, pattern) => write!(f, "Repeat({pattern:?})"),
            OutputPattern::Optional(_, pattern) => write!(f, "Optional({pattern:?})"),
            OutputPattern::Unordered(_, patterns) => write!(f, "Unordered({patterns:?})"),
            OutputPattern::Choice(_, patterns) => write!(f, "Choice({patterns:?})"),
            OutputPattern::Sequence(_, patterns) => write!(f, "Sequence({patterns:?})"),
            OutputPattern::Ignore(_, patterns, pattern) => {
                write!(f, "Ignore({patterns:?}, {pattern:?})")
            }
            OutputPattern::Reject(_, patterns, pattern) => {
                write!(f, "Reject({patterns:?}, {pattern:?})")
            }
            OutputPattern::Any(_, until) => write!(f, "Any({until:?})"),
            OutputPattern::End => write!(f, "End"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ScriptRunError {
    #[error("{0}")]
    Pattern(#[from] OutputPatternMatchFailure),
    #[error("{0}")]
    Exit(ExitStatus),
    #[error("expected failure, but passed")]
    ExpectedFailure,
    #[error("{0}")]
    IO(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error, derive_more::Display, PartialEq, Eq)]
#[display("pattern {pattern_type} at line {script_line} does not match output line {:?}", output_line.as_ref().map(|l| l.text.clone()).unwrap_or("<eof>".to_string()))]
pub struct OutputPatternMatchFailure {
    pub script_line: usize,
    pub pattern_type: &'static str,
    pub output_line: Option<Line>,
}

#[derive(Debug, Clone)]
pub struct OutputMatchContext {
    depth: usize,
    trace: Arc<Mutex<Vec<String>>>,
    ignore: bool,
}

impl OutputMatchContext {
    pub fn new() -> Self {
        Self {
            depth: 0,
            trace: Default::default(),
            ignore: false,
        }
    }

    pub fn descend(&self) -> Self {
        Self {
            depth: self.depth + 1,
            trace: self.trace.clone(),
            ignore: self.ignore,
        }
    }

    pub fn ignore(&self) -> Self {
        Self {
            depth: self.depth,
            trace: self.trace.clone(),
            ignore: true,
        }
    }

    pub fn trace(&self, line: &str) {
        let ignore = if self.ignore { "-" } else { "" };
        self.trace.lock().unwrap().push(format!(
            "{:indent$}{ignore}{}",
            "",
            line,
            indent = self.depth * 2
        ));
    }

    pub fn traces(&self) -> Vec<String> {
        std::mem::take(&mut self.trace.lock().unwrap())
    }
}

impl OutputPattern {
    pub fn matches<'s>(
        &self,
        context: OutputMatchContext,
        mut output: Lines,
    ) -> Result<Lines, OutputPatternMatchFailure> {
        context.trace(&format!("matching {:?}", self));
        match self {
            OutputPattern::Literal(script_line, literal) => {
                let (line, next) = output.next(context.clone())?;
                if let Some(line) = line {
                    if &line.text == literal {
                        context.trace(&format!("literal match: {:?} == {literal:?}", line.text));
                        Ok(next)
                    } else {
                        context.trace(&format!(
                            "literal FAILED match: {:?} == {literal:?}",
                            line.text
                        ));
                        Err(OutputPatternMatchFailure {
                            script_line: *script_line,
                            pattern_type: "literal",
                            output_line: Some(line),
                        })
                    }
                } else {
                    Err(OutputPatternMatchFailure {
                        script_line: *script_line,
                        pattern_type: "literal",
                        output_line: None,
                    })
                }
            }
            OutputPattern::Pattern(script_line, pattern_string, pattern) => {
                let (line, next) = output.next(context.clone())?;
                if let Some(line) = line {
                    if let Some(matches) = pattern.match_against(&line.text) {
                        context.trace(&format!(
                            "pattern match: {:?} =~ {pattern_string:?}",
                            line.text
                        ));
                        Ok(next)
                    } else {
                        context.trace(&format!(
                            "pattern FAILED match: {:?} =~ {pattern_string:?}",
                            line.text
                        ));
                        Err(OutputPatternMatchFailure {
                            script_line: *script_line,
                            pattern_type: "pattern",
                            output_line: Some(line),
                        })
                    }
                } else {
                    Err(OutputPatternMatchFailure {
                        script_line: *script_line,
                        pattern_type: "pattern",
                        output_line: None,
                    })
                }
            }
            OutputPattern::Sequence(script_line, patterns) => {
                for pattern in patterns {
                    match pattern.matches(context.descend(), output) {
                        Ok(v) => {
                            output = v;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(output)
            }
            OutputPattern::Repeat(script_line, pattern) => {
                // Mandatory first match
                let mut output = pattern.matches(context.descend(), output)?;
                // Any number of additional matches, greedy
                loop {
                    match pattern.matches(context.descend(), output.clone()) {
                        Ok(new_rest) => {
                            output = new_rest;
                        }
                        Err(_) => break Ok(output),
                    }
                }
            }
            OutputPattern::Optional(script_line, pattern) => {
                // Never fails
                match pattern.matches(context.descend(), output.clone()) {
                    Ok(v) => Ok(v),
                    Err(_) => Ok(output),
                }
            }
            OutputPattern::Unordered(script_line, patterns) => {
                // Found is initialized with 0..patterns.len()
                let mut not_found = (0..patterns.len()).collect::<HashSet<_>>();
                'outer: while !not_found.is_empty() {
                    for pattern in &not_found {
                        let pattern = *pattern;
                        match patterns[pattern].matches(context.descend(), output.clone()) {
                            Ok(v) => {
                                not_found.remove(&pattern);
                                output = v;
                                continue 'outer;
                            }
                            Err(_) => {
                                continue;
                            }
                        }
                    }
                    return Err(OutputPatternMatchFailure {
                        script_line: *script_line,
                        pattern_type: "unordered",
                        output_line: None,
                    });
                }
                Ok(output)
            }
            OutputPattern::Choice(script_line, patterns) => {
                for pattern in patterns {
                    if let Ok(v) = pattern.matches(context.descend(), output.clone()) {
                        return Ok(v);
                    }
                }
                Err(OutputPatternMatchFailure {
                    script_line: *script_line,
                    pattern_type: "choice",
                    output_line: None,
                })
            }
            OutputPattern::Ignore(script_line, ignore, pattern) => {
                pattern.matches(context.descend(), output.with_ignore(ignore))
            }
            OutputPattern::Reject(script_line, reject, pattern) => {
                pattern.matches(context.descend(), output.with_reject(reject))
            }
            OutputPattern::Any(script_line, until) => {
                if let Some(until) = until {
                    loop {
                        match until.matches(context.descend(), output.clone()) {
                            Ok(v) => {
                                output = v;
                                break Ok(output);
                            }
                            Err(e) => {
                                // Eat one line and try again
                                let (line, next) = output.next(context.clone())?;
                                if line.is_some() {
                                    output = next;
                                    continue;
                                } else {
                                    break Err(e);
                                }
                            }
                        }
                    }
                } else {
                    loop {
                        let (line, next) = output.next(context.clone())?;
                        if line.is_some() {
                            output = next;
                        } else {
                            break Ok(output);
                        }
                    }
                }
            }
            OutputPattern::End => {
                let (line, next) = output.next(context)?;
                if let Some(line) = line {
                    Err(OutputPatternMatchFailure {
                        script_line: 0,
                        pattern_type: "end",
                        output_line: Some(line),
                    })
                } else {
                    Ok(next)
                }
            }
        }
    }
}

impl Script {
    pub fn run(&self, args: ScriptRunArgs) -> Result<(), ScriptRunError> {
        use crate::{cprintln, term::Color};
        use std::collections::HashMap;

        let mut envs = HashMap::new();

        for command in &self.commands {
            if !args.quiet {
                cprintln!(fg = Color::Green, "{}", command.command.command);
                cprintln!(
                    dimmed = true,
                    "{:->count$}",
                    "",
                    count = termsize::get().map(|s| (s.cols - 1) as usize).unwrap_or(80)
                );
            }
            let (output, status) = command.command.run(args.quiet, &envs)?;
            if !command.exit.matches(status) {
                println!("❌ FAIL: {status}");
                if !args.ignore_exit_codes {
                    return Err(ScriptRunError::Exit(status));
                }
            }
            if output.is_empty() && !args.quiet {
                cprintln!(dimmed = true, "(no output)");
            }
            if let Some(set_var) = &command.set_var {
                envs.insert(set_var.clone(), output.to_string().trim().to_string());
            }
            if !args.quiet {
                cprintln!(
                    dimmed = true,
                    "{:->count$}",
                    "",
                    count = termsize::get().map(|s| (s.cols - 1) as usize).unwrap_or(80)
                );
            }
            let context = OutputMatchContext::new();
            match command.pattern.matches(context.clone(), output) {
                Ok(_) => {
                    if command.expect_failure {
                        if !args.quiet {
                            println!("❌ Expected failure, but passed");
                        }
                        if !args.ignore_matches {
                            return Err(ScriptRunError::ExpectedFailure);
                        }
                    } else {
                        if !args.quiet {
                            println!("✅ OK");
                        }
                    }
                }
                Err(e) => {
                    if !command.expect_failure {
                        println!("ERROR: {e}");
                        for line in context.traces() {
                            println!("{line}");
                        }
                    }
                    if command.expect_failure {
                        if !args.quiet {
                            println!("✅ OK (expected failure)");
                        }
                    } else {
                        if !args.quiet {
                            println!("❌ FAIL");
                        }
                        if !args.ignore_matches {
                            return Err(ScriptRunError::Pattern(e));
                        }
                    }
                }
            }
            if !args.quiet {
                println!();
            }
            if let Some(delay) = args.delay_steps {
                std::thread::sleep(std::time::Duration::from_millis(delay));
            }
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
pub enum CommandExit {
    #[default]
    Success,
    Failure(i32),
    Any,
}
impl CommandExit {
    pub fn matches(&self, status: ExitStatus) -> bool {
        match self {
            CommandExit::Success => status.success(),
            CommandExit::Failure(code) => *code == status.code().unwrap_or(-1),
            CommandExit::Any => true,
        }
    }
}

#[derive(Debug)]
pub struct ScriptCommand {
    pub command: CommandLine,
    pub pattern: OutputPattern,
    pub exit: CommandExit,
    pub expect_failure: bool,
    pub set_var: Option<String>,
}

#[cfg(test)]
mod tests {
    use crate::parser::v0::{parse_output_pattern, parse_script};

    use super::*;
    use std::error::Error;

    #[test]
    fn test_script() -> Result<(), Box<dyn Error>> {
        let script = r#"
#pattern VERSION \d+\.\d+\.\d+

$ something --version || echo 1
? Something %{VERSION}

$ something --help
? Usage: something [OPTIONS]
repeat {
    choice {
? %{DATA} %{GREEDYDATA}
? %{DATA}=%{DATA} %{GREEDYDATA}
    }
}
"#;

        let script = parse_script(script)?;
        assert_eq!(script.commands.len(), 2);
        eprintln!("{:?}", script);
        Ok(())
    }

    #[test]
    fn test_bad_script() -> Result<(), Box<dyn Error>> {
        let script = r#"
$ (cmd; cmd)
$ cmd &
    "#;

        assert!(matches!(
            parse_script(script),
            Err(ScriptError {
                error: ScriptErrorType::BackgroundProcessNotAllowed,
                ..
            })
        ));
        Ok(())
    }

    #[test]
    fn test_literal_match() {
        let output = "hello\nworld";
        let pattern = OutputPattern::Sequence(
            0,
            vec![
                OutputPattern::Literal(0, "hello".to_string()),
                OutputPattern::Literal(0, "world".to_string()),
            ],
        );
        let lines = Lines::new(output.lines().map(|l| l.to_string()).collect::<Vec<_>>());

        let result = pattern.matches(OutputMatchContext::new(), lines);
        assert!(result.unwrap().is_exhausted());
    }

    #[test]
    fn test_repeat_choice() {
        let output = "hello\nworld";
        let pattern = OutputPattern::Sequence(
            0,
            vec![OutputPattern::Repeat(
                0,
                Box::new(OutputPattern::Choice(
                    0,
                    vec![
                        OutputPattern::Literal(0, "hello".to_string()),
                        OutputPattern::Literal(0, "world".to_string()),
                    ],
                )),
            )],
        );
        let lines = Lines::new(output.lines().map(|l| l.to_string()).collect::<Vec<_>>());
        let result = pattern.matches(OutputMatchContext::new(), lines);
        assert!(result.unwrap().is_exhausted());
    }

    #[test]
    fn test_parse_pattern() {
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
        let pattern = parse_output_pattern(
            &pattern.lines().map(|l| l.to_string()).collect::<Vec<_>>(),
            &mut grok,
        )
        .unwrap();
        eprintln!("{pattern:?}");
    }
}
