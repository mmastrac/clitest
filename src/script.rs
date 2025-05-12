use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    process::ExitStatus,
    sync::{Arc, Mutex},
};

use grok::Grok;
use serde::{Serialize, ser::SerializeMap};

use crate::{command::CommandLine, cprintln_rule};

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
                                location: rejected_pattern.location.clone(),
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

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct ScriptLocation {
    pub file: ScriptFile,
    pub line: usize,
}

impl ScriptLocation {
    pub fn new(file: ScriptFile, line: usize) -> Self {
        Self { file, line }
    }
}

impl std::fmt::Display for ScriptLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.file, self.line)
    }
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct ScriptFile {
    pub file: Arc<PathBuf>,
}

impl ScriptFile {
    /// Normalizes/prettifies the path if we can.
    pub fn new(file: PathBuf) -> Self {
        if cfg!(unix) {
            if let Ok(canonical) = file.canonicalize() {
                if let Some(home) = dirs::home_dir() {
                    if canonical.starts_with(&home) {
                        if let Some(diff) = pathdiff::diff_paths(&canonical, home) {
                            return Self {
                                file: Arc::new(Path::new("~").join(diff)),
                            };
                        }
                    }
                }
                if let Ok(tmp) = Path::new("/tmp").canonicalize() {
                    if canonical.starts_with(&tmp) {
                        if let Some(diff) = pathdiff::diff_paths(&canonical, &tmp) {
                            return Self {
                                file: Arc::new(Path::new("/tmp").join(diff)),
                            };
                        }
                    }
                }
            }
        }
        Self {
            file: Arc::new(file),
        }
    }
}

impl std::fmt::Display for ScriptFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.file.display())
    }
}

#[derive(derive_more::Debug, Serialize)]
pub struct Script {
    pub commands: Vec<ScriptBlock>,
    #[debug(skip)]
    #[serde(skip)]
    pub grok: Grok,
}

#[derive(Debug, Clone, Default)]
pub struct ScriptRunArgs {
    pub delay_steps: Option<u64>,
    pub ignore_exit_codes: bool,
    pub ignore_matches: bool,
    pub quiet: bool,
    pub show_line_numbers: bool,
    pub runner: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct ScriptRunContext {
    pub args: ScriptRunArgs,
    envs: HashMap<String, String>,
}

impl ScriptRunContext {
    pub fn new(args: ScriptRunArgs) -> Self {
        let mut envs = HashMap::new();

        macro_rules! target {
            ($env:ident, $var:ident, [$($vals:expr),*]) => {
                $(
                if cfg!($var = $vals) {
                    envs.insert(stringify!($env).to_string(), $vals.to_string());
                }
                )*
            };
        }

        target!(
            TARGET_OS,
            target_os,
            ["windows", "linux", "macos", "ios", "android"]
        );
        target!(TARGET_FAMILY, target_family, ["windows", "unix", "wasm"]);
        target!(
            TARGET_ARCH,
            target_arch,
            ["x86", "x86_64", "arm", "aarch64"]
        );

        Self { args, envs }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ScriptLine {
    pub location: ScriptLocation,
    text: String,
}

impl ScriptLine {
    pub fn new(file: ScriptFile, line: usize, text: impl AsRef<str>) -> Self {
        Self {
            location: ScriptLocation::new(file, line),
            text: text.as_ref().to_string(),
        }
    }

    pub fn parse(file: ScriptFile, text: impl AsRef<str>) -> Vec<Self> {
        text.as_ref()
            .lines()
            .enumerate()
            .map(|(line, text)| Self {
                location: ScriptLocation::new(file.clone(), line + 1),
                text: text.to_string(),
            })
            .collect()
    }

    pub fn starts_with(&self, text: &str) -> bool {
        self.text.trim().starts_with(text)
    }

    pub fn first_char(&self) -> Option<char> {
        self.text.trim().chars().next()
    }

    pub fn text(&self) -> &str {
        self.text.trim()
    }

    pub fn text_untrimmed(&self) -> &str {
        &self.text
    }

    pub fn is_empty(&self) -> bool {
        self.text.trim().is_empty()
    }

    pub fn strip_prefix(&self, prefix: &str) -> Option<&str> {
        self.text.strip_prefix(prefix)
    }
}

#[derive(Debug, thiserror::Error, derive_more::Display)]
#[display("{error} at {location}{}", associated_data.as_deref().map_or("".to_string(), |d| format!(": {d}")))]
pub struct ScriptError {
    pub error: ScriptErrorType,
    pub location: ScriptLocation,
    pub associated_data: Option<String>,
}

impl ScriptError {
    pub fn new(error: ScriptErrorType, location: ScriptLocation) -> Self {
        if std::env::var("PANIC_ON_ERROR").is_ok() {
            panic!("ScriptError: {error} at {location}");
        }
        Self {
            error,
            location,
            associated_data: None,
        }
    }

    pub fn new_with_data(
        error: ScriptErrorType,
        location: ScriptLocation,
        associated_data: String,
    ) -> Self {
        if std::env::var("PANIC_ON_ERROR").is_ok() {
            panic!("ScriptError: {error} at {location}: {associated_data}");
        }
        Self {
            error,
            location,
            associated_data: Some(associated_data),
        }
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
    #[error("invalid pattern at global level (only reject or ignore allowed here)")]
    InvalidGlobalPattern,
    #[error("invalid block type")]
    InvalidBlockType,
    #[error("unsupported command position")]
    UnsupportedCommandPosition,
    #[error("invalid trailing pattern after *")]
    InvalidAnyPattern,
    #[error("invalid exit status")]
    InvalidExitStatus,
    #[error("invalid set variable")]
    InvalidSetVariable,
    #[error("invalid version")]
    InvalidVersion,
    #[error("missing command lines")]
    MissingCommandLines,
    #[error(
        "block end without matching block start, too many closing braces or braces not properly nested"
    )]
    InvalidBlockEnd,
    #[error("invalid if condition")]
    InvalidIfCondition,
}

#[derive(Clone)]
pub struct OutputPattern {
    pub location: ScriptLocation,
    pub pattern: OutputPatternType,
    pub ignore: Arc<Vec<OutputPattern>>,
    pub reject: Arc<Vec<OutputPattern>>,
}

impl Serialize for OutputPattern {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.pattern.serialize(serializer)
    }
}

impl std::fmt::Debug for OutputPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.pattern)
    }
}

impl OutputPattern {
    pub fn new_sequence(location: ScriptLocation, mut patterns: Vec<OutputPattern>) -> Self {
        if patterns.len() == 1 {
            patterns.remove(0)
        } else {
            Self {
                pattern: OutputPatternType::Sequence(patterns),
                ignore: Default::default(),
                reject: Default::default(),
                location: location.clone(),
            }
        }
    }

    pub fn matches(
        &self,
        context: OutputMatchContext,
        output: Lines,
    ) -> Result<Lines, OutputPatternMatchFailure> {
        if self.ignore.is_empty() && self.reject.is_empty() {
            self.pattern.matches(&self.location, context, output)
        } else {
            let output = output.with_ignore(&self.ignore).with_reject(&self.reject);
            self.pattern.matches(&self.location, context, output)
        }
    }

    /// The minimum number of lines this pattern will match.
    pub fn min_matches(&self) -> usize {
        self.pattern.min_matches()
    }

    /// The maximum number of lines this pattern will match (or usize::MAX if unbounded).
    pub fn max_matches(&self) -> usize {
        self.pattern.max_matches()
    }
}

#[derive(Clone)]
pub enum OutputPatternType {
    /// The end of the output
    End,
    /// Matches no lines of output, always succeeds
    None,
    /// Any lines, followed by a pattern.
    Any(Box<OutputPattern>),
    /// A literal string
    Literal(String),
    /// A grok pattern
    Pattern(Arc<GrokPattern>),
    /// A pattern that matches one or more of the given pattern
    Repeat(Box<OutputPattern>),
    /// A pattern that matches zero or one of the given pattern
    Optional(Box<OutputPattern>),
    /// A pattern that all of its subpatterns, but in any order
    Unordered(Vec<OutputPattern>),
    /// A pattern that matches one of the given patterns
    Choice(Vec<OutputPattern>),
    /// A pattern that matches a sequence of patterns
    Sequence(Vec<OutputPattern>),
    /// A pattern that matches a condition
    If(IfCondition, Box<OutputPattern>),
}

impl Serialize for OutputPatternType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            OutputPatternType::Literal(literal) => {
                serializer.serialize_str(&format!("! {literal}"))
            }
            OutputPatternType::Pattern(pattern) => {
                serializer.serialize_str(&format!("? {}", pattern.pattern))
            }
            OutputPatternType::Repeat(pattern) => {
                HashMap::from([("repeat", &pattern)]).serialize(serializer)
            }
            OutputPatternType::Optional(pattern) => {
                HashMap::from([("optional", &pattern)]).serialize(serializer)
            }
            OutputPatternType::Unordered(patterns) => {
                HashMap::from([("unordered", &patterns)]).serialize(serializer)
            }
            OutputPatternType::Choice(patterns) => {
                HashMap::from([("choice", &patterns)]).serialize(serializer)
            }
            OutputPatternType::Sequence(patterns) => {
                HashMap::from([("sequence", &patterns)]).serialize(serializer)
            }
            OutputPatternType::Any(pattern) => {
                HashMap::from([("any", &pattern)]).serialize(serializer)
            }
            OutputPatternType::If(condition, pattern) => {
                #[derive(Serialize)]
                struct If<'a> {
                    condition: &'a IfCondition,
                    pattern: &'a OutputPattern,
                }
                If { condition, pattern }.serialize(serializer)
            }
            OutputPatternType::End => serializer.serialize_str("end"),
            OutputPatternType::None => serializer.serialize_str("none"),
        }
    }
}

impl OutputPatternType {
    /// The minimum number of lines this pattern will match.
    pub fn min_matches(&self) -> usize {
        match self {
            OutputPatternType::None => 0,
            OutputPatternType::Literal(_) => 1,
            OutputPatternType::Pattern(_) => 1,
            OutputPatternType::Repeat(pattern) => pattern.min_matches(),
            OutputPatternType::Optional(_) => 0,
            OutputPatternType::Unordered(patterns) => {
                patterns.iter().map(|p| p.min_matches()).sum()
            }
            OutputPatternType::Choice(patterns) => {
                patterns.iter().map(|p| p.min_matches()).min().unwrap_or(0)
            }
            OutputPatternType::Sequence(patterns) => patterns.iter().map(|p| p.min_matches()).sum(),
            OutputPatternType::Any(pattern) => pattern.min_matches(),
            OutputPatternType::If(_, _) => 0,
            OutputPatternType::End => 0,
        }
    }

    /// The maximum number of lines this pattern will match (or usize::MAX if unbounded).
    pub fn max_matches(&self) -> usize {
        fn saturating_iter_sum<I>(iter: I) -> usize
        where
            I: IntoIterator<Item = usize>,
        {
            iter.into_iter()
                .reduce(|n, i| n.saturating_add(i))
                .unwrap_or(0)
        }

        match self {
            OutputPatternType::None => 0,
            OutputPatternType::Literal(_) => 1,
            OutputPatternType::Pattern(_) => 1,
            OutputPatternType::Repeat(pattern) => {
                if pattern.max_matches() == 0 {
                    0
                } else {
                    usize::MAX
                }
            }
            OutputPatternType::Optional(pattern) => pattern.max_matches(),
            OutputPatternType::Unordered(patterns) => {
                saturating_iter_sum(patterns.iter().map(|p| p.max_matches()))
            }
            OutputPatternType::Choice(patterns) => {
                patterns.iter().map(|p| p.max_matches()).max().unwrap_or(0)
            }
            OutputPatternType::Sequence(patterns) => {
                saturating_iter_sum(patterns.iter().map(|p| p.max_matches()))
            }
            OutputPatternType::Any(_) => usize::MAX,
            OutputPatternType::If(_, pattern) => pattern.max_matches(),
            OutputPatternType::End => 0,
        }
    }
}

impl Default for OutputPatternType {
    fn default() -> Self {
        Self::Sequence(vec![])
    }
}

impl std::fmt::Debug for OutputPatternType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputPatternType::Literal(literal) => write!(f, "Literal({literal})"),
            OutputPatternType::Pattern(pattern) => write!(f, "Pattern({pattern:?})"),
            OutputPatternType::Repeat(pattern) => write!(f, "Repeat({pattern:?})"),
            OutputPatternType::Optional(pattern) => write!(f, "Optional({pattern:?})"),
            OutputPatternType::Unordered(patterns) => write!(f, "Unordered({patterns:?})"),
            OutputPatternType::Choice(patterns) => write!(f, "Choice({patterns:?})"),
            OutputPatternType::Sequence(patterns) => write!(f, "Sequence({patterns:?})"),
            OutputPatternType::Any(until) => write!(f, "Any({until:?})"),
            OutputPatternType::If(condition, pattern) => {
                write!(f, "If({condition:?}, {pattern:?})")
            }
            OutputPatternType::End => write!(f, "End"),
            OutputPatternType::None => write!(f, "None"),
        }
    }
}

#[derive(Serialize, derive_more::Debug)]
#[debug("/{pattern:?}/")]
pub struct GrokPattern {
    pattern: String,
    #[serde(skip)]
    grok: grok::Pattern,
}

impl GrokPattern {
    pub fn compile(
        grok: &mut Grok,
        line: &str,
        escape_non_grok: bool,
    ) -> Result<Self, grok::Error> {
        if escape_non_grok {
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
            Ok(Self {
                pattern: escaped_string,
                grok: grok.compile(&eol, false)?,
            })
        } else {
            let eol = format!("{line}$");
            Ok(Self {
                pattern: line.to_string(),
                grok: grok.compile(&eol, false)?,
            })
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
#[display("pattern {pattern_type} at line {location} does not match output line {:?}", output_line.as_ref().map(|l| l.text.clone()).unwrap_or("<eof>".to_string()))]
pub struct OutputPatternMatchFailure {
    pub location: ScriptLocation,
    pub pattern_type: &'static str,
    pub output_line: Option<Line>,
}

#[derive(Debug, Clone)]
pub struct OutputMatchContext<'s> {
    depth: usize,
    trace: Arc<Mutex<Vec<String>>>,
    ignore: bool,
    script_context: &'s ScriptRunContext,
}

impl<'s> OutputMatchContext<'s> {
    pub fn new(script_context: &'s ScriptRunContext) -> Self {
        Self {
            depth: 0,
            trace: Default::default(),
            ignore: false,
            script_context,
        }
    }

    pub fn descend(&self) -> Self {
        Self {
            depth: self.depth + 1,
            trace: self.trace.clone(),
            ignore: self.ignore,
            script_context: self.script_context,
        }
    }

    pub fn ignore(&self) -> Self {
        Self {
            depth: self.depth,
            trace: self.trace.clone(),
            ignore: true,
            script_context: self.script_context,
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

impl OutputPatternType {
    pub fn matches<'s>(
        &self,
        location: &ScriptLocation,
        context: OutputMatchContext,
        mut output: Lines,
    ) -> Result<Lines, OutputPatternMatchFailure> {
        context.trace(&format!("matching {:?}", self));
        match self {
            OutputPatternType::None => Ok(output),
            OutputPatternType::Literal(literal) => {
                let (line, next) = output.next(context.clone())?;
                if let Some(line) = line {
                    if &line.text.trim_end() == literal {
                        context.trace(&format!("literal match: {:?} == {literal:?}", line.text));
                        Ok(next)
                    } else {
                        context.trace(&format!(
                            "literal FAILED match: {:?} == {literal:?}",
                            line.text
                        ));
                        Err(OutputPatternMatchFailure {
                            location: location.clone(),
                            pattern_type: "literal",
                            output_line: Some(line),
                        })
                    }
                } else {
                    Err(OutputPatternMatchFailure {
                        location: location.clone(),
                        pattern_type: "literal",
                        output_line: None,
                    })
                }
            }
            OutputPatternType::Pattern(pattern) => {
                let (line, next) = output.next(context.clone())?;
                if let Some(line) = line {
                    let text = line.text.clone();
                    // Don't print panic backtraces
                    let res = match std::panic::catch_unwind(|| pattern.grok.match_against(&text)) {
                        Ok(res) => res,
                        Err(_) => {
                            return Err(OutputPatternMatchFailure {
                                location: location.clone(),
                                pattern_type: "pattern",
                                output_line: Some(line),
                            });
                        }
                    };
                    if let Some(matches) = res {
                        context.trace(&format!("pattern match: {:?} =~ {pattern:?}", line.text));
                        Ok(next)
                    } else {
                        context.trace(&format!(
                            "pattern FAILED match: {:?} =~ {pattern:?}",
                            line.text
                        ));
                        Err(OutputPatternMatchFailure {
                            location: location.clone(),
                            pattern_type: "pattern",
                            output_line: Some(line),
                        })
                    }
                } else {
                    Err(OutputPatternMatchFailure {
                        location: location.clone(),
                        pattern_type: "pattern",
                        output_line: None,
                    })
                }
            }
            OutputPatternType::Sequence(patterns) => {
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
            OutputPatternType::Repeat(pattern) => {
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
            OutputPatternType::Optional(pattern) => {
                // Never fails
                match pattern.matches(context.descend(), output.clone()) {
                    Ok(v) => Ok(v),
                    Err(_) => Ok(output),
                }
            }
            OutputPatternType::Unordered(patterns) => {
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
                        location: location.clone(),
                        pattern_type: "unordered",
                        output_line: None,
                    });
                }
                Ok(output)
            }
            OutputPatternType::Choice(patterns) => {
                for pattern in patterns {
                    if let Ok(v) = pattern.matches(context.descend(), output.clone()) {
                        return Ok(v);
                    }
                }
                Err(OutputPatternMatchFailure {
                    location: location.clone(),
                    pattern_type: "choice",
                    output_line: None,
                })
            }
            OutputPatternType::Any(until) => {
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
            }
            OutputPatternType::If(condition, pattern) => {
                if condition.matches(context.script_context) {
                    context.trace(&format!("if match: {:?}", condition));
                    pattern.matches(context.clone(), output.clone())
                } else {
                    context.trace(&format!("if FAILED match: {:?}", condition));
                    Ok(output)
                }
            }
            OutputPatternType::End => {
                let (line, next) = output.next(context)?;
                if let Some(line) = line {
                    Err(OutputPatternMatchFailure {
                        location: location.clone(),
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
    pub fn run(&self, context: &mut ScriptRunContext) -> Result<(), ScriptRunError> {
        for command in &self.commands {
            command.run(context)?;
        }
        Ok(())
    }
}

#[derive(Debug, Default, Serialize)]
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

    pub fn is_success(&self) -> bool {
        matches!(self, CommandExit::Success)
    }
}

#[derive(Debug)]
pub enum ScriptBlock {
    Command(ScriptCommand),
    If(IfCondition, Vec<ScriptBlock>),
    For(ForCondition, Vec<ScriptBlock>),
}

impl ScriptBlock {
    pub fn run(&self, context: &mut ScriptRunContext) -> Result<(), ScriptRunError> {
        match self {
            ScriptBlock::Command(command) => command.run(context)?,
            ScriptBlock::If(condition, blocks) => {
                if condition.matches(context) {
                    for block in blocks {
                        block.run(context)?;
                    }
                }
            }
            ScriptBlock::For(ForCondition::Env(env, values), blocks) => {
                for value in values {
                    context.envs.insert(env.clone(), value.clone());
                    for block in blocks {
                        block.run(context)?;
                    }
                }
            }
            _ => unreachable!(),
        }
        Ok(())
    }
}

impl Serialize for ScriptBlock {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            ScriptBlock::Command(command) => command.serialize(serializer),
            ScriptBlock::If(condition, blocks) => {
                let mut ser = serializer.serialize_map(Some(2))?;
                ser.serialize_entry("if", condition)?;
                ser.serialize_entry("blocks", blocks)?;
                ser.end()
            }
            ScriptBlock::For(condition, blocks) => {
                let mut ser = serializer.serialize_map(Some(2))?;
                ser.serialize_entry("for", condition)?;
                ser.serialize_entry("blocks", blocks)?;
                ser.end()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum IfCondition {
    True,
    False,
    EnvEq(bool, String, String),
}

impl IfCondition {
    pub fn matches(&self, context: &ScriptRunContext) -> bool {
        match self {
            IfCondition::True => true,
            IfCondition::False => false,
            IfCondition::EnvEq(negated, name, expected) => {
                let value = context
                    .envs
                    .get(name)
                    .map(|s| s.as_str())
                    .unwrap_or_default();
                (value == expected) ^ negated
            }
        }
    }
}

impl Serialize for IfCondition {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            IfCondition::True => "true".serialize(serializer),
            IfCondition::False => "false".serialize(serializer),
            IfCondition::EnvEq(negated, name, value) => {
                let mut ser = serializer.serialize_map(Some(2))?;
                ser.serialize_entry("env", name)?;
                ser.serialize_entry("value", value)?;
                ser.end()
            }
        }
    }
}

#[derive(Debug)]
pub enum ForCondition {
    Env(String, Vec<String>),
}

impl Serialize for ForCondition {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}

fn is_bool_false(b: &bool) -> bool {
    !b
}

#[derive(Debug, Serialize)]
pub struct ScriptCommand {
    pub command: CommandLine,
    pub pattern: OutputPattern,
    #[serde(skip_serializing_if = "CommandExit::is_success")]
    pub exit: CommandExit,
    #[serde(skip_serializing_if = "is_bool_false")]
    pub expect_failure: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub set_var: Option<String>,
}

impl ScriptCommand {
    pub fn run(&self, context: &mut ScriptRunContext) -> Result<(), ScriptRunError> {
        use crate::{cprintln, cprintln_rule, term::Color};

        let command = &self.command;
        let args = &context.args;

        if !args.quiet {
            cprintln!(fg = Color::Green, "{}", command.command);
            cprintln_rule!(fg = Color::Cyan, "{}", command.location);
        }
        let (output, status) = command.run(
            context.args.quiet,
            context.args.show_line_numbers,
            context.args.runner.clone(),
            &context.envs,
        )?;
        if !self.exit.matches(status) {
            println!("❌ FAIL: {status}");
            if !args.ignore_exit_codes {
                return Err(ScriptRunError::Exit(status));
            }
        }
        if output.is_empty() && !args.quiet {
            cprintln!(dimmed = true, "(no output)");
        }
        if let Some(set_var) = &self.set_var {
            context
                .envs
                .insert(set_var.clone(), output.to_string().trim().to_string());
        }
        if !args.quiet {
            cprintln_rule!();
        }
        let context = OutputMatchContext::new(context);
        match self.pattern.matches(context.clone(), output) {
            Ok(_) => {
                if self.expect_failure {
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
                if !self.expect_failure {
                    println!("ERROR: {e}");
                    for line in context.traces() {
                        println!("{line}");
                    }
                }
                if self.expect_failure {
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
            cprintln!();
        }
        if let Some(delay) = args.delay_steps {
            std::thread::sleep(std::time::Duration::from_millis(delay));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::v0::parse_script;

    use super::*;
    use std::error::Error;

    #[test]
    fn test_script() -> Result<(), Box<dyn Error>> {
        let script = r#"
pattern VERSION \d+\.\d+\.\d+

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

        let script = parse_script(ScriptFile::new("test.cli".into()), script)?;
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
            parse_script(ScriptFile::new("test.cli".into()), script),
            Err(ScriptError {
                error: ScriptErrorType::BackgroundProcessNotAllowed,
                ..
            })
        ));
        Ok(())
    }
}
