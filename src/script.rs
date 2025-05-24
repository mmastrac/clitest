use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::{Path, PathBuf},
    process::ExitStatus,
    sync::{Arc, Mutex, atomic::AtomicBool},
    time::Duration,
};

use grok::Grok;
use grok2 as grok;
use serde::{Serialize, ser::SerializeMap};
use termcolor::Color;

use crate::{command::CommandLine, cprint, cprintln, cprintln_rule};

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

impl<'s> IntoIterator for &'s Lines {
    type Item = &'s String;
    type IntoIter = std::slice::Iter<'s, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.lines.iter()
    }
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

#[derive(derive_more::Debug)]
pub struct ScriptRunContext {
    pub args: ScriptRunArgs,
    envs: HashMap<String, String>,
    background: bool,
    #[debug(skip)]
    kill: ScriptKillReceiver,
    #[debug(skip)]
    kill_sender: ScriptKillSender,
}

impl Default for ScriptRunContext {
    fn default() -> Self {
        let kill = Arc::new(AtomicBool::new(false));
        Self {
            args: ScriptRunArgs::default(),
            envs: HashMap::new(),
            background: false,
            kill: ScriptKillReceiver::new(kill.clone()),
            kill_sender: ScriptKillSender::new(kill.clone()),
        }
    }
}

impl ScriptRunContext {
    pub fn new_background(&self) -> Self {
        let kill = Arc::new(AtomicBool::new(false));
        Self {
            args: self.args.clone(),
            envs: self.envs.clone(),
            background: true,
            kill: ScriptKillReceiver::new(kill.clone()),
            kill_sender: ScriptKillSender::new(kill.clone()),
        }
    }

    pub fn pwd(&self) -> PathBuf {
        self.envs
            .get("PWD")
            .map(|s| s.clone())
            .map(PathBuf::from)
            .unwrap_or_else(|| std::env::current_dir().expect("Couldn't get current directory"))
    }

    /// Perform shell expansion on a string.
    fn expand(&self, value: impl AsRef<str>) -> Result<String, ScriptRunError> {
        enum State {
            Normal,
            EscapeNext,
            InCurly,
            Dollar,
            InDollar,
        }

        let value = value.as_ref();

        // "\" triggers escaping
        // ${A} expands to the value of A
        // $A expands to the value of A (variable ends on first non-alphanumeric character)

        let mut state = State::Normal;
        let mut variable = String::new();
        let mut expanded = String::new();

        for c in value.chars() {
            match state {
                State::Normal => {
                    if c == '$' {
                        state = State::Dollar;
                        continue;
                    }
                    if c == '\\' {
                        state = State::EscapeNext;
                        continue;
                    }
                    expanded.push(c);
                }
                State::EscapeNext => {
                    expanded.push(c);
                    state = State::Normal;
                }
                State::InCurly => {
                    if c == '}' {
                        if let Some(value) = self.envs.get(&std::mem::take(&mut variable)) {
                            expanded.push_str(value);
                        } else {
                            return Err(ScriptRunError::ExpansionError(format!(
                                "undefined variable in ${{...}}: {}",
                                variable
                            )));
                        }
                        state = State::Normal;
                    } else {
                        variable.push(c);
                    }
                }
                State::Dollar => {
                    if c.is_alphanumeric() {
                        state = State::InDollar;
                        variable.push(c);
                    } else if c == '{' {
                        state = State::InCurly;
                    } else {
                        return Err(ScriptRunError::ExpansionError(format!(
                            "invalid variable: {}",
                            c
                        )));
                    }
                }
                State::InDollar => {
                    if c.is_alphanumeric() {
                        variable.push(c);
                    } else {
                        if let Some(value) = self.envs.get(&std::mem::take(&mut variable)) {
                            expanded.push_str(value);
                        } else {
                            return Err(ScriptRunError::ExpansionError(format!(
                                "undefined variable in $...: {}",
                                variable
                            )));
                        }
                        expanded.push(c);
                        state = State::Normal;
                    }
                }
            }
        }
        match state {
            State::InDollar => {
                if let Some(value) = self.envs.get(&variable) {
                    expanded.push_str(value);
                } else {
                    return Err(ScriptRunError::ExpansionError(format!(
                        "undefined variable: {}",
                        variable
                    )));
                }
            }
            State::Dollar => {
                return Err(ScriptRunError::ExpansionError(format!(
                    "incomplete variable"
                )));
            }
            State::InCurly => {
                return Err(ScriptRunError::ExpansionError(format!(
                    "unclosed variable: {}",
                    variable
                )));
            }
            State::Normal => {}
            State::EscapeNext => {
                return Err(ScriptRunError::ExpansionError(format!(
                    "unclosed backslash"
                )));
            }
        }
        Ok(expanded)
    }
}

pub struct ScriptKillReceiver {
    kill_receiver: Arc<AtomicBool>,
}

impl ScriptKillReceiver {
    pub fn new(kill_receiver: Arc<AtomicBool>) -> Self {
        Self { kill_receiver }
    }

    pub fn is_killed(&self) -> bool {
        self.kill_receiver.load(std::sync::atomic::Ordering::SeqCst)
    }

    pub fn run_with<T>(&self, kill: impl FnOnce() + Send, wait: impl FnOnce() -> T) -> T {
        std::thread::scope(|s| {
            let done = Arc::new(AtomicBool::new(false));
            let done_clone = done.clone();
            let t = s.spawn(move || {
                while !done_clone.load(std::sync::atomic::Ordering::SeqCst) {
                    if self.is_killed() {
                        kill();
                        break;
                    }
                    std::thread::sleep(Duration::from_millis(10));
                }
            });
            let res = wait();
            done.store(true, std::sync::atomic::Ordering::SeqCst);
            t.join().unwrap();
            res
        })
    }

    #[cfg(windows)]
    pub fn run_cmd(&self, output: std::process::Child) -> std::io::Result<ExitStatus> {
        use std::os::windows::io::AsRawHandle;
        use win32job::Job;

        fn map_job_error(e: win32job::JobError) -> std::io::Error {
            match e {
                win32job::JobError::AssignFailed(e) => e,
                win32job::JobError::CreateFailed(e) => e,
                win32job::JobError::GetInfoFailed(e) => e,
                win32job::JobError::SetInfoFailed(e) => e,
                _ => std::io::Error::new(std::io::ErrorKind::Other, "Unknown error"),
            }
        }

        // Create a new Job object
        let job = Job::create().map_err(map_job_error)?;

        // Configure the job to terminate all child processes when the job is closed
        let mut info = job.query_extended_limit_info().map_err(map_job_error)?;
        info.limit_kill_on_job_close();
        job.set_extended_limit_info(&info).map_err(map_job_error)?;
        job.assign_process(output.as_raw_handle() as _);

        // Resume the main thread for the process
        let id = output.id();
        for thread_entry in tlhelp32::Snapshot::new_thread()? {
            if thread_entry.owner_process_id == id {
                use windows_sys::Win32::Foundation::CloseHandle;
                use windows_sys::Win32::System::Threading::*;

                // TODO: error handling
                unsafe {
                    let thread = OpenThread(THREAD_SUSPEND_RESUME, 0, thread_entry.thread_id);
                    ResumeThread(thread);
                    CloseHandle(thread);
                }
            }
        }

        let job = Mutex::new(Some(job));
        let output = Mutex::new(output);
        self.run_with(
            || {
                _ = job.lock().unwrap().take();
                _ = output.lock().unwrap().kill();
            },
            || {
                let start = std::time::Instant::now();
                let mut warned = false;
                loop {
                    let res = output.lock().unwrap().try_wait()?;
                    if let Some(status) = res {
                        return Ok::<_, std::io::Error>(status);
                    }
                    if start.elapsed() > std::time::Duration::from_secs(10) {
                        if !warned {
                            let child = output.lock().unwrap().id();
                            eprintln!("Process #{child} taking too long to finish.");
                            warned = true;
                        }
                    }
                    std::thread::sleep(Duration::from_millis(10));
                }
            },
        )
    }

    #[cfg(unix)]
    pub fn run_cmd(&self, output: std::process::Child) -> std::io::Result<ExitStatus> {
        let output = Mutex::new(output);
        self.run_with(
            || {
                use signal_child::{signal, signal::*};
                let id = output.lock().unwrap().id() as i32;
                _ = signal(-id, SIGINT);
                std::thread::sleep(Duration::from_millis(10));
                _ = signal(-id, SIGTERM);
            },
            || {
                let start = std::time::Instant::now();
                let mut warned = false;
                loop {
                    let res = output.lock().unwrap().try_wait()?;
                    if let Some(status) = res {
                        return Ok::<_, std::io::Error>(status);
                    }
                    if start.elapsed() > std::time::Duration::from_secs(10) {
                        if !warned {
                            let child = output.lock().unwrap().id();
                            eprintln!("Process #{child} taking too long to finish.");
                            warned = true;
                        }
                    }
                    std::thread::sleep(Duration::from_millis(10));
                }
            },
        )
    }
}

#[derive(Clone)]
pub struct ScriptKillSender {
    kill_sender: Arc<AtomicBool>,
}

impl ScriptKillSender {
    pub fn new(kill_sender: Arc<AtomicBool>) -> Self {
        Self { kill_sender }
    }

    pub fn kill(&self) {
        self.kill_sender
            .store(true, std::sync::atomic::Ordering::SeqCst);
    }
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

        // Set the current working directory as a special variable "PWD"
        envs.insert(
            "PWD".to_string(),
            std::env::current_dir()
                .expect("Couldn't get current directory")
                .to_string_lossy()
                .to_string(),
        );
        // Save the initial PWD as INITIAL_PWD so it can easily be restored
        envs.insert("INITIAL_PWD".to_string(), envs["PWD"].clone());

        let kill = Arc::new(AtomicBool::new(false));

        Self {
            args,
            envs,
            background: false,
            kill: ScriptKillReceiver::new(kill.clone()),
            kill_sender: ScriptKillSender::new(kill.clone()),
        }
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
    #[error("invalid internal command")]
    InvalidInternalCommand,
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
    ) -> Result<Self, grok2::Error> {
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
    ExpansionError(String),
    #[error("{0}")]
    IO(#[from] std::io::Error),
    #[error("killed")]
    Killed,
    #[error("background process took too long to finish")]
    BackgroundProcessTookTooLong,
}

impl ScriptRunError {
    #[allow(unused)]
    pub fn short(&self) -> String {
        match self {
            Self::Pattern(_) => "Pattern".to_string(),
            Self::Exit(status) => format!("Exit({})", status),
            Self::ExpectedFailure => "ExpectedFailure".to_string(),
            Self::IO(e) => format!("IO({:?})", e.kind()),
            Self::Killed => "Killed".to_string(),
            Self::BackgroundProcessTookTooLong => "BackgroundProcessTookTooLong".to_string(),
            Self::ExpansionError(e) => "ExpansionError".to_string(),
        }
    }
}

#[derive(Clone, Debug, thiserror::Error, derive_more::Display, PartialEq, Eq)]
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
        ScriptBlock::run_blocks(context, &self.commands)?;
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

#[derive(derive_more::Debug)]
pub enum ScriptBlock {
    Command(ScriptCommand),
    InternalCommand(InternalCommand),
    Background(Vec<ScriptBlock>),
    Defer(Vec<ScriptBlock>),
    If(IfCondition, Vec<ScriptBlock>),
    For(ForCondition, Vec<ScriptBlock>),
}

impl ScriptBlock {
    pub fn run_blocks(
        context: &mut ScriptRunContext,
        blocks: &[ScriptBlock],
    ) -> Result<Vec<ScriptResult>, ScriptRunError> {
        enum Deferred<'a> {
            Script(&'a ScriptBlock),
            Internal(
                Box<
                    dyn FnOnce(&mut ScriptRunContext) -> Result<(), ScriptRunError>
                        + Send
                        + Sync
                        + 'a,
                >,
            ),
        }

        let mut results = Vec::new();
        std::thread::scope(|s| {
            let mut defer_blocks = VecDeque::new();
            let mut background = Vec::new();
            let mut pending_error = None;
            for block in blocks {
                if context.kill.is_killed() {
                    return Err(ScriptRunError::Killed);
                }
                match block {
                    ScriptBlock::Background(blocks) => {
                        let mut context = context.new_background();
                        context.args.quiet = true;
                        let kill_sender = context.kill_sender.clone();
                        let handle = s.spawn(move || Self::run_blocks(&mut context, blocks));
                        background.push((handle, kill_sender));
                    }
                    ScriptBlock::Defer(blocks) => {
                        // Insert at the front of the queue by extending and
                        // then rotating
                        for block in blocks {
                            defer_blocks.push_back(Deferred::Script(block));
                        }
                        defer_blocks.rotate_left(blocks.len());
                    }
                    ScriptBlock::InternalCommand(command) => {
                        if let Some(f) = command.run(context)? {
                            defer_blocks.insert(0, Deferred::Internal(f));
                        }
                    }
                    _ => match block.run(context) {
                        Ok(res) => results.extend(res),
                        Err(e) => {
                            pending_error = Some(e);
                            break;
                        }
                    },
                }
            }
            for (handle, kill_sender) in background {
                kill_sender.kill();
                let start = std::time::Instant::now();
                let mut warned = false;
                let results = loop {
                    if handle.is_finished() {
                        break handle.join().unwrap()?;
                    }
                    std::thread::sleep(std::time::Duration::from_millis(10));
                    if !warned && start.elapsed() > std::time::Duration::from_secs(10) {
                        if !context.args.quiet {
                            cprintln!(
                                fg = Color::Yellow,
                                "Background process is taking too long to finish."
                            );
                        }
                        warned = true;
                    }
                    if start.elapsed() > std::time::Duration::from_secs(30) {
                        if !context.args.quiet {
                            cprintln!(
                                fg = Color::Red,
                                "Background process took too long to finish."
                            );
                        }
                        return Err(ScriptRunError::BackgroundProcessTookTooLong);
                    }
                };
                for result in results {
                    if !context.args.quiet {
                        cprint!(dimmed = true, "(background) ");
                        for line in result.command.command.split('\n') {
                            cprintln!(fg = Color::Green, "{}", line);
                        }
                        cprintln_rule!(fg = Color::Cyan, "{}", result.command.location);
                        for line in &result.output {
                            cprintln!("{}", line);
                        }
                        if result.output.is_empty() {
                            cprintln!(dimmed = true, "(no output)");
                        }
                        cprintln_rule!();
                    }
                    result.evaluate(context)?;
                }
            }
            for block in defer_blocks {
                match block {
                    Deferred::Script(block) => {
                        if !context.args.quiet {
                            cprint!(dimmed = true, "(deferred) ");
                        }
                        block.run(context)?;
                    }
                    Deferred::Internal(block) => {
                        if !context.args.quiet {
                            cprint!(dimmed = true, "(cleanup) ");
                        }
                        block(context)?;
                    }
                }
            }
            if let Some(error) = pending_error {
                return Err(error);
            }
            Ok(results)
        })
    }

    pub fn run(&self, context: &mut ScriptRunContext) -> Result<Vec<ScriptResult>, ScriptRunError> {
        let pwd = context.pwd();
        if !matches!(std::fs::exists(&pwd), Ok(true)) {
            if !context.args.quiet {
                cprintln!(
                    fg = Color::Red,
                    "$PWD {pwd:?} doesn't exist, returning to $INITIAL_PWD {:?}. Run `cd $INITIAL_PWD` to fix.",
                    context.envs["INITIAL_PWD"]
                );
            }
            context
                .envs
                .insert("PWD".to_string(), context.envs["INITIAL_PWD"].clone());
        }

        match self {
            ScriptBlock::Command(command) => {
                let result = command.run(context)?;
                if !context.background {
                    result.evaluate(context)?;
                    Ok(vec![])
                } else {
                    Ok(vec![result])
                }
            }
            ScriptBlock::If(condition, blocks) => {
                let condition = condition.expand(context)?;
                if condition.matches(context) {
                    Self::run_blocks(context, blocks)
                } else {
                    Ok(vec![])
                }
            }
            ScriptBlock::For(ForCondition::Env(env, values), blocks) => {
                let mut results = Vec::new();
                for value in values {
                    context.envs.insert(env.clone(), context.expand(value)?);
                    results.extend(Self::run_blocks(context, blocks)?);
                }
                Ok(results)
            }
            _ => unreachable!(),
        }
    }
}

impl Serialize for ScriptBlock {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            ScriptBlock::Command(command) => command.serialize(serializer),
            ScriptBlock::InternalCommand(command) => command.serialize(serializer),
            ScriptBlock::Background(blocks) => {
                let mut ser = serializer.serialize_map(Some(1))?;
                ser.serialize_entry("background", blocks)?;
                ser.end()
            }
            ScriptBlock::Defer(blocks) => {
                let mut ser = serializer.serialize_map(Some(1))?;
                ser.serialize_entry("defer", blocks)?;
                ser.end()
            }
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
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum InternalCommand {
    UsingTempdir,
    UsingDir(String, bool),
    ChangeDir(String),
    Set(String, String),
}

impl InternalCommand {
    pub fn run(
        &self,
        context: &mut ScriptRunContext,
    ) -> Result<
        Option<Box<dyn FnOnce(&mut ScriptRunContext) -> Result<(), ScriptRunError> + Send + Sync>>,
        ScriptRunError,
    > {
        match self.clone() {
            InternalCommand::UsingTempdir => {
                let current_pwd = context.pwd();
                let tempdir = tempfile::tempdir().expect("Couldn't create tempdir");
                if !context.args.quiet {
                    cprint!(fg = Color::Yellow, "using tempdir: ");
                    cprintln!("{}", tempdir.path().to_string_lossy());
                    cprintln!();
                }
                context.envs.insert(
                    "PWD".to_string(),
                    tempdir.path().to_string_lossy().to_string(),
                );
                Ok(Some(Box::new(move |context: &mut ScriptRunContext| {
                    if !context.args.quiet {
                        cprintln!(
                            fg = Color::Yellow,
                            "removing {} && cd {}",
                            tempdir.path().to_string_lossy(),
                            current_pwd.to_string_lossy()
                        );
                        cprintln!();
                    }
                    std::fs::remove_dir_all(&tempdir)?;
                    drop(tempdir);
                    Ok::<_, ScriptRunError>(())
                })))
            }
            InternalCommand::UsingDir(dir, new) => {
                let current_pwd = context.pwd();
                let dir = context.expand(dir)?;
                let new_pwd = current_pwd.join(dir);
                if !context.args.quiet {
                    if new {
                        cprint!(fg = Color::Yellow, "using new dir: ");
                    } else {
                        cprint!(fg = Color::Yellow, "using dir: ");
                    }
                    cprintln!("{}", new_pwd.to_string_lossy());
                    cprintln!();
                }

                if new {
                    std::fs::create_dir_all(&new_pwd)?;
                } else {
                    if !new_pwd.exists() {
                        return Err(ScriptRunError::IO(std::io::Error::new(
                            std::io::ErrorKind::NotFound,
                            "directory does not exist",
                        )));
                    }
                }
                context
                    .envs
                    .insert("PWD".to_string(), new_pwd.to_string_lossy().to_string());
                Ok(Some(Box::new(move |context: &mut ScriptRunContext| {
                    if !context.args.quiet {
                        if new {
                            cprintln!(
                                fg = Color::Yellow,
                                "removing {} && cd {}",
                                new_pwd.to_string_lossy(),
                                current_pwd.to_string_lossy()
                            );
                        } else {
                            cprintln!(fg = Color::Yellow, "cd {}", current_pwd.to_string_lossy());
                        }
                        cprintln!();
                    }
                    if new {
                        std::fs::remove_dir_all(&new_pwd)?;
                    }
                    context
                        .envs
                        .insert("PWD".to_string(), current_pwd.to_string_lossy().to_string());
                    Ok::<_, ScriptRunError>(())
                })))
            }
            InternalCommand::ChangeDir(dir) => {
                let dir = context.expand(dir)?;

                if !context.args.quiet {
                    cprintln!(fg = Color::Yellow, "cd {dir}");
                    cprintln!();
                }
                let current_pwd = context.pwd();
                let new_pwd = current_pwd.join(dir);
                context
                    .envs
                    .insert("PWD".to_string(), new_pwd.to_string_lossy().to_string());
                Ok(None)
            }
            InternalCommand::Set(name, value) => {
                let value = context.expand(value)?;

                if !context.args.quiet {
                    cprintln!(fg = Color::Yellow, "set {name} {value}");
                    cprintln!();
                }

                context.envs.insert(name.clone(), value);
                Ok(None)
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

    pub fn expand(&self, context: &ScriptRunContext) -> Result<IfCondition, ScriptRunError> {
        match self {
            IfCondition::True => Ok(IfCondition::True),
            IfCondition::False => Ok(IfCondition::False),
            IfCondition::EnvEq(negated, name, expected) => {
                let value = context.expand(expected)?;
                Ok(IfCondition::EnvEq(*negated, name.clone(), value))
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
    pub fn run(&self, context: &mut ScriptRunContext) -> Result<ScriptResult, ScriptRunError> {
        use crate::{cprintln, cprintln_rule, term::Color};

        let command = &self.command;
        let args = &context.args;

        if !args.quiet {
            for line in command.command.split('\n') {
                cprintln!(fg = Color::Green, "{}", line);
            }
            cprintln_rule!(fg = Color::Cyan, "{}", command.location);
        }
        let (output, status) = command.run(
            context.args.quiet || context.background,
            context.args.show_line_numbers,
            context.args.runner.clone(),
            &context.envs,
            &context.kill,
        )?;

        // Side-effects
        if let Some(set_var) = &self.set_var {
            context
                .envs
                .insert(set_var.clone(), output.to_string().trim().to_string());
        }

        let exit_result = if !self.exit.matches(status) {
            ExitResult::Mismatch(status)
        } else {
            ExitResult::Matches(status)
        };

        let context = OutputMatchContext::new(context);
        let pattern_result = match self.pattern.matches(context.clone(), output.clone()) {
            Ok(_) => {
                if self.expect_failure {
                    PatternResult::ExpectedFailure
                } else {
                    PatternResult::Matches
                }
            }
            Err(e) => {
                if self.expect_failure {
                    PatternResult::MatchesFailure
                } else {
                    let mut trace = String::new();
                    for line in context.traces() {
                        trace.push_str(&format!("{line}\n"));
                    }
                    PatternResult::Mismatch(e, trace)
                }
            }
        };

        if output.is_empty() && !args.quiet {
            cprintln!(dimmed = true, "(no output)");
        }
        if !args.quiet {
            cprintln_rule!();
        }

        if let Some(delay) = args.delay_steps {
            std::thread::sleep(std::time::Duration::from_millis(delay));
        }

        Ok(ScriptResult {
            command: command.clone(),
            pattern: pattern_result,
            exit: exit_result,
            output,
        })
    }
}

#[derive(derive_more::Debug)]
pub struct ScriptResult {
    pub command: CommandLine,
    pub pattern: PatternResult,
    pub exit: ExitResult,
    #[debug(skip)]
    pub output: Lines,
}

impl ScriptResult {
    pub fn evaluate(&self, context: &mut ScriptRunContext) -> Result<(), ScriptRunError> {
        let args = &context.args;
        let (success, failure, arrow) = if *crate::term::IS_UTF8 {
            ("✅", "❌", "→")
        } else {
            ("[*]", "[X]", "->")
        };

        if let ExitResult::Mismatch(status) = self.exit {
            if !args.ignore_exit_codes {
                if !args.quiet {
                    cprintln!(fg = Color::Red, "{failure} FAIL: {status}");
                    cprintln!(dimmed = true, " {arrow} {}", self.command.command);
                    cprintln!();
                }
                return Err(ScriptRunError::Exit(status));
            }
        }

        if let PatternResult::Mismatch(e, trace) = &self.pattern {
            if !args.ignore_matches {
                if !args.quiet {
                    cprintln!(fg = Color::Red, "ERROR: {e}");
                    cprintln!(dimmed = true, "{trace}");
                    cprintln!(fg = Color::Red, "{failure} FAIL");
                    cprintln!();
                }
                return Err(ScriptRunError::Pattern(e.clone()));
            }
        }

        if let PatternResult::ExpectedFailure = self.pattern {
            if !args.ignore_matches {
                if !args.quiet {
                    cprintln!(fg = Color::Red, "{failure} FAIL (output shouldn't match)");
                    cprintln!(dimmed = true, " {arrow} {}", self.command.command);
                    cprintln!();
                }
                return Err(ScriptRunError::ExpectedFailure);
            }
        }

        if let ExitResult::Matches(status) = self.exit {
            if !args.quiet {
                if status.success() {
                    cprintln!(fg = Color::Green, "{success} OK");
                } else {
                    cprintln!(fg = Color::Green, "{success} OK ({status})");
                }
                cprintln!();
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum PatternResult {
    Matches,
    MatchesFailure,
    ExpectedFailure,
    Mismatch(OutputPatternMatchFailure, String),
}

#[derive(Debug)]
pub enum ExitResult {
    Matches(ExitStatus),
    Mismatch(ExitStatus),
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

    #[test]
    fn test_script_run_context_expand() {
        let mut context = ScriptRunContext::new(ScriptRunArgs::default());
        context.envs.insert("A".to_string(), "1".to_string());
        context.envs.insert("B".to_string(), "2".to_string());
        context.envs.insert("C".to_string(), "3".to_string());
        assert_eq!(context.expand("$A").unwrap(), "1".to_string());
        assert_eq!(context.expand("$A $B ").unwrap(), "1 2 ".to_string());
        assert_eq!(context.expand("${A} ${B} ").unwrap(), "1 2 ".to_string());
        assert_eq!(context.expand(r#"\$A"#).unwrap(), "$A".to_string());
        assert_eq!(context.expand(r#"\${A}"#).unwrap(), "${A}".to_string());
        assert_eq!(context.expand(r#"\\$A"#).unwrap(), r#"\1"#);
        assert_eq!(context.expand(r#"\\${A}"#).unwrap(), r#"\1"#);
    }
}
