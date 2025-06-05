use std::{
    collections::{HashMap, VecDeque},
    path::{Path, PathBuf},
    process::ExitStatus,
    sync::{Arc, Mutex, atomic::AtomicBool},
    thread::ScopedJoinHandle,
    time::{Duration, Instant},
};

use grok::Grok;
use keepcalm::SharedMut;
use serde::{Serialize, ser::SerializeMap};
use termcolor::{Color, ColorChoice, WriteColor};

use crate::command::CommandLine;
use crate::output::*;
use crate::{cwrite, cwriteln, cwriteln_rule};

const DEFAULT_TIMEOUT: Duration = Duration::from_secs(30);

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
    pub show_line_numbers: bool,
    pub runner: Option<String>,
    pub quiet: bool,
    pub timeout: Option<Duration>,
    pub no_color: bool,
}

#[derive(derive_more::Debug, Clone)]
pub struct ScriptOutput {
    #[debug(skip)]
    stream: SharedMut<Box<dyn WriteColorAny>>,
}

trait WriteColorAny: WriteColor + Send + Sync + std::any::Any + 'static + std::fmt::Debug {}

impl WriteColorAny for termcolor::StandardStream {}
impl WriteColorAny for termcolor::Buffer {}

impl ScriptOutput {
    pub fn no_color() -> Self {
        let stm = termcolor::StandardStream::stdout(ColorChoice::Never);
        Self {
            stream: SharedMut::new(Box::new(stm) as _),
        }
    }

    pub fn quiet(no_color: bool) -> Self {
        let stm = if no_color {
            termcolor::Buffer::no_color()
        } else {
            termcolor::Buffer::ansi()
        };
        Self {
            stream: SharedMut::new(Box::new(stm) as _),
        }
    }

    pub fn take_buffer(self) -> String {
        let stream = SharedMut::try_unwrap(self.stream).unwrap();
        let stream = stream as Box<dyn std::any::Any>;
        let stream = stream.downcast::<termcolor::Buffer>().unwrap();
        String::from_utf8_lossy(&stream.into_inner()).to_string()
    }
}

impl Default for ScriptOutput {
    fn default() -> Self {
        let stm = termcolor::StandardStream::stdout(ColorChoice::Auto);
        Self {
            stream: SharedMut::new(Box::new(stm) as _),
        }
    }
}

impl std::io::Write for ScriptOutputLock<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.stream.write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.stream.flush()
    }
}

impl termcolor::WriteColor for ScriptOutputLock<'_> {
    fn supports_color(&self) -> bool {
        self.stream.supports_color()
    }
    fn set_color(&mut self, spec: &termcolor::ColorSpec) -> std::io::Result<()> {
        self.stream.set_color(spec)
    }
    fn reset(&mut self) -> std::io::Result<()> {
        self.stream.reset()
    }
    fn is_synchronous(&self) -> bool {
        self.stream.is_synchronous()
    }
    fn set_hyperlink(&mut self, _link: &termcolor::HyperlinkSpec) -> std::io::Result<()> {
        self.stream.set_hyperlink(_link)
    }
    fn supports_hyperlinks(&self) -> bool {
        self.stream.supports_hyperlinks()
    }
}

struct ScriptOutputLock<'a> {
    stream: keepcalm::SharedWriteLock<'a, Box<dyn WriteColorAny>>,
}

#[derive(derive_more::Debug)]
pub struct ScriptRunContext {
    pub args: ScriptRunArgs,
    pub envs: HashMap<String, String>,
    background: bool,
    #[debug(skip)]
    kill: ScriptKillReceiver,
    #[debug(skip)]
    kill_sender: ScriptKillSender,
    output: ScriptOutput,
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
            output: ScriptOutput::default(),
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
            output: ScriptOutput::quiet(self.args.no_color),
        }
    }

    pub fn pwd(&self) -> PathBuf {
        self.envs
            .get("PWD")
            .cloned()
            .map(PathBuf::from)
            .unwrap_or_else(|| std::env::current_dir().expect("Couldn't get current directory"))
    }

    pub fn take_output(self) -> String {
        self.output.take_buffer()
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
                return Err(ScriptRunError::ExpansionError(
                    "incomplete variable".to_string(),
                ));
            }
            State::InCurly => {
                return Err(ScriptRunError::ExpansionError(format!(
                    "unclosed variable: {}",
                    variable
                )));
            }
            State::Normal => {}
            State::EscapeNext => {
                return Err(ScriptRunError::ExpansionError(
                    "unclosed backslash".to_string(),
                ));
            }
        }
        Ok(expanded)
    }

    /// Get a mutable reference to the output stream.
    pub fn stream(&self) -> impl termcolor::WriteColor + use<'_> {
        ScriptOutputLock {
            stream: self.output.stream.write(),
        }
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
                    if start.elapsed() > std::time::Duration::from_secs(10) && !warned {
                        let child = output.lock().unwrap().id();
                        eprintln!("Process #{child} taking too long to finish.");
                        warned = true;
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
    pub fn new(args: ScriptRunArgs, script_path: &Path) -> Self {
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
            script_path.parent().unwrap().to_string_lossy().to_string(),
        );
        // Save the initial PWD as INITIAL_PWD so it can easily be restored
        envs.insert("INITIAL_PWD".to_string(), envs["PWD"].clone());

        let kill = Arc::new(AtomicBool::new(false));

        let output = if args.quiet {
            ScriptOutput::quiet(args.no_color)
        } else if args.no_color {
            ScriptOutput::no_color()
        } else {
            ScriptOutput::default()
        };

        Self {
            args,
            envs,
            background: false,
            kill: ScriptKillReceiver::new(kill.clone()),
            kill_sender: ScriptKillSender::new(kill.clone()),
            output,
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
    #[error("retry took too long to finish")]
    RetryTookTooLong,
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
            Self::RetryTookTooLong => "RetryTookTooLong".to_string(),
        }
    }
}

impl Script {
    pub fn run(&self, context: &mut ScriptRunContext) -> Result<(), ScriptRunError> {
        let v = ScriptBlock::run_blocks(context, &self.commands)?;
        assert!(v.is_empty(), "script did not run to completion: {v:?}");
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
    Retry(Vec<ScriptBlock>),
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
            Background(
                ScopedJoinHandle<'a, Result<Vec<ScriptResult>, ScriptRunError>>,
                ScriptKillSender,
            ),
        }

        let mut results = Vec::new();
        std::thread::scope(|s| {
            let mut defer_blocks = VecDeque::new();
            let mut pending_error = None;
            for block in blocks {
                if context.kill.is_killed() {
                    return Err(ScriptRunError::Killed);
                }
                match block {
                    ScriptBlock::Background(blocks) => {
                        let mut context = context.new_background();
                        let kill_sender = context.kill_sender.clone();
                        let handle = s.spawn(move || Self::run_blocks(&mut context, blocks));
                        defer_blocks.push_front(Deferred::Background(handle, kill_sender));
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
                            defer_blocks.push_front(Deferred::Internal(f));
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
            for block in defer_blocks {
                match block {
                    Deferred::Script(block) => {
                        cwrite!(context.stream(), dimmed = true, "(deferred) ");
                        block.run(context)?;
                    }
                    Deferred::Internal(block) => {
                        cwrite!(context.stream(), dimmed = true, "(cleanup) ");
                        block(context)?;
                    }
                    Deferred::Background(handle, kill_sender) => {
                        kill_sender.kill();
                        let start = std::time::Instant::now();
                        let mut warned = false;
                        let results = loop {
                            if handle.is_finished() {
                                break handle.join().unwrap()?;
                            }
                            std::thread::sleep(std::time::Duration::from_millis(10));
                            if !warned && start.elapsed() > std::time::Duration::from_secs(10) {
                                cwriteln!(
                                    context.stream(),
                                    fg = Color::Yellow,
                                    "Background process is taking too long to finish."
                                );
                                warned = true;
                            }
                            if start.elapsed() > context.args.timeout.unwrap_or(DEFAULT_TIMEOUT) {
                                cwriteln!(
                                    context.stream(),
                                    fg = Color::Red,
                                    "Background process took too long to finish."
                                );
                                return Err(ScriptRunError::BackgroundProcessTookTooLong);
                            }
                        };
                        for result in results {
                            cwrite!(context.stream(), dimmed = true, "(background) ");
                            for line in result.command.command.split('\n') {
                                cwriteln!(context.stream(), fg = Color::Green, "{}", line);
                            }
                            cwriteln_rule!(
                                context.stream(),
                                fg = Color::Cyan,
                                "{}",
                                result.command.location
                            );
                            for line in &result.output {
                                cwriteln!(context.stream(), "{}", line);
                            }
                            if result.output.is_empty() {
                                cwriteln!(context.stream(), dimmed = true, "(no output)");
                            }
                            cwriteln_rule!(context.stream());
                            result.evaluate(context)?;
                        }
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
            let initial_pwd = context.envs["INITIAL_PWD"].clone();
            cwriteln!(
                context.stream(),
                fg = Color::Red,
                "$PWD {pwd:?} doesn't exist, returning to $INITIAL_PWD {initial_pwd:?}. Run `cd $INITIAL_PWD` to fix.",
            );
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
            ScriptBlock::Retry(blocks) => {
                let start = Instant::now();
                let mut backoff = Duration::from_millis(100);

                cwrite!(context.stream(), fg = Color::Green, "retry: ");
                cwriteln!(context.stream(), "running...");

                loop {
                    let mut nested_context = context.new_background();
                    match Self::run_blocks(&mut nested_context, blocks) {
                        Ok(results) => {
                            let mut all_ok = true;
                            for result in results {
                                if !result.evaluate(&mut nested_context).is_ok() {
                                    all_ok = false;
                                    break;
                                }
                            }
                            if all_ok {
                                let output = nested_context.take_output();
                                cwrite!(context.stream(), fg = Color::Green, "retry: ");
                                cwriteln!(context.stream(), "success");
                                cwriteln!(context.stream());
                                cwriteln!(context.stream(), "{output}");
                                return Ok(vec![]);
                            }
                        }
                        Err(_) => {}
                    }

                    if start.elapsed() > context.args.timeout.unwrap_or(DEFAULT_TIMEOUT) {
                        let output = nested_context.take_output();
                        cwrite!(context.stream(), fg = Color::Green, "retry: ");
                        cwriteln!(context.stream(), fg = Color::Red, "timed out");
                        cwriteln!(context.stream());
                        cwriteln!(context.stream(), "{output}");
                        cwriteln_rule!(context.stream());
                        return Err(ScriptRunError::RetryTookTooLong);
                    }
                    std::thread::sleep(backoff);
                    backoff *= 2;
                }
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
            ScriptBlock::Retry(blocks) => {
                let mut ser = serializer.serialize_map(Some(1))?;
                ser.serialize_entry("retry", blocks)?;
                ser.end()
            }
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
                cwrite!(context.stream(), fg = Color::Yellow, "using tempdir: ");
                cwriteln!(context.stream(), "{}", tempdir.path().to_string_lossy());
                cwriteln!(context.stream());
                context.envs.insert(
                    "PWD".to_string(),
                    tempdir.path().to_string_lossy().to_string(),
                );
                Ok(Some(Box::new(move |context: &mut ScriptRunContext| {
                    cwriteln!(
                        context.stream(),
                        fg = Color::Yellow,
                        "removing {} && cd {}",
                        tempdir.path().to_string_lossy(),
                        current_pwd.to_string_lossy()
                    );
                    cwriteln!(context.stream());
                    std::fs::remove_dir_all(&tempdir)?;
                    drop(tempdir);
                    Ok::<_, ScriptRunError>(())
                })))
            }
            InternalCommand::UsingDir(dir, new) => {
                let current_pwd = context.pwd();
                let dir = context.expand(dir)?;
                let new_pwd = current_pwd.join(dir);
                if new {
                    cwrite!(context.stream(), fg = Color::Yellow, "using new dir: ");
                } else {
                    cwrite!(context.stream(), fg = Color::Yellow, "using dir: ");
                }
                cwriteln!(context.stream(), "{}", new_pwd.to_string_lossy());
                cwriteln!(context.stream());

                if new {
                    std::fs::create_dir_all(&new_pwd)?;
                } else if !new_pwd.exists() {
                    return Err(ScriptRunError::IO(std::io::Error::new(
                        std::io::ErrorKind::NotFound,
                        "directory does not exist",
                    )));
                }
                context
                    .envs
                    .insert("PWD".to_string(), new_pwd.to_string_lossy().to_string());
                Ok(Some(Box::new(move |context: &mut ScriptRunContext| {
                    if new {
                        cwriteln!(
                            context.stream(),
                            fg = Color::Yellow,
                            "removing {} && cd {}",
                            new_pwd.to_string_lossy(),
                            current_pwd.to_string_lossy()
                        );
                        cwriteln!(context.stream());
                    } else {
                        cwriteln!(
                            context.stream(),
                            fg = Color::Yellow,
                            "cd {}",
                            current_pwd.to_string_lossy()
                        );
                        cwriteln!(context.stream());
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

                cwriteln!(context.stream(), fg = Color::Yellow, "cd {dir}");
                cwriteln!(context.stream());
                let current_pwd = context.pwd();
                let new_pwd = current_pwd.join(dir);
                context
                    .envs
                    .insert("PWD".to_string(), new_pwd.to_string_lossy().to_string());
                Ok(None)
            }
            InternalCommand::Set(name, value) => {
                let value = context.expand(value)?;

                cwriteln!(context.stream(), fg = Color::Yellow, "set {name} {value}");
                cwriteln!(context.stream());

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
        use crate::{cwriteln_rule, term::Color};

        let command = &self.command;
        let args = &context.args;

        for line in command.command.split('\n') {
            cwriteln!(context.stream(), fg = Color::Green, "{}", line);
        }
        cwriteln_rule!(context.stream(), fg = Color::Cyan, "{}", command.location);
        let (output, status) = command.run(
            &mut context.stream(),
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

        let match_context = OutputMatchContext::new(context);
        let pattern_result = match self.pattern.matches(match_context.clone(), output.clone()) {
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
                    for line in match_context.traces() {
                        trace.push_str(&format!("{line}\n"));
                    }
                    PatternResult::Mismatch(e, trace)
                }
            }
        };

        if output.is_empty() {
            cwriteln!(context.stream(), dimmed = true, "(no output)");
        }
        cwriteln_rule!(context.stream());

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
                cwriteln!(
                    context.stream(),
                    fg = Color::Red,
                    "{failure} FAIL: {status}"
                );
                cwriteln!(
                    context.stream(),
                    dimmed = true,
                    " {arrow} {}",
                    self.command.command
                );
                cwriteln!(context.stream());
                return Err(ScriptRunError::Exit(status));
            }
        }

        if let PatternResult::Mismatch(e, trace) = &self.pattern {
            if !args.ignore_matches {
                cwriteln!(context.stream(), fg = Color::Red, "ERROR: {e}");
                cwriteln!(context.stream(), dimmed = true, "{trace}");
                cwriteln!(context.stream(), fg = Color::Red, "{failure} FAIL");
                cwriteln!(context.stream());
                return Err(ScriptRunError::Pattern(e.clone()));
            }
        }

        if let PatternResult::ExpectedFailure = self.pattern {
            if !args.ignore_matches {
                cwriteln!(
                    context.stream(),
                    fg = Color::Red,
                    "{failure} FAIL (output shouldn't match)"
                );
                cwriteln!(
                    context.stream(),
                    dimmed = true,
                    " {arrow} {}",
                    self.command.command
                );
                cwriteln!(context.stream());
                return Err(ScriptRunError::ExpectedFailure);
            }
        }

        if let ExitResult::Matches(status) = self.exit {
            if status.success() {
                cwriteln!(context.stream(), fg = Color::Green, "{success} OK");
            } else {
                cwriteln!(
                    context.stream(),
                    fg = Color::Green,
                    "{success} OK ({status})"
                );
            }
            cwriteln!(context.stream());
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
        let mut context = ScriptRunContext::new(ScriptRunArgs::default(), &Path::new("."));
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
