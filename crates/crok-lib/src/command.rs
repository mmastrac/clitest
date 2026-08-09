use std::{
    collections::HashMap,
    process::{Command, ExitStatus},
    time::{Duration, Instant},
};

use procstream::{Capture, CommandJobExt, Event, Signal, Stream};
use serde::Serialize;
use shellish_parse::ParseOptions;
use termcolor::Color;

use crate::{
    cwrite, cwriteln,
    output::Lines,
    script::{ScriptKillReceiver, ScriptKillSender, ScriptLocation},
};

#[derive(Copy, Clone, derive_more::Debug, PartialEq, Eq)]
pub enum CommandResult {
    #[debug("{_0:?}")]
    Exit(ExitStatus, bool),
    #[debug("timed out")]
    TimedOut,
}

impl CommandResult {
    pub fn success(&self) -> bool {
        match self {
            CommandResult::Exit(status, _) => status.success(),
            CommandResult::TimedOut => false,
        }
    }
}

impl std::fmt::Display for CommandResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandResult::Exit(status, killed) => {
                if *killed {
                    write!(f, "killed")?;
                    // On Unix the status also names the signal.
                    #[cfg(unix)]
                    {
                        use std::os::unix::process::ExitStatusExt;
                        if status.signal().is_some() {
                            write!(f, "; {status}")?;
                        }
                    }
                    Ok(())
                } else {
                    write!(f, "{status}")
                }
            }
            CommandResult::TimedOut => write!(f, "timed out"),
        }
    }
}

#[derive(Clone, Debug, Serialize)]
#[serde(transparent)]
pub struct CommandLine {
    pub command: String,
    #[serde(skip)]
    pub location: ScriptLocation,
    #[serde(skip)]
    pub line_count: usize,
}

impl CommandLine {
    pub fn new(command: String, location: ScriptLocation, line_count: usize) -> Self {
        Self {
            command,
            location,
            line_count,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn run(
        &self,
        writer: &mut dyn termcolor::WriteColor,
        show_line_numbers: bool,
        runner: Option<String>,
        timeout: Duration,
        envs: &HashMap<String, String>,
        kill_receiver: &ScriptKillReceiver,
        kill_sender: &ScriptKillSender,
    ) -> Result<(Lines, CommandResult), std::io::Error> {
        let start = Instant::now();
        let warn_time = timeout.saturating_mul(90) / 100;
        let timeout = timeout.saturating_mul(110) / 100;

        let mut command = if let Some(runner) = runner {
            let bits = shellish_parse::parse(&runner, ParseOptions::default())
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidInput, e))?;
            let mut cmd = Command::new(&bits[0]);
            cmd.args(&bits[1..]);
            // A runner may cross a process or host boundary (ssh, docker exec)
            // where our environment doesn't follow so we inline them.
            cmd.arg(inline_env_command(&self.command, envs));
            cmd
        } else {
            let mut cmd = Command::new("sh");
            cmd.arg("-c");
            cmd.arg(&self.command);
            cmd
        };
        command.envs(envs);
        if let Some(pwd) = envs.get("PWD") {
            command.current_dir(pwd);
        }

        // Spawn into an isolated job (a new process group / Job object) with each
        // line of stdout and stderr delivered as a chunk.
        let (mut child, output) = command.spawn_job(Capture::lines())?;

        let job = child.job().clone();

        // Watch the script-wide kill flag and bring the whole tree down if it is
        // set, while we consume the command's output on this thread. Terminate
        // gracefully, then hard-kill anything that ignores it.
        let result = kill_receiver.run_with(
            || _ = job.shutdown(Signal::Terminate, Duration::from_millis(250)),
            move || {
                let mut line_number = 1;
                let mut output_lines = vec![];
                let mut warned = false;

                // The framer delivers whole lines: it truncates an over-long
                // line to one Overlong-tagged line and strips a bare trailing
                // CR at end of stream, so nothing to stitch or trim here.
                let mut push_line = |stream: Stream, line: String| {
                    if show_line_numbers {
                        cwrite!(
                            writer,
                            fg = Color::White,
                            dimmed = true,
                            "{line_number:>3} "
                        );
                    }

                    let line_out = fast_strip_ansi::strip_ansi_string(&line);
                    if stream == Stream::Stdout {
                        cwriteln!(writer, fg = Color::White, "{line_out}");
                    } else {
                        cwriteln!(writer, fg = Color::Yellow, "{line_out}");
                    }

                    output_lines.push(line);
                    line_number += 1;
                };

                loop {
                    // Check the deadline every pass, so a silent command cannot
                    // outrun it while we wait for output or exit.
                    if start.elapsed() >= timeout {
                        cwriteln!(writer, fg = Color::Yellow, "Process took too long!");
                        kill_sender.kill();
                        _ = child.shutdown(Signal::Terminate, Duration::from_millis(250));
                        return Ok((Lines::new(output_lines), CommandResult::TimedOut));
                    }

                    // Wake at the warning threshold (once), then again at the hard
                    // timeout, even if the command is producing no output.
                    let remaining = timeout.saturating_sub(start.elapsed());
                    let wait = if warned {
                        remaining
                    } else {
                        remaining.min(warn_time.saturating_sub(start.elapsed()))
                    };

                    match output.recv_timeout(wait) {
                        Ok(Event::Chunk(chunk)) => {
                            let stream = chunk.stream;
                            // Move the bytes into a String, copying only when
                            // invalid UTF-8 forces a lossy pass.
                            let line = String::from_utf8(chunk.item.bytes).unwrap_or_else(|e| {
                                String::from_utf8_lossy(e.as_bytes()).into_owned()
                            });
                            push_line(stream, line);
                        }
                        // The leader may exit before we have drained every chunk.
                        Ok(Event::Exit(_)) => {}
                        Err(procstream::RecvTimeout::Closed) => break,
                        Err(procstream::RecvTimeout::Timeout) => {
                            if !warned && start.elapsed() < timeout {
                                eprintln!("Process #{} taking too long to finish.", child.id());
                                warned = true;
                            }
                        }
                    }
                }

                let status = child.wait()?;
                Ok((Lines::new(output_lines), CommandResult::Exit(status, false)))
            },
        );

        // `run_with` has joined the kill watcher, so read the flag here rather
        // than in the closure, where it would race the watcher that sets it.
        match result {
            Ok((lines, CommandResult::Exit(status, _))) => {
                Ok((lines, CommandResult::Exit(status, job.terminated())))
            }
            other => other,
        }
    }
}

/// Vars crok manages itself: PWD/INITIAL_PWD drive the runner's working
/// directory, and TARGET_* exist for script conditionals, not for commands.
const SPECIAL_VARS: &[&str] = &[
    "PWD",
    "INITIAL_PWD",
    "TARGET_OS",
    "TARGET_FAMILY",
    "TARGET_ARCH",
];

/// Rewrite `command` as `env K='v' ... sh -c 'command'` so a runner that crosses a
/// process or host boundary still delivers them.
fn inline_env_command(command: &str, envs: &HashMap<String, String>) -> String {
    let mut vars: Vec<_> = envs
        .iter()
        .filter(|(k, _)| !SPECIAL_VARS.contains(&k.as_str()))
        .collect();
    if vars.is_empty() {
        return command.to_string();
    }
    vars.sort();

    let mut out = String::from("env");
    for (key, value) in vars {
        out.push(' ');
        out.push_str(key);
        out.push('=');
        out.push_str(&sh_quote(value));
    }
    // Wrap in `sh -c` so the env applies to the whole command, not just the
    // first segment of a pipe or `&&` chain.
    out.push_str(" sh -c ");
    out.push_str(&sh_quote(command));
    out
}

/// POSIX quoting: keep the generated command printable.
fn sh_quote(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    // Current single-quoted run, opened lazily so control-char splices sit
    // between runs rather than inside one.
    let mut open = false;
    for c in s.chars() {
        if c.is_ascii_control() && c != '\n' || c == '\x7f' {
            if open {
                out.push('\'');
                open = false;
            }
            out.push_str(&format!("\"$(printf '\\{:03o}')\"", c as u32));
        } else {
            if !open {
                out.push('\'');
                open = true;
            }
            if c == '\'' {
                out.push_str("'\\''");
            } else {
                out.push(c);
            }
        }
    }
    if open {
        out.push('\'');
    }
    if out.is_empty() {
        out.push_str("''");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sh_quote() {
        assert_eq!(sh_quote("plain"), "'plain'");
        assert_eq!(sh_quote(""), "''");
        assert_eq!(sh_quote("a b $c `d`"), "'a b $c `d`'");
        assert_eq!(sh_quote("it's"), r#"'it'\''s'"#);
    }

    #[test]
    fn test_sh_quote_control_chars() {
        assert_eq!(sh_quote("a\x01b"), r#"'a'"$(printf '\001')"'b'"#);
        assert_eq!(sh_quote("\x1b"), r#""$(printf '\033')""#);
        assert_eq!(sh_quote("a\tb"), r#"'a'"$(printf '\011')"'b'"#);
        assert_eq!(sh_quote("\x7f"), r#""$(printf '\177')""#);
        // Newline stays literal: $(printf '\n') would strip it as a trailing
        // newline of the substitution.
        assert_eq!(sh_quote("a\nb"), "'a\nb'");
        // Quote directly after a control splice.
        assert_eq!(sh_quote("\x01'x"), r#""$(printf '\001')"''\''x'"#);
    }

    /// The generated splices must round-trip through a real shell.
    #[test]
    #[cfg(unix)]
    fn test_sh_quote_shell_roundtrip() {
        for value in ["plain", "it's", "a\x01b", "a\nb", "\x1b[1m", "a\tb'\x02"] {
            let out = std::process::Command::new("sh")
                .arg("-c")
                .arg(format!("printf %s {}", sh_quote(value)))
                .output()
                .unwrap();
            assert_eq!(
                String::from_utf8_lossy(&out.stdout),
                value,
                "round-trip failed for {value:?}"
            );
        }
    }

    #[test]
    fn test_inline_env_command_empty() {
        let envs = HashMap::from([("PWD".to_string(), "/tmp".to_string())]);
        assert_eq!(inline_env_command("echo hi", &envs), "echo hi");
    }

    #[test]
    fn test_inline_env_command() {
        let envs = HashMap::from([
            ("PWD".to_string(), "/tmp".to_string()),
            ("TARGET_OS".to_string(), "freebsd".to_string()),
            ("RUSTFLAGS".to_string(), "-C opt-level=1".to_string()),
            ("B".to_string(), "it's".to_string()),
        ]);
        assert_eq!(
            inline_env_command("cargo build && cargo test", &envs),
            r#"env B='it'\''s' RUSTFLAGS='-C opt-level=1' sh -c 'cargo build && cargo test'"#
        );
    }
}
