use std::{
    collections::HashMap,
    io::{BufRead, BufReader},
    process::{Command, ExitStatus, Stdio},
    sync::{Arc, Mutex},
    thread,
};

use serde::Serialize;
use shellish_parse::ParseOptions;
use termcolor::Color;

use crate::{
    cprint, cprintln, cwrite, cwriteln,
    script::{Lines, ScriptKillReceiver, ScriptLocation},
};

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

    pub fn run(
        &self,
        writer: &mut dyn termcolor::WriteColor,
        show_line_numbers: bool,
        runner: Option<String>,
        envs: &HashMap<String, String>,
        kill_receiver: &ScriptKillReceiver,
    ) -> Result<(Lines, ExitStatus), std::io::Error> {
        thread::scope(|s| {
            let mut command = if let Some(runner) = runner {
                let bits = shellish_parse::parse(&runner, ParseOptions::default())
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidInput, e))?;
                let mut cmd = Command::new(&bits[0]);
                cmd.args(&bits[1..]);
                cmd
            } else {
                let mut cmd = Command::new("sh");
                cmd.arg("-c");
                cmd
            };
            command.arg(&self.command);
            command.envs(envs);
            if let Some(pwd) = envs.get("PWD") {
                command.current_dir(pwd);
            }
            #[cfg(unix)]
            {
                use std::os::unix::process::CommandExt;
                command.process_group(0);
            }
            #[cfg(windows)]
            {
                use std::os::windows::process::CommandExt;
                const CREATE_SUSPENDED: u32 = 0x00000004;
                command.creation_flags(CREATE_SUSPENDED);
            }
            command.stdout(Stdio::piped());
            command.stderr(Stdio::piped());
            command.stdin(Stdio::null());
            let mut output = command.spawn().map_err(|e| {
                std::io::Error::new(
                    e.kind(),
                    format!("failed to spawn command {command:?}: {e}"),
                )
            })?;

            let (tx, rx) = std::sync::mpsc::channel();

            // Spawn a thread for stdout and stderr and collect each line we read into a buffer
            let stdout_lines = tx.clone();
            let stdout = output.stdout.take().unwrap();
            let stdout = s.spawn(move || {
                let mut reader = BufReader::new(stdout);
                let mut line = String::new();
                while reader.read_line(&mut line).unwrap() > 0 {
                    if line.is_empty() {
                        continue;
                    }
                    if line.ends_with('\n') {
                        line.pop();
                    }
                    _ = stdout_lines.send((true, std::mem::take(&mut line)));
                }
            });

            let stderr_lines = tx;
            let stderr = output.stderr.take().unwrap();
            let stderr = s.spawn(move || {
                let mut reader = BufReader::new(stderr);
                let mut line = String::new();
                while reader.read_line(&mut line).unwrap() > 0 {
                    if line.is_empty() {
                        continue;
                    }
                    if line.ends_with('\n') {
                        line.pop();
                    }
                    _ = stderr_lines.send((false, std::mem::take(&mut line)));
                }
            });

            let runner = s.spawn(move || kill_receiver.run_cmd(output));

            let mut line_number = 1;
            let mut output_lines = vec![];
            while let Ok((is_stdout, line)) = rx.recv() {
                if show_line_numbers {
                    cwrite!(
                        writer,
                        fg = Color::White,
                        dimmed = true,
                        "{line_number:>3} "
                    );
                }
                if is_stdout {
                    cwriteln!(writer, fg = Color::White, "{line}");
                } else {
                    cwriteln!(writer, fg = Color::Yellow, "{line}");
                }

                output_lines.push(line);
                line_number += 1;
            }

            let join_start = std::time::Instant::now();
            let mut handles = vec![stdout, stderr];
            while !handles.is_empty() {
                if join_start.elapsed() > std::time::Duration::from_secs(30) {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::TimedOut,
                        "process took too long to join",
                    ));
                }

                let mut new_handles = vec![];
                for handle in handles.drain(..) {
                    if handle.is_finished() {
                        handle
                            .join()
                            .map_err(|_| std::io::Error::other("thread panicked"))?;
                    } else {
                        new_handles.push(handle);
                    }
                }
                handles = new_handles;
                std::thread::sleep(std::time::Duration::from_millis(10));
            }

            Ok((Lines::new(output_lines), runner.join().unwrap()?))
        })
    }
}
