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
    cprint, cprintln,
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
        quiet: bool,
        show_line_numbers: bool,
        runner: Option<String>,
        envs: &HashMap<String, String>,
        kill_receiver: &ScriptKillReceiver,
    ) -> Result<(Lines, ExitStatus), std::io::Error> {
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
        command.stdout(Stdio::piped());
        command.stderr(Stdio::piped());
        command.stdin(Stdio::null());
        let mut output = command.spawn().map_err(|e| {
            std::io::Error::new(
                e.kind(),
                format!("failed to spawn command {command:?}: {e}"),
            )
        })?;
        let output_lines = Arc::new(Mutex::new(Vec::new()));
        let line_number = Arc::new(Mutex::new(1));

        // Spawn a thread for stdout and stderr and collect each line we read into a buffer
        let stdout_lines = output_lines.clone();
        let stdout = output.stdout.take().unwrap();
        let line_number_stdout = line_number.clone();
        let stdout = thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            let mut line = String::new();
            while reader.read_line(&mut line).unwrap() > 0 {
                if line.is_empty() {
                    continue;
                }
                if line.ends_with('\n') {
                    line.pop();
                }
                let mut line_number = line_number_stdout.lock().unwrap();
                if !quiet {
                    if show_line_numbers {
                        cprint!(fg = Color::White, dimmed = true, "{line_number:>3} ");
                    }
                    cprintln!("{line}");
                }
                stdout_lines.lock().unwrap().push(std::mem::take(&mut line));
                *line_number += 1;
            }
        });

        let stderr_lines = output_lines.clone();
        let stderr = output.stderr.take().unwrap();
        let stderr = thread::spawn(move || {
            let mut reader = BufReader::new(stderr);
            let mut line = String::new();
            while reader.read_line(&mut line).unwrap() > 0 {
                if line.is_empty() {
                    continue;
                }
                if line.ends_with('\n') {
                    line.pop();
                }
                let mut line_number = line_number.lock().unwrap();
                if !quiet {
                    if show_line_numbers {
                        cprint!(fg = Color::White, dimmed = true, "{line_number:>3} ");
                    }
                    cprintln!(fg = Color::Yellow, "{line}");
                }
                stderr_lines.lock().unwrap().push(std::mem::take(&mut line));
                *line_number += 1;
            }
        });

        let res = kill_receiver.run_cmd(output)?;
        let join_start = std::time::Instant::now();
        let mut stdout_holder = Some(stdout);
        let mut stderr_holder = Some(stderr);
        loop {
            if stdout_holder.is_none() && stderr_holder.is_none() {
                break;
            }
            if join_start.elapsed() > std::time::Duration::from_secs(30) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::TimedOut,
                    "process took too long to join",
                ));
            }

            if let Some(stdout) = stdout_holder.take() {
                if stdout.is_finished() {
                    stdout.join().map_err(|_| {
                        std::io::Error::new(std::io::ErrorKind::Other, "stdout thread panicked")
                    })?;
                } else {
                    stdout_holder = Some(stdout);
                }
            }
            if let Some(stderr) = stderr_holder.take() {
                if stderr.is_finished() {
                    stderr.join().map_err(|_| {
                        std::io::Error::new(std::io::ErrorKind::Other, "stderr thread panicked")
                    })?;
                } else {
                    stderr_holder = Some(stderr);
                }
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        let output_lines = Arc::try_unwrap(output_lines).map_err(|_| {
            std::io::Error::new(std::io::ErrorKind::Other, "output lines still locked")
        })?;

        Ok((Lines::new(output_lines.into_inner().unwrap()), res))
    }
}
