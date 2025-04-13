use std::{
    collections::HashMap,
    io::{BufRead, BufReader},
    process::{Command, ExitStatus, Stdio},
    sync::{Arc, Mutex},
    thread,
};

use termcolor::Color;

use crate::{cprintln, script::Lines};

#[derive(Debug)]
pub struct CommandLine {
    pub command: String,
}

impl CommandLine {
    pub fn new(command: String) -> Self {
        Self { command }
    }

    pub fn run(
        &self,
        quiet: bool,
        envs: &HashMap<String, String>,
    ) -> Result<(Lines, ExitStatus), std::io::Error> {
        let mut command = Command::new("sh");
        command.arg("-c");
        command.arg(&self.command);
        command.envs(envs);
        command.stdout(Stdio::piped());
        command.stderr(Stdio::piped());
        let mut output = command.spawn()?;
        let output_lines = Arc::new(Mutex::new(Vec::new()));

        // Spawn a thread for stdout and stderr and collect each line we read into a buffer
        let stdout_lines = output_lines.clone();
        let stdout = output.stdout.take().unwrap();
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
                if !quiet {
                    println!("{line}");
                }
                stdout_lines.lock().unwrap().push(std::mem::take(&mut line));
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
                if !quiet {
                    cprintln!(fg = Color::Yellow, "{line}");
                }
                stderr_lines.lock().unwrap().push(std::mem::take(&mut line));
            }
        });

        let res = output.wait()?;
        stdout.join().map_err(|_| {
            std::io::Error::new(std::io::ErrorKind::Other, "stdout thread panicked")
        })?;
        stderr.join().map_err(|_| {
            std::io::Error::new(std::io::ErrorKind::Other, "stderr thread panicked")
        })?;

        let output_lines = Arc::try_unwrap(output_lines).map_err(|_| {
            std::io::Error::new(std::io::ErrorKind::Other, "output lines still locked")
        })?;

        Ok((Lines::new(output_lines.into_inner().unwrap()), res))
    }
}
