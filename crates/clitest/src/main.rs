#![doc = include_str!("../README.md")]

use clap::{Parser, ValueEnum};
use std::{path::PathBuf, time::Duration};
use termcolor::Color;

use clitest_lib::script::ScriptOutput;
use clitest_lib::*;

use parser::parse_script;
use script::{Script, ScriptFile, ScriptRunArgs};
#[derive(Parser, Debug, Clone, Copy, ValueEnum)]
enum DumpFormat {
    Json,
    Toml,
    Yaml,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to the script to run.
    #[arg(value_hint = clap::ValueHint::FilePath)]
    scripts: Vec<PathBuf>,

    /// Delay between steps.
    #[arg(long)]
    delay_steps: Option<u64>,

    /// Ignore mismatched exit codes.
    #[arg(long)]
    ignore_exit_codes: bool,

    /// Ignore failed matches.
    #[arg(long)]
    ignore_matches: bool,

    /// Quiet.
    #[arg(long, short)]
    quiet: bool,

    /// Verbose output
    #[arg(long, short)]
    verbose: bool,

    /// Command timeout in seconds.
    #[arg(long)]
    timeout: Option<f32>,

    /// Show line numbers in command output.
    #[arg(long)]
    show_line_numbers: bool,

    /// The command to run the script with. Default is 'sh -c'.
    #[arg(long)]
    runner: Option<String>,

    /// Dump the script to JSON or TOML.
    #[arg(long)]
    dump: Option<DumpFormat>,

    /// Version (used for shebang only)
    #[arg(long, hide = true)]
    v0: bool,
}

struct ScriptToRun {
    original_path: ScriptFile,
    canonical_path: PathBuf,
    script: Script,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let script_files = args
        .scripts
        .iter()
        .map(|path| {
            let canonical_path = dunce::canonicalize(path)?;
            let script_file = ScriptFile::new(path.clone());
            let script = parse_script(script_file.clone(), &std::fs::read_to_string(path)?)?;
            Ok(ScriptToRun {
                original_path: script_file.clone(),
                canonical_path,
                script,
            })
        })
        .collect::<Result<Vec<_>, Box<dyn std::error::Error>>>();

    let script_files = match script_files {
        Ok(s) => s,
        Err(e) => {
            cprint!(fg = Color::Red, "Error:");
            cprintln!(" {e}");
            std::process::exit(1);
        }
    };

    if let Some(format) = args.dump {
        let s = script_files
            .into_iter()
            .map(|s| s.script)
            .collect::<Vec<_>>();
        match format {
            DumpFormat::Json => {
                cprintln!(
                    "{}",
                    serde_json::to_string_pretty(&s).expect("Failed to serialize script files")
                );
            }
            DumpFormat::Yaml => {
                for script in s {
                    cprintln!(
                        "{}",
                        serde_yaml::to_string(&script).expect("Failed to serialize script files")
                    );
                }
            }
            DumpFormat::Toml => {
                for script in s {
                    cprintln!(
                        "{}",
                        toml::to_string_pretty(&script).expect("Failed to serialize script files")
                    );
                }
            }
        }
        return Ok(());
    }

    let mut failed = 0;
    let total = script_files.len();

    for script in script_files {
        let args = ScriptRunArgs {
            delay_steps: args.delay_steps,
            ignore_exit_codes: args.ignore_exit_codes,
            ignore_matches: args.ignore_matches,
            runner: args.runner.clone(),
            show_line_numbers: args.show_line_numbers,
            quiet: args.quiet,
            no_color: false,
            verbose: args.verbose,
            timeout: args.timeout.map(Duration::from_secs_f32),
            simplified_output: false,
        };

        if args.quiet {
            cprint!("Running ");
            cprint!(fg = Color::Cyan, "{} ... ", script.original_path);
            match script.script.run_with_args(args, ScriptOutput::quiet(true)) {
                Ok(_) => cprintln!(fg = Color::Green, "OK"),
                Err(e) => {
                    cprintln!(fg = Color::Red, "FAILED");
                    failed += 1;
                    cprint!(fg = Color::Red, "Error: ");
                    cprintln!("{e}");
                }
            }
        } else if script
            .script
            .run_with_args(args, ScriptOutput::default())
            .is_err()
        {
            failed += 1;
        }
    }

    if failed > 0 {
        cprintln!(fg = Color::Red, "{failed}/{total} test(s) failed");
        std::process::exit(1);
    } else {
        cprintln!(fg = Color::Green, "{total} test(s) passed");
    }

    Ok(())
}
