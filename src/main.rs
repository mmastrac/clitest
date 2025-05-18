use clap::{Parser, ValueEnum};
use parser::parse_script;
use script::{Script, ScriptFile, ScriptRunArgs, ScriptRunContext};
use std::path::PathBuf;
use termcolor::Color;

mod command;
mod parser;
mod script;
mod term;

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
    #[arg(long)]
    quiet: bool,

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
    script_dir: PathBuf,
    script: Script,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    term::ensure_panic_hook();

    let args = Args::parse();

    let script_files = args
        .scripts
        .iter()
        .map(|path| {
            let canonical_path = path.canonicalize()?;
            let script_dir = canonical_path
                .parent()
                .ok_or("failed to get script directory")?
                .to_path_buf();
            let script_file = ScriptFile::new(path.clone());
            let script = parse_script(script_file.clone(), &std::fs::read_to_string(path)?)?;
            Ok(ScriptToRun {
                original_path: script_file.clone(),
                script_dir,
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
                println!(
                    "{}",
                    serde_json::to_string_pretty(&s).expect("Failed to serialize script files")
                );
            }
            DumpFormat::Yaml => {
                for script in s {
                    println!(
                        "{}",
                        serde_yaml::to_string(&script).expect("Failed to serialize script files")
                    );
                }
            }
            DumpFormat::Toml => {
                for script in s {
                    println!(
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
        std::env::set_current_dir(&script.script_dir).expect("failed to set current directory");

        let args = ScriptRunArgs {
            delay_steps: args.delay_steps,
            ignore_exit_codes: args.ignore_exit_codes,
            ignore_matches: args.ignore_matches,
            quiet: args.quiet,
            runner: args.runner.clone(),
            show_line_numbers: args.show_line_numbers,
        };

        let mut context = ScriptRunContext::new(args);

        if context.args.quiet {
            cprint!(fg = Color::Cyan, "{} ... ", script.original_path);
            match script.script.run(&mut context) {
                Ok(_) => cprintln!(fg = Color::Green, "OK"),
                Err(e) => {
                    cprintln!(fg = Color::Red, "FAILED");
                    failed += 1;
                    cprintln!("Error:{e}");
                }
            }
        } else {
            cprint!("Running ");
            cprint!(fg = Color::Cyan, "{}", script.original_path);
            cprintln!(" ...");
            cprintln!();
            match script.script.run(&mut context) {
                Ok(_) => {
                    cprint!(fg = Color::Cyan, "{} ", script.original_path);
                    cprintln!(fg = Color::Green, "PASSED");
                }
                Err(e) => {
                    cprint!(fg = Color::Cyan, "{} ", script.original_path);
                    cprintln!(fg = Color::Red, "FAILED");
                    failed += 1;
                    cprint!(fg = Color::Red, "Error: ");
                    cprintln!("{}", e);
                    cprintln!();
                }
            }
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
