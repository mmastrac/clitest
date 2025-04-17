use clap::Parser;
use parser::v0::parse_script;
use script::{Script, ScriptRunArgs};
use std::path::PathBuf;
use termcolor::Color;

mod command;
mod parser;
mod script;
mod term;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to the script to run
    #[arg(value_hint = clap::ValueHint::FilePath)]
    scripts: Vec<PathBuf>,

    /// Delay between steps
    #[arg(long)]
    delay_steps: Option<u64>,

    /// Ignore exit codes
    #[arg(long)]
    ignore_exit_codes: bool,

    /// Ignore matches
    #[arg(long)]
    ignore_matches: bool,

    /// Quiet
    #[arg(long)]
    quiet: bool,
}

struct ScriptToRun {
    original_path: PathBuf,
    script_dir: PathBuf,
    script: Script,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
            let script = parse_script(&std::fs::read_to_string(path)?)?;
            Ok(ScriptToRun {
                original_path: path.clone(),
                script_dir,
                script,
            })
        })
        .collect::<Result<Vec<_>, Box<dyn std::error::Error>>>()?;

    let mut failed = 0;
    let total = script_files.len();

    for script in script_files {
        std::env::set_current_dir(&script.script_dir).expect("failed to set current directory");

        let args = ScriptRunArgs {
            delay_steps: args.delay_steps,
            ignore_exit_codes: args.ignore_exit_codes,
            ignore_matches: args.ignore_matches,
            quiet: args.quiet,
        };

        if args.quiet {
            cprint!(fg = Color::Cyan, "{} ... ", script.original_path.display());
            script.script.run(args)?;
            cprintln!(fg = Color::Green, "OK");
        } else {
            cprintln!(fg = Color::Cyan, "{}", script.original_path.display());
            println!();
            if script.script.run(args).is_ok() {
                cprint!(fg = Color::Cyan, "{} ", script.original_path.display());
                cprintln!(fg = Color::Green, "PASSED");
            } else {
                println!();
                cprint!(fg = Color::Cyan, "{} ", script.original_path.display());
                cprintln!(fg = Color::Red, "FAILED");
                failed += 1;
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
