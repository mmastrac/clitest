use clap::Parser;
use parser::v0::parse_script;
use script::ScriptRunArgs;
use std::path::PathBuf;

mod command;
mod parser;
mod script;
mod term;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to the script to run
    #[arg(long, value_hint = clap::ValueHint::FilePath)]
    script: PathBuf,

    /// Delay between steps
    #[arg(long)]
    delay_steps: Option<u64>,

    /// Regenerate test data
    #[arg(long)]
    regenerate: bool,

    /// Explain the script
    #[arg(long)]
    explain: bool,

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let script_file = args.script;
    let script = std::fs::read_to_string(&script_file).expect("failed to read script");

    let canonical_dir = script_file
        .canonicalize()
        .expect("failed to canonicalize script file");
    let script_dir = canonical_dir
        .parent()
        .expect("failed to get script directory");
    std::env::set_current_dir(script_dir).expect("failed to set current directory");

    let script = parse_script(&script).expect("failed to parse script");

    let args = ScriptRunArgs {
        delay_steps: args.delay_steps,
        regenerate: args.regenerate,
        explain: args.explain,
        ignore_exit_codes: args.ignore_exit_codes,
        ignore_matches: args.ignore_matches,
        quiet: args.quiet,
    };

    script.run(args)?;
    Ok(())
}
