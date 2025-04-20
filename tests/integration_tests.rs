use clitest::{
    cprint, cprintln,
    parser::parse_script,
    script::ScriptRunArgs,
    term::{self, Color},
};
use std::path::Path;

fn main() {
    term::ensure_panic_hook();

    let mut total = 0;
    let mut failed = 0;
    println!();
    let pattern = std::env::args().nth(1).unwrap_or_default();
    let crate_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    for test_dir in std::fs::read_dir(crate_root.join("tests"))
        .into_iter()
        .flatten()
        .flatten()
    {
        let test_dir_name = test_dir.file_name().to_str().unwrap().to_owned();
        for test in std::fs::read_dir(test_dir.path())
            .into_iter()
            .flatten()
            .flatten()
        {
            let test_name = test.file_name().to_str().unwrap().to_owned();
            if !format!("{test_dir_name}/{test_name}").contains(&pattern) {
                continue;
            }

            let test_content = std::fs::read_to_string(test.path()).unwrap();
            cprint!("Running ");
            cprint!(fg = Color::Green, "{test_name} ({test_dir_name})");
            cprint!(" ... ");

            let script = parse_script(&test_content).unwrap();
            total += 1;
            if let Err(e) = script.run(ScriptRunArgs {
                delay_steps: None,
                ignore_exit_codes: false,
                ignore_matches: false,
                quiet: true,
                runner: None,
            }) {
                cprintln!(fg = Color::Red, "❌ FAIL");
                failed += 1;
                println!("{}", e);
            } else {
                cprintln!(fg = Color::Green, "✅ OK");
            }
        }
    }

    println!();
    cprintln!(
        fg = if failed > 0 { Color::Red } else { Color::White },
        dimmed = true,
        "{} tests run, {} failed",
        total,
        failed
    );
    println!();
}
