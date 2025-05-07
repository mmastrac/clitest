use std::collections::HashMap;

use clitest::{
    cprint, cprintln,
    parser::parse_script,
    script::{ScriptFile, ScriptRunArgs, ScriptRunContext},
    term::{self, Color},
};

fn main() {
    term::ensure_panic_hook();

    let mut total = 0;
    let mut failed = 0;
    cprintln!();

    let tests = clitest::testing::load_test_scripts(std::env::args().nth(1).as_deref());

    for test in tests {
        cprint!("Running ");
        cprint!(fg = Color::Green, "{}", test.name);
        cprint!(" ... ");

        let script = parse_script(ScriptFile::new(test.path), &test.content).unwrap();
        total += 1;
        let args = ScriptRunArgs {
            delay_steps: None,
            ignore_exit_codes: false,
            ignore_matches: false,
            quiet: true,
            runner: None,
            show_line_numbers: true,
        };
        let mut context = ScriptRunContext {
            args,
            envs: HashMap::new(),
        };
        if let Err(e) = script.run(&mut context) {
            cprintln!(fg = Color::Red, "❌ FAIL");
            failed += 1;
            println!("{}", e);
        } else {
            cprintln!(fg = Color::Green, "✅ OK");
        }
    }

    cprintln!();
    cprintln!(
        fg = if failed > 0 { Color::Red } else { Color::White },
        dimmed = true,
        "{} tests run, {} failed",
        total,
        failed
    );
    cprintln!();
}
