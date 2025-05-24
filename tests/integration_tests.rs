use clitest::{
    cprint, cprintln, cprintln_rule,
    parser::parse_script,
    script::{ScriptFile, ScriptRunArgs, ScriptRunContext},
    term::{self, Color},
};

fn main() {
    let mut total = 0;
    let mut failed = 0;
    cprintln!();

    let tests = clitest::testing::load_test_scripts(std::env::args().nth(1).as_deref());
    let mut failed_tests = Vec::new();

    for test in tests {
        let is_fail = test.path.to_str().unwrap().contains("-fail");
        cprint!("Running ");
        cprint!(fg = Color::Green, "{}", test.name);
        cprint!(" ... ");

        let script = parse_script(ScriptFile::new(test.path.clone()), &test.content).unwrap();
        total += 1;
        let args = ScriptRunArgs {
            quiet: true,
            show_line_numbers: true,
            ..Default::default()
        };
        let mut context = ScriptRunContext::new(args);
        if is_fail {
            if let Err(e) = script.run(&mut context) {
                cprintln!(fg = Color::Green, "✅ OK ({})", e.short());
            } else {
                cprintln!(fg = Color::Red, "❌ FAIL (expected a failure)");
                failed += 1;
                failed_tests.push(test);
            }
        } else {
            if let Err(e) = script.run(&mut context) {
                cprintln!(fg = Color::Red, "❌ FAIL");
                failed += 1;
                cprintln!(fg = Color::Red, "{}", e);
                failed_tests.push(test);
            } else {
                cprintln!(fg = Color::Green, "✅ OK");
            }
        }
    }

    for test in failed_tests {
        cprintln!();
        cprintln_rule!();
        cprint!("Re-running failed test ");
        cprint!(fg = Color::Green, "{}", test.name);
        cprintln!(" ... ");
        cprintln_rule!();
        let script = parse_script(ScriptFile::new(test.path.clone()), &test.content).unwrap();
        let args = ScriptRunArgs {
            show_line_numbers: true,
            ..Default::default()
        };
        let mut context = ScriptRunContext::new(args);
        _ = script.run(&mut context);
        cprintln_rule!();
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

    if failed > 0 {
        std::process::exit(1);
    }
}
