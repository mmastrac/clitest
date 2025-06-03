use std::path::Path;

use clitest::{
    cprint, cprintln, cprintln_rule, cwriteln,
    parser::parse_script,
    script::{ScriptFile, ScriptRunArgs, ScriptRunContext},
    term::Color,
    testing::TestCase,
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

        let script =
            parse_script(ScriptFile::new(test.relative_path.clone()), &test.content).unwrap();
        total += 1;
        let args = ScriptRunArgs {
            quiet: true,
            no_color: true,
            ..Default::default()
        };
        let mut context = ScriptRunContext::new(args, &test.path);
        cwriteln!(
            context.stream(),
            "Running {} ...",
            test.relative_path.display()
        );
        cwriteln!(context.stream());
        let res = script.run(&mut context);
        if let Err(e) = &res {
            cwriteln!(context.stream(), "{} FAILED", test.relative_path.display());
            cwriteln!(context.stream(), "Error: {}", e);
            cwriteln!(context.stream());
            cwriteln!(context.stream(), "1/1 test(s) failed");
        } else {
            cwriteln!(context.stream(), "{} PASSED", test.relative_path.display());
            cwriteln!(context.stream(), "1 test(s) passed");
        }

        if is_fail {
            if let Err(e) = res {
                cprintln!(fg = Color::Green, "✅ OK ({})", e.short());
                if !check_output(&test, context) {
                    failed += 1;
                }
            } else {
                cprintln!(fg = Color::Red, "❌ FAIL (expected a failure)");
                failed += 1;
                failed_tests.push(test);
            }
        } else if let Err(e) = res {
            cprintln!(fg = Color::Red, "❌ FAIL");
            failed += 1;
            cprintln!(fg = Color::Red, "{}", e);
            failed_tests.push(test);
        } else {
            cprintln!(fg = Color::Green, "✅ OK");
            if !check_output(&test, context) {
                failed += 1;
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
        let mut context = ScriptRunContext::new(args, &test.path);
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

/// Munge the output to make it easier to compare.
fn munge_output(s: &str) -> String {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .canonicalize()
        .unwrap()
        .display()
        .to_string();

    // Replace any line that starts with "───" with "---"
    let mut output = String::new();
    for line in s.lines() {
        if line.starts_with("───") {
            output.push_str("---");
        } else {
            let line = line.replace(&root, "<root>");
            if line.contains("/tmp/") {
                // Replace /tmp/<filename> with <tmp>
                let tmp = line.split("/tmp/").nth(1).unwrap();
                let tmp = tmp
                    .split(|c: char| !c.is_alphanumeric() && c != '_' && c != '-' && c != '.')
                    .nth(0)
                    .unwrap();
                output.push_str(
                    line.replace(format!("/tmp/{}", tmp).as_str(), "<tmp>")
                        .as_str(),
                );
            } else {
                output.push_str(&line);
            }
        }
        output.push('\n');
    }
    output
}

fn check_output(test: &TestCase, context: ScriptRunContext) -> bool {
    let output = context.take_output();
    let b = munge_output(&output);

    if std::env::var("UPDATE_TESTS").is_ok() {
        if let Some(expected_output_file) = &test.expected_output_file {
            std::fs::write(expected_output_file, &b).unwrap();
        }
    }

    if let Some(expected_output) = &test.expected_output {
        let a = munge_output(expected_output);
        if a == b {
            return true;
        }
        cprintln!(
            fg = Color::Red,
            "⚠️  Contents differ for {}!",
            test.relative_path.display()
        );
        cprintln_rule!();
        let comparison = pretty_assertions::StrComparison::new(&a, &b);
        cprintln!("{}", comparison);
        cprintln_rule!();
        false
    } else {
        true
    }
}
