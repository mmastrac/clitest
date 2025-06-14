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

    let tmp = std::env::temp_dir()
        .canonicalize()
        .unwrap()
        .display()
        .to_string();

    let apple_path = tmp.strip_prefix("/private");
    let tmps = if tmp != "/tmp" {
        if let Some(apple_path) = apple_path {
            vec![apple_path, &tmp, "/tmp"]
        } else {
            vec![&tmp, "/tmp"]
        }
    } else {
        vec![tmp.as_str()]
    };

    // Replace any line that starts with "───" with "---"
    let mut output = String::new();
    for line in s.lines() {
        munge_line(&root, &tmps, &mut output, line);
    }
    output
}

fn munge_line(root: &String, tmp: &[&str], output: &mut String, line: &str) {
    if line.starts_with("───") {
        output.push_str("---\n");
    } else if line.contains("<ignore>") {
        output.push_str("<ignore>\n");
    } else {
        let line = line.replace(root, "<root>");
        for tmp in tmp {
            if line.contains(tmp) {
                munge_tmp(tmp, output, &line);
                output.push('\n');
                return;
            }
        }
        output.push_str(&line);
        output.push('\n');
    }
}

fn munge_tmp(tmp: &str, output: &mut String, line: &String) {
    let tmp_char = |c: char| !c.is_alphanumeric() && c != '_' && c != '-' && c != '.';

    // Replace /tmp or /tmp/<filename> with <tmp>
    let tmp_path = line.split_once(tmp).unwrap().1;
    let tmp_path = if tmp_path.is_empty() || tmp_path.chars().nth(0).unwrap() != '/' {
        None
    } else {
        tmp_path[1..].split(tmp_char).nth(0)
    };

    if let Some(tmp_path) = tmp_path {
        output.push_str(
            line.replace(format!("{tmp}/{tmp_path}").as_str(), "<tmp>")
                .as_str(),
        );
    } else {
        output.push_str(line.replace(tmp, "<tmp>").as_str());
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_munge_output() {
        let root = "/root".to_string();
        let tmp = "/tmp".to_string();

        let mut output = String::new();
        munge_line(&root, &tmp, &mut output, "This is /tmp");
        assert_eq!(output, "This is <tmp>\n");

        let mut output = String::new();
        munge_line(&root, &tmp, &mut output, "This is /tmp!");
        assert_eq!(output, "This is <tmp>!\n");

        let mut output = String::new();
        munge_line(&root, &tmp, &mut output, "This is /tmp/foo");
        assert_eq!(output, "This is <tmp>\n");

        let mut output = String::new();
        munge_line(&root, &tmp, &mut output, "This is /tmp/foo!");
        assert_eq!(output, "This is <tmp>!\n");
    }
}
