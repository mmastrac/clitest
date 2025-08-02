use std::time::Instant;

use clitest_lib::{
    cprint, cprintln, cprintln_rule,
    parser::parse_script_file,
    script::{ScriptFile, ScriptOutput, ScriptRunArgs},
    term::Color,
    util::NicePathBuf,
};

use clitest_integration::testing::{TestCase, load_test_scripts, root_dir, tests_dir};

pub fn run() {
    let root = root_dir();
    std::env::set_current_dir(&root)
        .unwrap_or_else(|_| panic!("failed to set current directory to {root:?}"));

    let mut total = 0;
    let mut failed = 0;
    cprintln!();

    let tests = load_test_scripts(std::env::args().nth(1).as_deref());

    cprint!("Running ");
    cprint!(fg = Color::Yellow, "{}", tests.len());
    cprint!(" test(s) from ");
    cprint!(
        fg = Color::Cyan,
        "<workspace>/{}/",
        NicePathBuf::from(tests_dir())
    );
    cprintln!();

    let mut failed_tests = Vec::new();

    for test in tests {
        let is_fail = test.path.to_string().contains("-fail");
        cprint!("Running ");
        cprint!(fg = Color::Green, "{}", test.name);
        cprint!(" ... ");

        let script = parse_script_file(None, ScriptFile::new(&test.path)).unwrap();
        total += 1;
        let args = ScriptRunArgs {
            quiet: true,
            no_color: true,
            simplified_output: true,
            ..Default::default()
        };
        let output = ScriptOutput::quiet(true);
        let start = Instant::now();
        let res = script.run_with_args(args, output.clone());

        if is_fail {
            if let Err(e) = res {
                cprint!(fg = Color::Green, "✅ OK ({})", e.short());
                if !check_output(&test, output.take_buffer()) {
                    failed += 1;
                }
            } else {
                cprint!(fg = Color::Red, "❌ FAIL (expected a failure)");
                failed += 1;
                failed_tests.push(test);
            }
        } else if let Err(e) = res {
            cprint!(fg = Color::Red, "❌ FAIL");
            failed += 1;
            cprint!(fg = Color::Red, " {}", e);
            failed_tests.push(test);
        } else {
            cprint!(fg = Color::Green, "✅ OK");
            if !check_output(&test, output.take_buffer()) {
                failed += 1;
            }
        }
        let duration = start.elapsed();
        cprintln!(dimmed = true, " ({:.2}s)", duration.as_secs_f64());
    }

    for test in failed_tests {
        cprintln!();
        cprintln_rule!();
        cprint!("Re-running failed test ");
        cprint!(fg = Color::Green, "{}", test.name);
        cprintln!(" ... ");
        cprintln_rule!();
        let script = parse_script_file(None, ScriptFile::new(test.path.clone())).unwrap();
        let args = ScriptRunArgs {
            show_line_numbers: true,
            ..Default::default()
        };
        let _ = script.run_with_args(args, ScriptOutput::default());
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

fn main() {
    run();
}

/// Munge the output to make it easier to compare.
fn munge_output(root: &str, s: &str) -> String {
    let tmp = dunce::canonicalize(std::env::temp_dir())
        .unwrap()
        .to_str()
        .unwrap()
        .to_owned();

    let apple_path = tmp.strip_prefix("/private");
    let tmps = if tmp != "/tmp" {
        if let Some(apple_path) = apple_path {
            vec![apple_path, &tmp, "/tmp"]
        } else if cfg!(windows) {
            vec!["%TEMP%", &tmp, r"\tmp"]
        } else {
            vec![&tmp, "/tmp"]
        }
    } else {
        vec![tmp.as_str()]
    };

    // Replace any line that starts with "───" with "---"
    let mut output = String::new();
    // On Windows, use \ for everything, then replace back to / for final test
    #[cfg(windows)]
    let s = &s.replace('/', r"\");
    #[cfg(windows)]
    let s = s.replace(r"\\?\", "");
    for line in s.lines() {
        munge_line(root, &tmps, &mut output, line);
    }
    if cfg!(windows) {
        output = output.replace("\\", "/");
    }
    output
}

fn munge_line(root: &str, tmp: &[&str], output: &mut String, line: &str) {
    // Windows/Unix differs here
    #[cfg(windows)]
    let line = line.replace("exit code", "exit status");
    // Background processes don't get a signal report
    #[cfg(windows)]
    let line = line.replace(" (signal: 1 (SIGHUP))", "");

    if line.contains("<ignore>") {
        output.push_str("<ignore>\n");
    } else {
        let line = line.replace(
            "~/",
            &format!("{}/", std::env::var("HOME").unwrap_or_default()),
        );
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
    let sep = if cfg!(windows) { '\\' } else { '/' };

    // Replace /tmp or /tmp/<filename> with <tmp>
    let tmp_path = line.split_once(tmp).unwrap().1;
    let tmp_path = if tmp_path.is_empty() || tmp_path.chars().next().unwrap() != sep {
        None
    } else {
        tmp_path[1..].split(tmp_char).next()
    };

    if let Some(tmp_path) = tmp_path {
        output.push_str(
            line.replace(format!("{tmp}{sep}{tmp_path}").as_str(), "<tmp>")
                .as_str(),
        );
    } else {
        output.push_str(line.replace(tmp, "<tmp>").as_str());
    }
}

fn check_output(test: &TestCase, output: String) -> bool {
    let root = &NicePathBuf::from(test.path.as_ref().parent().unwrap()).to_string();
    let b = munge_output(root, &output);

    if std::env::var("UPDATE_TESTS").is_ok() {
        if let Some(expected_output_file) = &test.expected_output_file {
            std::fs::write(expected_output_file, &b).unwrap();
        }
    }

    if let Some(expected_output) = &test.expected_output {
        let a = munge_output(root, expected_output);
        if a == b {
            return true;
        }
        cprintln!(fg = Color::Red, "⚠️  Contents differ for {}!", test.path);
        cprintln_rule!();
        let comparison = pretty_assertions::StrComparison::new(&a, &b);
        cprintln!("{}", comparison);
        cprintln_rule!();
        cprintln!("\nOriginal output before munge:");
        cprintln_rule!();
        cprintln!("{}", output);
        cprintln_rule!();
        false
    } else {
        true
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_munge_output() {
        let root = "/root".to_string();
        let tmp = ["/tmp"];

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
