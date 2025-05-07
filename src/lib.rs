#![doc = include_str!("../README.md")]

pub mod command;
pub mod parser;
pub mod script;
pub mod term;

#[cfg(any(test, feature = "__testing"))]
pub mod testing {
    use std::path::PathBuf;

    pub struct TestCase {
        pub name: String,
        pub content: String,
        pub path: PathBuf,
    }

    pub fn load_test_scripts(pattern: Option<&str>) -> Vec<TestCase> {
        use std::path::Path;

        let mut scripts = Vec::new();
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
                if !test_name.ends_with(".cli") {
                    continue;
                }
                if let Some(pattern) = pattern {
                    if !format!("{test_dir_name}/{test_name}").contains(pattern) {
                        continue;
                    }
                }

                let test_content = std::fs::read_to_string(test.path()).unwrap();
                scripts.push(TestCase {
                    name: format!("{test_dir_name}/{test_name}"),
                    content: test_content,
                    path: test.path(),
                });
            }
        }

        scripts.sort_by_key(|test| test.name.clone());

        scripts
    }
}
