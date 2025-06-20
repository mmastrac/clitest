use clap::{Arg, ArgMatches, Command};
use clitest_lib::parser;
use clitest_lib::script::{ScriptFile, ScriptRunArgs, ScriptRunContext};
use mdbook::book::Book;
use mdbook::errors::Error;
use mdbook::preprocess::{CmdPreprocessor, Preprocessor, PreprocessorContext};
use mdbook::BookItem;
use semver::{Version, VersionReq};
use std::io;
use std::process;
use std::time::Duration;

pub fn make_app() -> Command {
    Command::new("mdbook-clitest")
        .about("A mdbook preprocessor that runs CLI tests")
        .subcommand(
            Command::new("supports")
                .arg(Arg::new("renderer").required(true))
                .about("Check whether a renderer is supported by this preprocessor"),
        )
}

struct ClitestPreprocessor;

impl ClitestPreprocessor {
    pub fn new() -> ClitestPreprocessor {
        ClitestPreprocessor
    }
}

impl Preprocessor for ClitestPreprocessor {
    fn name(&self) -> &str {
        "clitest"
    }

    fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book, Error> {
        let mut total = 0;
        let mut error = None;
        book.for_each_mut(|item| {
            let BookItem::Chapter(ch) = item else {
                return;
            };
            // Find ```bash session blocks and process them until the closing ```
            let mut in_session = false;
            let mut session_lines = Vec::new();
            let mut session_start = 0;
            for (i, line) in ch.content.lines().enumerate() {
                if line.starts_with("```bash session") {
                    in_session = true;
                    session_start = i + 1;
                    continue;
                }
                if in_session && line.starts_with("```") {
                    in_session = false;
                    let mut script = "#!/usr/bin/env clitest --v0\n".to_string();
                    script.push_str(&std::mem::take(&mut session_lines).join("\n"));
                    let script_path = ctx.root.join(ch.path.as_ref().unwrap()).to_owned();
                    let result = parser::parse_script(ScriptFile::new(script_path.clone()), &script);
                    match result {
                        Err(e) => {
                            eprintln!("failed to parse script at {script_path:?}:{session_start}: {e:?}");
                            error = Some(e.into());
                        }
                        Ok(script) => {
                            let args = ScriptRunArgs {
                                quiet: true,
                                timeout: Some(Duration::from_secs(5)),
                                ..Default::default()
                            };
                            let mut context = ScriptRunContext::new(args, script_path.as_path());
                            context.set_env("PATH".to_string(), "/usr/bin:/bin".to_string());
                            eprintln!("running script at {script_path:?}:{session_start}...");
                            match script.run(&mut context) {
                                Ok(_) => {}
                                Err(e) => {
                                    eprintln!("failed to run script at {script_path:?}:{session_start}: {e:?}");
                                    eprintln!("{}", context.take_output());
                                    error = Some(e.into());
                                }
                            }
                        },
                    }
                    total += 1;
                    continue;
                }
                if in_session {
                    session_lines.push(line);
                }
            }
        });
        eprintln!("processed {total} code blocks");
        if let Some(e) = error {
            return Err(e);
        }
        Ok(book)
    }

    fn supports_renderer(&self, renderer: &str) -> bool {
        true
    }
}

fn handle_preprocessing(pre: &dyn Preprocessor) -> Result<(), Error> {
    let (ctx, book) = CmdPreprocessor::parse_input(io::stdin())?;

    let book_version = Version::parse(&ctx.mdbook_version)?;
    let version_req = VersionReq::parse(mdbook::MDBOOK_VERSION)?;

    if !version_req.matches(&book_version) {
        eprintln!(
            "Warning: The {} plugin was built against version {} of mdbook, \
             but we're being called from version {}",
            pre.name(),
            mdbook::MDBOOK_VERSION,
            ctx.mdbook_version
        );
    }

    let processed_book = pre.run(&ctx, book)?;
    serde_json::to_writer(io::stdout(), &processed_book)?;

    Ok(())
}

fn handle_supports(pre: &dyn Preprocessor, sub_args: &ArgMatches) -> ! {
    let renderer = sub_args
        .get_one::<String>("renderer")
        .expect("Required argument");
    let supported = pre.supports_renderer(renderer);

    if supported {
        process::exit(0);
    } else {
        process::exit(1);
    }
}

fn main() {
    let matches = make_app().get_matches();
    let preprocessor = ClitestPreprocessor::new();

    if let Some(sub_args) = matches.subcommand_matches("supports") {
        handle_supports(&preprocessor, sub_args);
    } else if let Err(e) = handle_preprocessing(&preprocessor) {
        eprintln!("{e:?}");
        process::exit(1);
    }
}
