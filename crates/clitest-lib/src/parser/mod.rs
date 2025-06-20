use crate::script::{Script, ScriptError, ScriptErrorType, ScriptFile, ScriptLocation};

pub mod v0;

pub fn parse_script(file: ScriptFile, script: &str) -> Result<Script, ScriptError> {
    let version = script.lines().next().unwrap_or_default();
    match version {
        "#!/usr/bin/env clitest --v0" => v0::parse_script(file, script),
        _ => Err(ScriptError::new(
            ScriptErrorType::InvalidVersion,
            ScriptLocation::new(file, 1),
        )),
    }
}
