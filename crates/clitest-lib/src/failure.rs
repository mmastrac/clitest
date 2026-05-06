use crate::{output::{Line, OutputPatternType}, script::{IfCondition, ScriptLocation}};

#[derive(Clone, Debug, thiserror::Error, derive_more::Display, PartialEq, Eq)]
#[display("pattern {pattern_type} at line {location} {verb} output line {line:?}", verb = self.verb(), line = self.line())]
pub struct OutputPatternMatchFailure {
    pub location: ScriptLocation,
    pub pattern_type: &'static str,
    pub output_line: Option<Line>,
}

impl OutputPatternMatchFailure {
    fn verb(&self) -> &'static str {
        if self.pattern_type == "reject" {
            "rejected"
        } else {
            "does not match"
        }
    }

    fn line(&self) -> String {
        self.output_line
            .as_ref()
            .map(|l| l.text.clone())
            .unwrap_or("<eof>".to_string())
    }
}

#[derive(Debug, Clone)]
pub enum OutputMatchTrace {
    Matching(OutputPatternType),
    Match(Line),
    NoMatch(Line),
    AliasFailed(String, String),
    IfMatch(IfCondition),
    IfNoMatch(IfCondition),
}

impl std::fmt::Display for OutputMatchTrace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputMatchTrace::Matching(pattern) => write!(f, "matching {pattern:?}"),
            OutputMatchTrace::Match(line) => write!(f, "match: {line:?}", line = line.text),
            OutputMatchTrace::NoMatch(line) => write!(f, "FAILED match: {line:?}", line = line.text),
            OutputMatchTrace::AliasFailed(existing, value) => write!(f, "alias failed: {existing:?} != {value:?}"),
            OutputMatchTrace::IfMatch(condition) => write!(f, "if match: {condition:?}"),
            OutputMatchTrace::IfNoMatch(condition) => write!(f, "if no match: {condition:?}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct OutputMatchTraceLine {
    pub indent: usize,
    pub ignore: bool,
    pub trace: OutputMatchTrace,
}

impl std::fmt::Display for OutputMatchTraceLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:indent$}{ignore}{trace}{ignore2}", "", 
        indent = self.indent, 
        ignore = if self.ignore { "-" } else { "" },
        trace = self.trace,
        ignore2 = if self.ignore { " (ignore)" } else { "" })
    }
}