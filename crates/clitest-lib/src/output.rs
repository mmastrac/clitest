use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex, OnceLock},
};

use grok::Grok;
use serde::Serialize;

use crate::script::{IfCondition, ScriptLocation, ScriptRunContext};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Line {
    pub number: usize,
    pub text: String,
}

#[derive(Clone)]
pub struct Lines {
    lines: Arc<Vec<String>>,
    current_line: usize,
    ignored_patterns: OutputPatterns,
    negative_disabled: bool,
    rejected_patterns: OutputPatterns,
}

impl std::fmt::Debug for Lines {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Lines {{ ignored: {} pattern(s), rejected: {} pattern(s) }}",
            self.ignored_patterns.len(),
            self.rejected_patterns.len()
        )
    }
}

impl<'s> IntoIterator for &'s Lines {
    type Item = &'s String;
    type IntoIter = std::slice::Iter<'s, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.lines.iter()
    }
}

impl std::fmt::Display for Lines {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lines[self.current_line..].join("\n"))
    }
}

impl Lines {
    pub fn new(lines: Vec<String>) -> Self {
        Self {
            lines: Arc::new(lines),
            current_line: 0,
            ignored_patterns: Default::default(),
            negative_disabled: false,
            rejected_patterns: Default::default(),
        }
    }

    pub fn is_exhausted(&self) -> bool {
        self.current_line >= self.lines.len()
    }

    pub fn next_line(&self) -> Option<Line> {
        if self.current_line < self.lines.len() {
            Some(Line {
                number: self.current_line,
                text: self.lines[self.current_line].clone(),
            })
        } else {
            None
        }
    }

    pub fn next(
        &self,
        context: OutputMatchContext,
    ) -> Result<(Option<Line>, Lines), OutputPatternMatchFailure> {
        let mut next = self.clone();
        'outer: while next.current_line < next.lines.len() {
            if !self.negative_disabled {
                let ignore_check = next.without_negatives();
                for ignored_pattern in &*next.ignored_patterns {
                    if let Ok(next_next) =
                        ignored_pattern.matches(context.ignore(), ignore_check.clone())
                    {
                        next = next_next.with_negatives();
                        continue 'outer;
                    }
                }
                for rejected_pattern in &*next.rejected_patterns {
                    if let Ok(_) = rejected_pattern.matches(context.ignore(), ignore_check.clone())
                    {
                        return Err(OutputPatternMatchFailure {
                            location: rejected_pattern.location.clone(),
                            pattern_type: "reject",
                            output_line: next.next_line(),
                        });
                    }
                }
            }
            let line = Line {
                number: next.current_line,
                text: next.lines[next.current_line].clone(),
            };
            next.current_line += 1;
            return Ok((Some(line), next));
        }
        Ok((None, next))
    }

    pub fn with_ignore(&self, ignore: &OutputPatterns) -> Self {
        let mut ignored_patterns = self.ignored_patterns.clone();
        ignored_patterns.extend(ignore);
        Self {
            ignored_patterns,
            ..self.clone()
        }
    }

    pub fn with_reject(&self, reject: &OutputPatterns) -> Self {
        let mut rejected_patterns = self.rejected_patterns.clone();
        rejected_patterns.extend(reject);
        Self {
            rejected_patterns,
            ..self.clone()
        }
    }

    fn without_negatives(&self) -> Self {
        Self {
            negative_disabled: true,
            ..self.clone()
        }
    }

    fn with_negatives(&self) -> Self {
        Self {
            negative_disabled: false,
            ..self.clone()
        }
    }

    pub fn into_inner(self) -> Vec<String> {
        Arc::unwrap_or_clone(self.lines).split_off(self.current_line)
    }

    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }
}

#[derive(Clone, Default, Debug, Serialize)]

pub struct OutputPatterns {
    patterns: Arc<Vec<OutputPattern>>,
}

impl OutputPatterns {
    pub fn new(patterns: Vec<OutputPattern>) -> Self {
        Self {
            patterns: Arc::new(patterns),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.patterns.is_empty()
    }

    pub fn len(&self) -> usize {
        self.patterns.len()
    }

    pub fn extend(&mut self, patterns: &OutputPatterns) {
        if self.is_empty() {
            self.patterns = patterns.patterns.clone();
            return;
        }
        let new_patterns = std::mem::take(&mut self.patterns);
        let mut new_patterns = Arc::unwrap_or_clone(new_patterns);
        new_patterns.extend(patterns.patterns.iter().cloned());
        self.patterns = Arc::new(new_patterns);
    }
}

impl std::ops::Deref for OutputPatterns {
    type Target = Vec<OutputPattern>;
    fn deref(&self) -> &Self::Target {
        &self.patterns
    }
}

#[derive(Clone)]
pub struct OutputPattern {
    pub location: ScriptLocation,
    pub pattern: OutputPatternType,
    pub ignore: OutputPatterns,
    pub reject: OutputPatterns,
}

impl Serialize for OutputPattern {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.pattern.serialize(serializer)
    }
}

impl std::fmt::Debug for OutputPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.pattern)
    }
}

impl OutputPattern {
    pub fn new_sequence(location: ScriptLocation, mut patterns: Vec<OutputPattern>) -> Self {
        if patterns.len() == 1 {
            patterns.remove(0)
        } else {
            Self {
                pattern: OutputPatternType::Sequence(patterns),
                ignore: Default::default(),
                reject: Default::default(),
                location: location.clone(),
            }
        }
    }

    pub fn prepare(&self, grok: &Grok) -> Result<(), OutputPatternPrepareError> {
        for pattern in &*self.ignore.patterns {
            pattern.prepare(grok)?
        }
        for pattern in &*self.reject.patterns {
            pattern.prepare(grok)?
        }
        match &self.pattern {
            OutputPatternType::Pattern(pattern) => {
                pattern
                    .prepare(grok)
                    .map_err(|e| OutputPatternPrepareError {
                        location: self.location.clone(),
                        pattern: pattern.pattern.clone(),
                        error: e,
                    })?
            }
            OutputPatternType::Sequence(patterns) => {
                for pattern in patterns {
                    pattern.prepare(grok)?;
                }
            }
            OutputPatternType::Unordered(patterns) => {
                for pattern in patterns {
                    pattern.prepare(grok)?;
                }
            }
            OutputPatternType::Choice(patterns) => {
                for pattern in patterns {
                    pattern.prepare(grok)?;
                }
            }
            OutputPatternType::If(_, pattern) => pattern.prepare(grok)?,
            OutputPatternType::Not(pattern) => pattern.prepare(grok)?,
            OutputPatternType::Any(pattern) => pattern.prepare(grok)?,
            OutputPatternType::Repeat(pattern) => pattern.prepare(grok)?,
            OutputPatternType::Optional(pattern) => pattern.prepare(grok)?,
            OutputPatternType::Literal(_) => {}
            OutputPatternType::End | OutputPatternType::None => {}
        }
        Ok(())
    }

    pub fn matches(
        &self,
        context: OutputMatchContext,
        output: Lines,
    ) -> Result<Lines, OutputPatternMatchFailure> {
        if self.ignore.is_empty() && self.reject.is_empty() {
            self.pattern.matches(&self.location, context, output)
        } else {
            let output = output.with_ignore(&self.ignore).with_reject(&self.reject);
            self.pattern.matches(&self.location, context, output)
        }
    }

    /// The minimum number of lines this pattern will match.
    pub fn min_matches(&self) -> usize {
        self.pattern.min_matches()
    }

    /// The maximum number of lines this pattern will match (or usize::MAX if unbounded).
    pub fn max_matches(&self) -> usize {
        self.pattern.max_matches()
    }
}

#[derive(thiserror::Error, Debug)]
#[error("pattern {pattern} at line {location} failed to compile: {error}")]
pub struct OutputPatternPrepareError {
    pub location: ScriptLocation,
    pub pattern: String,
    pub error: grok::Error,
}

#[derive(Clone)]
pub enum OutputPatternType {
    /// The end of the output
    End,
    /// Matches no lines of output, always succeeds
    None,
    /// Any lines, followed by a pattern.
    Any(Box<OutputPattern>),
    /// A literal string
    Literal(String),
    /// A grok pattern
    Pattern(Arc<GrokPattern>),
    /// A pattern that matches one or more of the given pattern
    Repeat(Box<OutputPattern>),
    /// A pattern that matches zero or one of the given pattern
    Optional(Box<OutputPattern>),
    /// A pattern that all of its subpatterns, but in any order
    Unordered(Vec<OutputPattern>),
    /// A pattern that matches one of the given patterns
    Choice(Vec<OutputPattern>),
    /// A pattern that matches a sequence of patterns
    Sequence(Vec<OutputPattern>),
    /// A negative look-ahead pattern
    Not(Box<OutputPattern>),
    /// A pattern that matches a condition
    If(IfCondition, Box<OutputPattern>),
}

impl Serialize for OutputPatternType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            OutputPatternType::Literal(literal) => {
                serializer.serialize_str(&format!("! {literal}"))
            }
            OutputPatternType::Pattern(pattern) => {
                serializer.serialize_str(&format!("? {}", pattern.pattern))
            }
            OutputPatternType::Repeat(pattern) => {
                HashMap::from([("repeat", &pattern)]).serialize(serializer)
            }
            OutputPatternType::Optional(pattern) => {
                HashMap::from([("optional", &pattern)]).serialize(serializer)
            }
            OutputPatternType::Unordered(patterns) => {
                HashMap::from([("unordered", &patterns)]).serialize(serializer)
            }
            OutputPatternType::Choice(patterns) => {
                HashMap::from([("choice", &patterns)]).serialize(serializer)
            }
            OutputPatternType::Sequence(patterns) => {
                HashMap::from([("sequence", &patterns)]).serialize(serializer)
            }
            OutputPatternType::Not(pattern) => {
                HashMap::from([("not", &pattern)]).serialize(serializer)
            }
            OutputPatternType::Any(pattern) => {
                HashMap::from([("any", &pattern)]).serialize(serializer)
            }
            OutputPatternType::If(condition, pattern) => {
                #[derive(Serialize)]
                struct If<'a> {
                    condition: &'a IfCondition,
                    pattern: &'a OutputPattern,
                }
                If { condition, pattern }.serialize(serializer)
            }
            OutputPatternType::End => serializer.serialize_str("end"),
            OutputPatternType::None => serializer.serialize_str("none"),
        }
    }
}

impl OutputPatternType {
    /// The minimum number of lines this pattern will match.
    pub fn min_matches(&self) -> usize {
        match self {
            OutputPatternType::None => 0,
            OutputPatternType::Literal(_) => 1,
            OutputPatternType::Pattern(_) => 1,
            OutputPatternType::Repeat(pattern) => pattern.min_matches(),
            OutputPatternType::Optional(_) => 0,
            OutputPatternType::Unordered(patterns) => {
                patterns.iter().map(|p| p.min_matches()).sum()
            }
            OutputPatternType::Choice(patterns) => {
                patterns.iter().map(|p| p.min_matches()).min().unwrap_or(0)
            }
            OutputPatternType::Sequence(patterns) => patterns.iter().map(|p| p.min_matches()).sum(),
            OutputPatternType::Not(_) => 0,
            OutputPatternType::Any(pattern) => pattern.min_matches(),
            OutputPatternType::If(_, _) => 0,
            OutputPatternType::End => 0,
        }
    }

    /// The maximum number of lines this pattern will match (or usize::MAX if unbounded).
    pub fn max_matches(&self) -> usize {
        fn saturating_iter_sum<I>(iter: I) -> usize
        where
            I: IntoIterator<Item = usize>,
        {
            iter.into_iter()
                .reduce(|n, i| n.saturating_add(i))
                .unwrap_or(0)
        }

        match self {
            OutputPatternType::None => 0,
            OutputPatternType::Literal(_) => 1,
            OutputPatternType::Pattern(_) => 1,
            OutputPatternType::Repeat(pattern) => {
                if pattern.max_matches() == 0 {
                    0
                } else {
                    usize::MAX
                }
            }
            OutputPatternType::Optional(pattern) => pattern.max_matches(),
            OutputPatternType::Unordered(patterns) => {
                saturating_iter_sum(patterns.iter().map(|p| p.max_matches()))
            }
            OutputPatternType::Choice(patterns) => {
                patterns.iter().map(|p| p.max_matches()).max().unwrap_or(0)
            }
            OutputPatternType::Sequence(patterns) => {
                saturating_iter_sum(patterns.iter().map(|p| p.max_matches()))
            }
            OutputPatternType::Not(_) => 0,
            OutputPatternType::Any(_) => usize::MAX,
            OutputPatternType::If(_, pattern) => pattern.max_matches(),
            OutputPatternType::End => 0,
        }
    }
}

impl Default for OutputPatternType {
    fn default() -> Self {
        Self::Sequence(vec![])
    }
}

impl std::fmt::Debug for OutputPatternType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputPatternType::Literal(literal) => write!(f, "Literal({literal})"),
            OutputPatternType::Pattern(pattern) => write!(f, "Pattern({pattern:?})"),
            OutputPatternType::Repeat(pattern) => write!(f, "Repeat({pattern:?})"),
            OutputPatternType::Optional(pattern) => write!(f, "Optional({pattern:?})"),
            OutputPatternType::Unordered(patterns) => write!(f, "Unordered({patterns:?})"),
            OutputPatternType::Choice(patterns) => write!(f, "Choice({patterns:?})"),
            OutputPatternType::Sequence(patterns) => write!(f, "Sequence({patterns:?})"),
            OutputPatternType::Not(pattern) => write!(f, "Not({pattern:?})"),
            OutputPatternType::Any(until) => write!(f, "Any({until:?})"),
            OutputPatternType::If(condition, pattern) => {
                write!(f, "If({condition:?}, {pattern:?})")
            }
            OutputPatternType::End => write!(f, "End"),
            OutputPatternType::None => write!(f, "None"),
        }
    }
}

#[derive(Serialize, derive_more::Debug)]
#[debug("/{pattern:?}/")]
pub struct GrokPattern {
    pattern: String,
    aliases: Vec<String>,
    #[serde(skip)]
    grok: OnceLock<grok::Pattern>,
}

impl GrokPattern {
    pub fn compile(line: &str, escape_non_grok: bool) -> Result<Self, String> {
        use grok::parser::GrokPatternError;
        let mut test_pattern = String::new();
        let mut final_pattern = String::new();
        let mut aliases = vec![];
        for bit in grok::parser::grok_split(line) {
            match bit {
                grok::parser::GrokComponent::RegularExpression { string, .. } => {
                    if escape_non_grok {
                        for char in string.chars() {
                            if char.is_ascii() && !char.is_alphanumeric() {
                                test_pattern.push('\\');
                                test_pattern.push(char);
                                final_pattern.push('\\');
                                final_pattern.push(char);
                            } else {
                                test_pattern.push(char);
                                final_pattern.push(char);
                            }
                        }
                    } else {
                        test_pattern.push_str(string);
                        final_pattern.push_str(string);
                    }
                }
                grok::parser::GrokComponent::GrokPattern { pattern, alias, .. } => {
                    test_pattern.push('.');
                    final_pattern.push_str(pattern);
                    if !alias.is_empty() {
                        aliases.push(alias.to_string());
                    }
                }
                grok::parser::GrokComponent::PatternError(GrokPatternError::InvalidCharacter(
                    c,
                )) => {
                    return Err(format!("Invalid character in pattern: {c:?}"));
                }
                grok::parser::GrokComponent::PatternError(GrokPatternError::InvalidPattern) => {
                    return Err("Invalid grok pattern".to_string());
                }
                grok::parser::GrokComponent::PatternError(
                    GrokPatternError::InvalidPatternDefinition,
                ) => {
                    return Err("Invalid grok pattern definition".to_string());
                }
            }
        }

        test_pattern.push('$');
        final_pattern.push('$');

        _ = Grok::empty()
            .compile(&test_pattern, false)
            .map_err(|e| e.to_string())?;

        Ok(Self {
            pattern: final_pattern,
            aliases,
            grok: OnceLock::new(),
        })
    }

    pub fn prepare(&self, grok: &Grok) -> Result<(), grok::Error> {
        // This could technically suffer from multiple init, but they should
        // always initialize the same way.
        if self.grok.get().is_none() {
            let pattern = grok.compile(&self.pattern, false)?;
            self.grok.get_or_init(move || pattern);
        }
        Ok(())
    }

    pub fn matches<'a>(&'a self, text: &'a str) -> Option<grok::Matches<'a>> {
        let pattern_ref = self.grok.get().expect("grok pattern not compiled");
        pattern_ref.match_against(text)
    }
}

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
pub struct OutputMatchContext<'s> {
    depth: usize,
    trace: Arc<Mutex<Vec<String>>>,
    ignore: bool,
    expectations: Arc<Mutex<HashMap<String, String>>>,
    script_context: &'s ScriptRunContext,
}

impl<'s> OutputMatchContext<'s> {
    pub fn new(script_context: &'s ScriptRunContext) -> Self {
        Self {
            depth: 0,
            trace: Default::default(),
            ignore: false,
            script_context,
            expectations: Default::default(),
        }
    }

    pub fn descend(&self) -> Self {
        Self {
            depth: self.depth + 1,
            trace: self.trace.clone(),
            ignore: self.ignore,
            script_context: self.script_context,
            expectations: self.expectations.clone(),
        }
    }

    pub fn ignore(&self) -> Self {
        Self {
            depth: self.depth,
            trace: self.trace.clone(),
            ignore: true,
            script_context: self.script_context,
            expectations: self.expectations.clone(),
        }
    }

    pub fn trace(&self, line: &str) {
        let ignore = if self.ignore { "-" } else { "" };
        self.trace.lock().unwrap().push(format!(
            "{:indent$}{ignore}{}",
            "",
            line,
            indent = self.depth * 2
        ));
    }

    pub fn traces(&self) -> Vec<String> {
        std::mem::take(&mut self.trace.lock().unwrap())
    }

    pub fn expect(&self, key: &str, value: String) {
        self.expectations
            .lock()
            .unwrap()
            .insert(key.to_string(), value);
    }

    pub fn expects(&self) -> HashMap<String, String> {
        self.expectations.lock().unwrap().clone()
    }
}

impl OutputPatternType {
    pub fn matches<'s>(
        &self,
        location: &ScriptLocation,
        context: OutputMatchContext,
        mut output: Lines,
    ) -> Result<Lines, OutputPatternMatchFailure> {
        context.trace(&format!("matching {self:?}"));
        match self {
            OutputPatternType::None => Ok(output),
            OutputPatternType::Literal(literal) => {
                let (line, next) = output.next(context.clone())?;
                if let Some(line) = line {
                    let text = line.text.trim_end();
                    if text == literal {
                        context.trace(&format!("literal match: {:?} == {literal:?}", line.text));
                        Ok(next)
                    } else if line.text.contains('\x1b')
                        && fast_strip_ansi::strip_ansi_string(&line.text).as_ref() == literal
                    {
                        context.trace(&format!("literal match: {text:?} == {literal:?}"));
                        Ok(next)
                    } else {
                        context.trace(&format!(
                            "literal FAILED match: {:?} == {literal:?}",
                            line.text
                        ));
                        Err(OutputPatternMatchFailure {
                            location: location.clone(),
                            pattern_type: "literal",
                            output_line: Some(line),
                        })
                    }
                } else {
                    Err(OutputPatternMatchFailure {
                        location: location.clone(),
                        pattern_type: "literal",
                        output_line: None,
                    })
                }
            }
            OutputPatternType::Pattern(pattern) => {
                let (line, next) = output.next(context.clone())?;
                if let Some(line) = line {
                    let mut text = line.text.clone();
                    let mut res = pattern.matches(&text);
                    if res.is_none() {
                        // Give it a second chance with the ANSI-stripped text IF we detect escape sequences
                        if text.contains('\x1b') {
                            text = fast_strip_ansi::strip_ansi_string(&text).into_owned();
                            res = pattern.matches(&text);
                        }
                    }
                    if let Some(matches) = res {
                        for alias in &pattern.aliases {
                            if let Some(value) = matches.get(alias) {
                                let existing = context
                                    .expectations
                                    .lock()
                                    .unwrap()
                                    .insert(alias.clone(), value.to_string());
                                if let Some(existing) = existing {
                                    if existing != value {
                                        context.trace(&format!(
                                            "pattern alias FAILED match: {existing:?} != {value:?}",
                                        ));
                                        return Err(OutputPatternMatchFailure {
                                            location: location.clone(),
                                            pattern_type: "pattern",
                                            output_line: Some(line),
                                        });
                                    }
                                }
                            }
                        }
                        context.trace(&format!("pattern match: {:?} =~ {pattern:?}", line.text));
                        Ok(next)
                    } else {
                        context.trace(&format!(
                            "pattern FAILED match: {:?} =~ {pattern:?}",
                            line.text
                        ));
                        Err(OutputPatternMatchFailure {
                            location: location.clone(),
                            pattern_type: "pattern",
                            output_line: Some(line),
                        })
                    }
                } else {
                    Err(OutputPatternMatchFailure {
                        location: location.clone(),
                        pattern_type: "pattern",
                        output_line: None,
                    })
                }
            }
            OutputPatternType::Sequence(patterns) => {
                for pattern in patterns {
                    match pattern.matches(context.descend(), output) {
                        Ok(v) => {
                            output = v;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(output)
            }
            OutputPatternType::Repeat(pattern) => {
                // Mandatory first match
                let mut output = pattern.matches(context.descend(), output)?;
                // Any number of additional matches, greedy
                loop {
                    match pattern.matches(context.descend(), output.clone()) {
                        Ok(new_rest) => {
                            output = new_rest;
                        }
                        Err(_) => break Ok(output),
                    }
                }
            }
            OutputPatternType::Optional(pattern) => {
                // Never fails
                match pattern.matches(context.descend(), output.clone()) {
                    Ok(v) => Ok(v),
                    Err(_) => Ok(output),
                }
            }
            OutputPatternType::Unordered(patterns) => {
                // Found is initialized with 0..patterns.len()
                let mut not_found = (0..patterns.len()).collect::<HashSet<_>>();
                'outer: while !not_found.is_empty() {
                    for pattern in &not_found {
                        let pattern = *pattern;
                        match patterns[pattern].matches(context.descend(), output.clone()) {
                            Ok(v) => {
                                not_found.remove(&pattern);
                                output = v;
                                continue 'outer;
                            }
                            Err(_) => {
                                continue;
                            }
                        }
                    }
                    return Err(OutputPatternMatchFailure {
                        location: location.clone(),
                        pattern_type: "unordered",
                        output_line: output.next_line(),
                    });
                }
                Ok(output)
            }
            OutputPatternType::Choice(patterns) => {
                for pattern in patterns {
                    if let Ok(v) = pattern.matches(context.descend(), output.clone()) {
                        return Ok(v);
                    }
                }
                Err(OutputPatternMatchFailure {
                    location: location.clone(),
                    pattern_type: "choice",
                    output_line: output.next_line(),
                })
            }
            OutputPatternType::Not(pattern) => {
                // Negative lookahead
                if let Err(_) = pattern.matches(context.descend(), output.clone()) {
                    Ok(output)
                } else {
                    Err(OutputPatternMatchFailure {
                        location: location.clone(),
                        pattern_type: "not",
                        output_line: output.next_line(),
                    })
                }
            }
            OutputPatternType::Any(until) => {
                loop {
                    match until.matches(context.descend(), output.clone()) {
                        Ok(v) => {
                            output = v;
                            break Ok(output);
                        }
                        Err(e) => {
                            // Eat one line and try again
                            let (line, next) = output.next(context.clone())?;
                            if line.is_some() {
                                output = next;
                                continue;
                            } else {
                                break Err(e);
                            }
                        }
                    }
                }
            }
            OutputPatternType::If(condition, pattern) => {
                if condition.matches(context.script_context) {
                    context.trace(&format!("if match: {condition:?}"));
                    pattern.matches(context.clone(), output.clone())
                } else {
                    context.trace(&format!("if FAILED match: {condition:?}"));
                    Ok(output)
                }
            }
            OutputPatternType::End => {
                let (line, next) = output.next(context)?;
                if let Some(line) = line {
                    Err(OutputPatternMatchFailure {
                        location: location.clone(),
                        pattern_type: "end",
                        output_line: Some(line),
                    })
                } else {
                    Ok(next)
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum PatternResult {
    Matches,
    MatchesFailure,
    ExpectedFailure,
    Mismatch(OutputPatternMatchFailure, String),
}
