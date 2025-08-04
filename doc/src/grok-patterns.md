# Grok patterns

Grok patterns are a way to parse text into structured data.

## Syntax

A grok pattern is constructed using one of the formats:

- `%{PATTERN_NAME}`: a standard named pattern
- `%{PATTERN_NAME=(regex)}`: a custom pattern defined using a regular expression
- `%{PATTERN_NAME:field_name}`: a standard named pattern with a named output
- `%{PATTERN_NAME:field_name=(regex)}`: a custom pattern with a named output

## Examples

The most basic pattern is `%{DATA}`, which matches any text lazily: as few times
as possible for the remainder of the line to match. Alternatively, you can use
`%{GREEDYDATA}` to greedily match any text, as many times as possible while
allowing the remainder of the line to match.

```bash session
$ echo "Hello, world!"
! Hello, %{DATA:what}!
```

Custom patterns are defined using the `pattern` command, after which the patterns
are available for use in the tests.

```bash session
pattern GREETING Hello|Goodbye;

$ echo "[INFO] Hello, world!"
! [%{LOGLEVEL}] %{GREETING}, %{DATA}!
```

A custom pattern may also be defined inline:

```bash session
$ echo "[DEBUG] Hello, world!"
! [%{CUSTOMLEVEL=INFO|DEBUG}] %{GREETING=(Hello|Goodbye)}, %{DATA}!
```

Custom patterns may be named and reused in a single line:

```bash session
$ echo "[DEBUG] Hello, world!"
! [%{MY_WORD=(\w+)}] %{MY_WORD}, %{MY_WORD}!
```

## Expectations and Aliases

By default, patterns don't have aliases and may match any value. If any patterns
are given aliases (eg: `%{PATTERN:alias}), all values of those aliases must
match throughout the entire test output.

You can use the `%EXPECT` command to expect a specific value for a given alias.
If no initial value is provided, the value is taken from the first match.

```bash session
$ echo "[DEBUG] Hello, world!"
%EXPECT word1 "DEBUG"
%EXPECT word2 "Hello"
%EXPECT word3 "world"
! [%{MY_WORD:word1=(\w+)}] %{MY_WORD:word2}, %{MY_WORD:word3}!
```

```bash session
$ printf "line\nline\nline\n"
! %{WORD:word}
! %{WORD:word}
! %{WORD:word}
```

## References

For further reading, see:

- [IBM Streams documentation](https://www.ibm.com/docs/en/streamsets/6.x?topic=guide-grok-patterns)
- [Mezmo documentation](https://docs.mezmo.com/telemetry-pipelines/using-grok-to-parse)
- [EdgeDelta blog post](https://edgedelta.com/company/blog/what-are-grok-patterns/)

## Tools

Some potential tools for working with grok patterns:

- [Grok debugger](https://grokdebugger.com/)
- [Regex101](https://regex101.com/)
