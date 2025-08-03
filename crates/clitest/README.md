# *CLI/test*: A literate CLI testing tool

[![Build](https://img.shields.io/github/actions/workflow/status/mmastrac/clitest/build.yml?branch=master)](https://github.com/mmastrac/clitest/actions/workflows/build.yml)
[![Book](https://img.shields.io/badge/book-online-blue)](https://mmastrac.github.io/clitest/)

A CLI testing tool that allows you to write tests for command-line applications using a simple, literate syntax.

For more information, see the [book](https://mmastrac.github.io/clitest/) which contains a full syntax reference and examples.

## Installation

```shell
cargo install clitest
```

## Usage

```shell
clitest [options] [test-file] [test-file] ...
```

The test runner will exit with a non-zero exit code if the command does not
match the expected output.

## Syntax

The test files use the following syntax:

### Command Execution

| Command               | Description                                   |
| --------------------- | --------------------------------------------- |
| `$ <command> …`       | Execute a shell command and match its output  |
| `%EXIT <n>`           | Expect command to exit with specific code n   |
| `%EXIT fail`          | Expect command to exit with any non-zero code |
| `%EXIT any`           | Accept any exit code (including timeouts)     |
| `%EXIT timeout`       | Expect command to timeout                     |
| `%TIMEOUT <duration>` | Set timeout for a command (e.g., 100ms, 5s)   |
| `%SET <variable>`     | Capture command output into a variable        |
| `%EXPECT_FAILURE`     | Expect pattern matching to fail               |

### Variables and Quoting

*CLI/test* uses shell-style variable references and quoting to delimit strings in
commands and control structures.

| Quote Type | Behavior                                              |
| ---------- | ----------------------------------------------------- |
| `'text'`   | Single quotes - literal value, no expansion           |
| `"text"`   | Double quotes - literal value with variable expansion |
| `\char`    | Backslash escape - preserve literal meaning           |
| `$VAR`     | Basic variable reference                              |
| `${VAR}`   | Explicit variable reference                           |
| `$PWD`     | Special variable for working directory                |

### Control Structures

| Structure                 | Description                                            |
| ------------------------- | ------------------------------------------------------ |
| `# <comment>`             | Ignore this line during test execution                 |
| `include <path>;`         | Include another script                                 |
| `if condition { … }`      | Conditionally execute commands                         |
| `for <var> in <…> { … }`  | Iterate over a list of values                          |
| `background { … }`        | Run commands in background (auto-killed on exit)       |
| `defer { … }`             | Execute cleanup commands after block ends (LIFO order) |
| `retry { … }`             | Retry commands until success or timeout                |
| `exit script;`            | Exit script early with success status                  |
| `set <var> <value>;`      | Set environment variable directly                      |
| `cd <directory>;`         | Change working directory                               |
| `using tempdir;`          | Create and use temporary directory (auto-deleted)      |
| `using new dir <name>;`   | Create new directory for testing (auto-deleted)        |
| `using dir <path>;`       | Use existing directory (not deleted)                   |
| `pattern <NAME> <regex>;` | Define custom grok pattern                             |

### Patterns

| Pattern                              | Description                                                  |
| ------------------------------------ | ------------------------------------------------------------ |
| `! <text>`                           | Auto-escaped pattern (literal text matching + grok patterns) |
| `? <pattern>`                        | Raw pattern (regex-style, requires escaping + grok patterns) |
| `!!!`                                | Multi-line auto-escaped pattern block                        |
| `???`                                | Multi-line raw pattern block                                 |
| `*`                                  | Any pattern (matches any number of lines lazily)             |
| `%{PATTERN_NAME}`                    | Standard grok pattern                                        |
| `%{PATTERN_NAME=(regex)}`            | Custom grok pattern with regex                               |
| `%{PATTERN_NAME:field_name}`         | Named grok pattern with output field                         |
| `%{PATTERN_NAME:field_name=(regex)}` | Custom named grok pattern                                    |
| `repeat { … }`                       | Match pattern multiple times                                 |
| `choice { … }`                       | Match any one of specified patterns                          |
| `unordered { … }`                    | Match patterns in any order                                  |
| `sequence { … }`                     | Match patterns in strict order                               |
| `optional { … }`                     | Make pattern optional (zero or one match)                    |
| `if <condition> { … }`               | Conditionally require patterns                               |
| `ignore { … }`                       | Skip/ignore certain output patterns                          |
| `reject { … }`                       | Ensure patterns don't appear in output                       |

### Common Grok Patterns

This is a subset of the grok patterns supported by *CLI/test*. See the full list of
supported patterns at <https://docs.rs/grok/latest/grok/patterns/index.html>,
including the full base patterns in the `grok` module:
<https://docs.rs/grok/latest/grok/patterns/grok/index.html>.

| Pattern         | Description               | Example                |
| --------------- | ------------------------- | ---------------------- |
| `%{DATA}`       | Matches any text (lazy)   | `Hello, %{DATA}`       |
| `%{GREEDYDATA}` | Matches any text (greedy) | `Hello, %{GREEDYDATA}` |
| `%{WORD}`       | Matches word characters   | `[%{WORD}]`            |
| `%{NUMBER}`     | Matches numeric values    | `Count: %{NUMBER}`     |

## Examples

Match exact output:

```shell
$ printf "a\nb\nc"
! a
! b
! c
```

Match using a [grok](https://mmastrac.github.io/clitest/grok-patterns.html) pattern:

```shell
$ echo "Hello, anything"
? Hello, %{GREEDYDATA}
```

