# `clitest`: A literate CLI testing tool

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

- `# <comment>` - Comments that are ignored during test execution

### Top-level

- `background { ... }` - Run the enclosed commands in the background, killing
  them when the current block finishes
- `defer { ... }` - Run the enclosed commands after the test has finished
- `retry { ... }` - Run the enclosed commands until they succeed or the test times out
- `for <var> in <list> { ... }` - Run the enclosed commands for each item in the list
- `if <condition> { ... }` - Run the enclosed commands if the condition is true
- `$ <command>` - Shell command to execute

### Patterns

- `? <grok pattern>` - Match output using a grok pattern (ie: parts outside of
  the grok patterns are interpreted as regex)
- `! <grok pattern>` - Match output using an auto-escaped grok pattern (ie: the
  non-grok parts will be escaped so that they are not interpreted as regex)
- `!!!` - Multi-line ! block (starts and ends with `!!!`)
- `???` - Multi-line ? block (starts and ends with `???`)
- `repeat { ... }` - Match the enclosed patterns multiple times
- `optional { ... }` - Match the enclosed patterns zero or one time
- `choice { ... }` - Match one of the enclosed patterns
- `unordered { ... }` - Match all enclosed patterns in any order
- `sequence { ... }` - Match all enclosed patterns in sequence
- `ignore { ... }` - Ignore any output that matches the enclosed patterns
- `reject { ... }` - Fail if any output matches the enclosed patterns
- `*` - Match any output, lazily (completes when the next structure matches)
- `if <condition> { ... }` - Run the enclosed patterns if the condition is true
  (eg: `if $TARGET_OS == "linux"` or `if $TARGET_ARCH != "arm"`)

### Special modifiers

- `%SET <var>` - Set the variable to the output of the command (`PWD` is special
  and controls the current working directory)
- `%EXIT <n|any>` - Expect exit with the given exit code (or any if `any` is used)
- `%EXPECT_FAILURE` - Expect the pattern match to fail (and fail the test if it succeeds)
- `%TIMEOUT <duration>` - Set the timeout for the command (default is `30s`,
  suffixes `s`, `ms`, `us`, etc are supported)

## Examples

Match exact output:

```shell
$ echo "a\nb\nc"
! a
! b
! c
```

Match using a [grok](https://www.ibm.com/docs/en/streamsets/6.x?topic=guide-grok-patterns) pattern:

```shell
$ echo "Hello, anything"
? Hello, %{GREEDYDATA}
```


