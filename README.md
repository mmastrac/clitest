# cli-tester

A CLI testing tool that allows you to write tests for command-line applications using a simple syntax.

## Installation

```shell
cargo install clitest
```

## Usage

```shell
clitest <test-file> <test-file> ...
```

The test runner will exit with a non-zero exit code if the command does not
match the expected output.

## Syntax

The test files use the following syntax:

- `# <comment>` - Comments that are ignored during test execution
- `$ <command>` - Shell command to execute
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

Match multiple lines:

```shell
$ echo "a\nb\nc"
!!!
a
b
c
!!!
```

Match multiple lines using a grok pattern:

```shell
$ echo "a\nb\nc"
repeat {
    ? %{DATA}
}
```

## Handling exit codes and mismatches

If a command exits with a non-zero exit code, the test runner will fail.

This can be overridden using `%EXIT <n|any>`:

```shell
$ exit 1
%EXIT 1
```

or

```shell
$ exit 1
%EXIT any
```

If the test is expected not to match the provided output, use `%EXPECT_FAILURE`:

```shell
$ echo "a\nb\nc"
%EXPECT_FAILURE
! wrong
```
