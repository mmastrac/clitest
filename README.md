# cli-tester

A CLI testing tool that allows you to write tests for command-line applications using a simple syntax.

## Installation

```
cargo install clitest
```

## Syntax

The test files use the following syntax:

- `# <comment>` - Comments that are ignored during test execution
- `$ <command>` - Shell command to execute
- `? <grok pattern>` - Match output using a grok pattern
- `! <grok pattern>` - Match output using an auto-escaped grok pattern (ie: the non-grok parts will
be escaped so that they are not interpreted as regex)
- `!!!` - Multi-line ! block (starts and ends with `!!!`)
- `???` - Multi-line ? block (starts and ends with `???`)
- `repeat { ... }` - Match the enclosed patterns multiple times
- `optional { ... }` - Match the enclosed patterns zero or one time
- `choice { ... }` - Match one of the enclosed patterns
- `unordered { ... }` - Match all enclosed patterns in any order
- `sequence { ... }` - Match all enclosed patterns in sequence
- `ignore { ... }` - Ignore any output that matches the enclosed patterns
- `reject { ... }` - Fail if any output matches the enclosed patterns

## Examples

Match exact output:

```
$ echo "a\nb\nc"
! a
! b
! c
```

Match using a grok pattern:

```
$ echo "anything"
? %{GREEDYDATA}
```

Match multiple lines:

```
$ echo "a\nb\nc"
!!!
a
b
c
!!!
```

Match multiple lines using a grok pattern:

```
$ echo "a\nb\nc"
repeat {
    ? %{DATA}
}
???
```

