# cli-tester

A CLI testing tool that allows you to write tests for command-line applications using a simple syntax.

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

# Extended Reference

## Basic Syntax

CLI test files use a special format to define test cases. Each test file should start with the shebang:

```bash
#!/usr/bin/env clitest --v0
```

## Command Execution

Commands are prefixed with `$`:

```bash
$ echo "Hello World"
```

Internal commands can be used to control the test environment. They do not use a
`$` prefix and must end with a semicolon:

```bash
using tempdir;
using dir "subdir";
using new dir "subdir";
cd "subdir";
```

 - `using tempdir` - Use a temporary directory for the test, tearing it down after the block ends
 - `using dir <path>` - Use the given directory for the test, leaving it in place after the block ends
 - `using new dir <path>` - Create a new directory for the test, tearing it down after the block ends
 - `cd <path>` - Change the current working directory from this point forward

## Output Matching

### Pattern Matching

There are two types of pattern matching:

1. `!` (Auto-escaped patterns):
   - Non-grok parts are automatically escaped to be treated as literal text
   - Grok patterns (like `%{DATA}`) are still interpreted
   - Example:
     ```bash
     $ printf "[LOG] Hello, world!\n"
     # [LOG] is treated literally, %{GREEDYDATA} matches any text
     ! [LOG] %{GREEDYDATA}
     ```

2. `?` (Raw patterns):
   - Everything is treated as a pattern
   - Special characters need to be escaped with `\`
   - Example:
     ```bash
     $ printf "[LOG] Hello, world!\n"
     # \[LOG\] matches [LOG] literally
     ? \[LOG\] %{GREEDYDATA}
     ```

### Multi-line Output

Use `!!!` for auto-escaped multi-line patterns or `???` for raw multi-line patterns:

```bash
$ printf "a\nb\nc\n"
!!!
a
b
c
!!!
```

## Control Structures

### Repeat

The `repeat` block allows matching repeated patterns:

```bash
$ printf "a\nb\nc\n"
# Match a repeating sequence of one of a, b, or c
repeat {
    choice {
        ! a
        ! b
        ! c
    }
}
```

### Choice

The `choice` block matches any one of the specified patterns:

```bash
# Match one of pattern1, pattern2, or pattern3
choice {
    ! pattern1
    ! pattern2
    ! pattern3
}
```

### Unordered

The `unordered` block matches patterns in any order:

```bash
unordered {
    ! a
    ! b
    ! c
}
```

### Sequence

The `sequence` block matches patterns in strict order:

```bash
sequence {
    ! a
    ! b
    ! c
}
```

### Optional

The `optional` block makes a pattern optional:

```bash
optional {
    ! optional output
}
```

### Ignore

The `ignore` block allows skipping certain output:

```bash
ignore {
    ? WARNING: %{DATA}
}
```

### Reject

The `reject` block ensures certain patterns don't appear:

```bash
reject {
    ! ERROR
}
```

## Environment Variables

### Setting Variables

You can set environment variables using `%SET`:

```bash
$ printf "value\n"
%SET MY_VAR
*
```

### Special Variables

- `PWD`: A special environment variable that controls the current working directory
  ```bash
  $ mktemp -d
  %SET TEMP_DIR
  *
  
  # Set PWD to change working directory
  $ echo $TEMP_DIR
  %SET PWD
  *
  ```

## Special Characters and Patterns

- Use `\` for line continuation
- Use `%%` to escape `%` in patterns
- Use `%{DATA}` to match any text in patterns
- Use `%{GREEDYDATA}` to match any text greedily
- Use `*` to match any output lazily (completes when the next structure matches)
