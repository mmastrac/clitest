# Control Structures

`clitest` provides several control structures to help you write complex test scenarios.

## Quoting

Note that internal commands and control structures follow shell-style syntax, so quoting
is significant.

Single quotes (`'`) preserve the literal value of every character within the
quotes. No characters inside single quotes have special meaning.

Double quotes (`"`) preserve the literal value of most characters, but still
allow for variable expansion (e.g., `$VAR` or `${VAR}`).

Backslashes (`\`) can be used to escape the next character, preserving its
literal meaning. This works both inside double quotes and unquoted text.

## For Loops

The `for` block allows you to iterate over a list of values:

```bash session
for OS in "linux" "macos" "windows" {
    $ uname -a | grep $OS
    %EXIT any
    optional {
        ! %{GREEDYDATA}
    }
}
```

## Conditional Blocks

You can use `if` blocks to conditionally execute commands:

```bash session
if TARGET_OS == "linux" {
    $ echo Linux specific output
    ! Linux specific output
}
```

Note that pattern `if` blocks and control `if` blocks have identical syntax, but
one contains patterns and the other contains commands.

## Background processes

Run commands in the background using `background { }`. When the block ends, the
background process is automatically killed. If the test exits early (e.g., due
to a failure), background processes are also killed.

Commands running in a `background` block have no explicit timeout, but you can
set an explicit timeout for each command with `%TIMEOUT` if needed.

```bash session
using tempdir;

background {
    $ python3 -m http.server 60801 2> server.log
    %EXIT any
}

$ echo "OK" > health

retry {
    $ curl -s http://localhost:60801/health
    ! OK
}
```

## Deferred cleanup

Run commands after the block finishes. Multiple `defer` blocks are executed in
reverse order (last in, first out):

```bash session
defer {
    $ echo "Second cleanup"
    ! Second cleanup
}

defer {
    $ echo "First cleanup"
    ! First cleanup
}

$ echo "Running!"
! Running!

$ echo "Done!"
! Done!
```

## Retry

Retry commands until they succeed or timeout:

```bash session
retry {
    $ true
}
``` 

`retry` uses the global timeout for the whole `retry` block, but you can set a
shorter timeout for the command itself with `%TIMEOUT`:

```bash session
retry {
    $ true
    %TIMEOUT 100ms
}
```
