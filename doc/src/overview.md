[![GitHub]][repo] [![rustdoc]][docs] [![Latest Version]][crates.io]

[GitHub]: img/github.svg
[repo]: https://github.com/mmastrac/clitest
[rustdoc]: img/rustdoc.svg
[docs]: https://docs.rs/clitest
[Latest Version]: https://img.shields.io/crates/v/clitest.svg
[crates.io]: https://crates.io/crates/clitest


# CLI/test

<div style="text-align: center;">
  <img src="img/clitest-logo.png" alt="CLI/test logo" style="width: 85px">
</div>

**CLI/test** is a CLI testing tool that allows you to write tests for command-line
applications using a simple, literate syntax. Unlike bash-based test frameworks,
expected output sits right next to the command that produces it, allowing tests to be read
like annotated terminal sessions.

**CLI/test** is built around `.cli` test files and the `clitest` runner, with optional
integrations: the `clitest-lib` crate for Rust tests, and `clitest-mdbook` so book
examples execute at build time.

Tests are as simple as:

```bash session
$ echo "Hello, world!"
! Hello, world!
```

### Design

**CLI/test** tests read as annotated terminal sessions: the command and its expected
output sit side by side, not split across separate assertion blocks. Built on [Grok
patterns](./grok-patterns.md), you can match structured or variable output without
brittle regular expressions. The runner executes each `$` command in order and stops on
the first failure.

### Building blocks

- **Commands** (`$ …`): run shell commands and match their output. See
  [Basic Usage](./basic-usage.md).
- **Directives** (`%EXIT`, `%SET`, ...) control exit codes, timeouts, and captures. See
  [Basic Usage](./basic-usage.md).
- **Patterns** (`!`, `?`, grok) match stdout literally, with regex, or with named
  patterns. See [Pattern Matching](./pattern-matching.md) and
  [Grok Patterns](./grok-patterns.md).
- **Control structures** (`if`, `for`, `background`, `defer`, `retry`, ...): express
  complex scenarios. See [Control Structures](./control-structures.md).
- **Variables and environment**: reference captured output and manage working
  directories. See [Environment and Variables](./environment.md).

A complete minimal file:

```bash session
#!/usr/bin/env clitest --v0

$ echo "Hello, world!"
! Hello, world!

$ cat nonexistent-file
%EXIT fail
*
```

### Integrations

- **Standalone `.cli` files**: write tests, then run `clitest tests/*.cli` locally or
  in CI. Each file is self-contained and readable as documentation.
- **Executable scripts**: with the shebang in place, mark the file executable and run
  it directly; the shebang invokes `clitest` for you.
- **Rust integration tests**: embed scripts in Rust tests with the `clitest!` macro from
  [clitest-lib](https://docs.rs/clitest-lib).
- The code blocks in this book are tested with the latest release of **CLI/Test*.

See [Getting Started](./getting-started.md) for a feature summary, [Installation](./installation.md) to start using **CLI/test**, or [Basic Usage](./basic-usage.md) to learn the syntax.
