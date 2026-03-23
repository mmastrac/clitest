# Introduction

*CLI/test* is a CLI testing tool that allows you to write tests for command-line
applications using a simple, literate syntax. Unlike bash-based test frameworks,
expected output sits right next to the command that produces it — so tests read
like annotated terminal sessions.

It provides flexible ways to verify command outputs, handle exit codes, manage
environment variables, and juggle processes. Built on top of [Grok
patterns](./grok-patterns.md), you can match complex patterns in the output with
minimal effort.

Tests are as simple as:

```bash session
$ echo "Hello, world!"
! Hello, world!
```

## Features

- Simple and readable test syntax
- Support for pattern matching using grok patterns
- Flexible output matching with multi-line support
- Environment variable management
- Control structures for complex test scenarios
- Background process management
- Temporary directory handling
- Cleanup
- Retry logic

## Why *CLI/test*?

*CLI/test* makes it easy to write and maintain tests for command-line
applications. Its syntax is designed to be concise, human-readable, and
powerful, allowing you to express complex test scenarios without extra noise.

