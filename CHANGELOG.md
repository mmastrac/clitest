# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.3] - 2025-08-26

### Fixed
- Improved handling of ANSI escape sequences in output
- Shell escaping is now more robust in commandlines and environment variables

## [0.2.2] - 2025-08-25

### Fixed
- ansi: Fixed bug where ANSI escape sequences were not stripped from output
- Automatically strip ANSI escape sequences from output if patterns don't match for a second match attempt

## [0.2.1] - 2025-08-11

### Added
- `not` patterns: Added support for negative lookahead patterns

## [0.2.0] - 2025-08-04

### Added
- Literal blocks: Added support for triple double-quote literal blocks (triple
  double-quote, ie: `"""`)
- (MAJOR) Grok captures: Added support for grok captures, extraction to variables
  ```bash session
  # All grok captures of "word" must match!

  $ echo "Hello, world!\nGoodbye, world!"
  # Expect a grok capture as an environment variable
  %SET CAPTURED_WORD ${word}
  ! Hello, %{WORD:word}!
  ! Goodbye, %{WORD:word}!

  $ echo "Hello, world!\nGoodbye, Earth!"
  # Import an environment variable as a grok capture requirement
  %EXPECT word "$CAPTURED_WORD"
  %EXPECT_FAILURE
  ! Hello, %{WORD:word}!
  ! Goodbye, %{WORD:word}!
  ```
### Fixed
 - `pattern`: Fixed bug where pattern could not appear before global `reject`/`ignore`
 
## [0.1.30] - 2025-08-02

### Added
- `exit script`: Added support for exiting a script early
- `include <path>`: Added support for including another script
### Changed
- `pattern <regex>`: The regular expression must now end with a semicolon

## [0.1.29] - 2025-07-31

### Changed

- Updated dependencies to latest versions
- `%EXIT fail`: Added support for expecting command failures

## [0.1.28] - 2025-06-25

### Added

- `%TIMEOUT` directive: Added support for setting command timeouts with
  duration suffixes (s, ms, us, etc.)
- Commands now show their execution times in output
- Added comprehensive grok patterns documentation
- Better timeout management and error reporting

### Changed

- Enhanced timeout handling throughout the codebase
- Improved command execution and error handling
- Updated documentation with new features and examples
- Refined test output formatting

### Fixed

- Timeout-related test failures and edge cases
- Improved reliability of background test execution

## [0.1.25] - 2025-06-24

### Added

- `%TIMEOUT` directive: Initial implementation of timeout functionality
- Commands now display their execution duration
- Better integration with command execution pipeline

### Changed

- Updated command execution to include timing information
- Improved parser to handle timeout directives
- Enhanced script execution with timeout capabilities

## [0.1.17] - 2025-06-20

### Added

- `retry` blocks: Added `retry { ... }` functionality for retrying commands
  until success
- Added various internal commands for enhanced
  functionality
- Added glob patterns to test file matching
- Improved handling of whitespace in triple blocks

### Changed

- Refactored output code separation from script code
- Improved command execution reliability

## [0.1.6] - 2025-06-15

### Added

- Implemented versioned parsing system for better compatibility

### Changed

- Switched to workspace dependencies for better dependency management
- Refactored codebase to split into multiple crates for better organization
- Improved Windows path handling using `dunce` crate
- Enhanced path handling throughout the application

### Fixed

- Fixed tests on macOS and other platforms
- Multiple build fixes and improvements
- `PWD` handling: Fixed working directory handling issues
- `background` block reliability: Improved reliability of `background { }`
  execution

### Changed

- Reverted minimum supported Rust version to 1.85
- Updated README to point to the book and trimmed content
- Created mdbook.yml for documentation

## [0.1.0] - 2025-06-08

### Added

- Initial release: First public release of clitest
