# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.30] - 2025-08-02

### Added
- **`exit script`**: Added support for exiting a script early
- **`include <path>`**: Added support for including another script
### Changed
- **`pattern <regex>`**: The regular expression must now end with a semicolon

## [0.1.29] - 2025-07-31

### Changed

- Updated dependencies to latest versions
- **`%EXIT fail`**: Added support for expecting command failures

## [0.1.28] - 2025-06-25

### Added

- **`%TIMEOUT` directive**: Added support for setting command timeouts with
  duration suffixes (s, ms, us, etc.)
- **Execution time display**: Commands now show their execution times in output
- **Enhanced documentation**: Added comprehensive grok patterns documentation
- **Improved timeout handling**: Better timeout management and error reporting

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

- **`%TIMEOUT` directive**: Initial implementation of timeout functionality
- **Execution time tracking**: Commands now display their execution duration
- **Enhanced timeout support**: Better integration with command execution
  pipeline

### Changed

- Updated command execution to include timing information
- Improved parser to handle timeout directives
- Enhanced script execution with timeout capabilities

## [0.1.17] - 2025-06-20

### Added

- **`retry` blocks**: Added `retry { ... }` functionality for retrying commands
  until success
- **Internal commands**: Added various internal commands for enhanced
  functionality
- **Glob support**: Added glob patterns to test file matching
- **Enhanced whitespace handling**: Improved handling of whitespace in triple
  blocks

### Changed

- Refactored output code separation from script code
- Improved command execution reliability

## [0.1.6] - 2025-06-15

### Added

- **Versioned parser**: Implemented versioned parsing system for better
  compatibility

### Changed

- Switched to workspace dependencies for better dependency management
- Refactored codebase to split into multiple crates for better organization
- Improved Windows path handling using `dunce` crate
- Enhanced path handling throughout the application

### Fixed

- **Platform-specific fixes**: Fixed tests on macOS and other platforms
- **Build system**: Multiple build fixes and improvements
- **PWD handling**: Fixed working directory handling issues
- **`background` block reliability**: Improved reliability of `background { }`
  execution

### Changed

- **MSRV**: Reverted minimum supported Rust version to 1.85
- **Documentation**: Updated README to point to the book and trimmed content
- **Project structure**: Created mdbook.yml for documentation

## [0.1.0] - 2025-06-08

### Added

- **Initial release**: First public release of clitest
