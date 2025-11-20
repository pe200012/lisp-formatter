# Changelog for `lisp-formatter`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2025-11-20

### Added
- Initial release with core formatting functionality
- Support for parsing and formatting S-expressions
- Multiple delimiter types: parentheses `()`, square brackets `[]`, curly braces `{}`
- Quote notation support: `'`, `` ` ``, `,`, `,@`
- Comment preservation (`;` style)
- Configurable indentation width (default: 2 spaces)
- Configurable line width for inline forms (default: 80 characters)
- Automatic multi-line breaking for long expressions
- Unicode support in atoms and strings
- CLI with `optparse-applicative`
- Comprehensive test suite with hspec
- Full Clojure syntax support (vectors, maps, sets)
- Support for Scheme, Common Lisp, Racket, and other Lisp dialects
