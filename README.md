# lisp-formatter

A general-purpose code formatter for Lisp-like languages, written in Haskell.

## Features

- **Parser**: Parses S-expressions with support for:
  - Atoms, strings, and lists
  - Multiple delimiter types: `()`, `[]`, `{}`
  - Comments (`;` style)
  - Quote notation (`'`, `` ` ``, `,`, `,@`)
  - Nested structures
  - Unicode support

- **Formatter**: Intelligently formats code with:
  - Configurable indentation width
  - Configurable line width for inline forms
  - Automatic multi-line breaking for long expressions
  - Comment preservation
  - Compact inline rendering when possible

## Installation

```bash
stack build
stack install
```

## Usage

### Command Line

```bash
# Format from stdin to stdout
echo '(defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))' | lisp-formatter

# Format a file
lisp-formatter --in input.lisp --out output.lisp

# Use custom indentation (default: 2 spaces)
lisp-formatter --indent 4 < input.lisp

# Use custom line width (default: 80 characters)
lisp-formatter --inline-width 100 < input.lisp

# Show help
lisp-formatter --help
```

### Library

```haskell
import Lib
import qualified Data.Text as T

main :: IO ()
main = do
    let code = "(defun hello (name) (print name))"
    case formatLispText defaultOptions (T.pack code) of
        Left err -> print err
        Right formatted -> T.putStrLn formatted
```

## Options

- `--indent N` or `-i N`: Number of spaces for indentation (default: 2)
- `--inline-width N` or `-w N`: Maximum width for single-line forms (default: 80)
- `--in FILE`: Read from FILE instead of stdin
- `--out FILE` or `-o FILE`: Write to FILE instead of stdout
- `--help` or `-h`: Show help message

## Examples

### Input
```lisp
(define (factorial n)(if(= n 0)1(* n(factorial(- n 1)))))
```

### Output
```lisp
(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))
```

### Input (long expression)
```lisp
(some-really-quite-extraordinarily-long-function-name (first-argument second-argument third-argument fourth-argument fifth-argument sixth-argument))
```

### Output
```lisp
(some-really-quite-extraordinarily-long-function-name
  (first-argument second-argument third-argument fourth-argument fifth-argument sixth-argument))
```

## Development

```bash
# Build
stack build

# Run tests
stack test

# Run with profiling
stack build --profile
stack exec --profile -- lisp-formatter-exe +RTS -p
```

## Supported Lisp Dialects

The formatter is designed to be general-purpose and should work with:
- **Scheme**: Traditional Lisp with parentheses
- **Common Lisp**: Full support for parentheses-based syntax
- **Racket**: Scheme-family language
- **Clojure**: Full support including vectors `[]`, maps `{}`, and sets `#{}`
- **Emacs Lisp**: Traditional parentheses-based Lisp
- Any other Lisp-like language with S-expression syntax

### Clojure Support

The formatter fully supports Clojure's additional delimiter types:
```clojure
; Vectors
[1 2 3]

; Maps
{:name "Alice" :age 30}

; Function with vector parameters
(defn add [a b]
  (+ a b))

; Let bindings
(let [x 10
      y 20]
  (+ x y))
```

## License

BSD-3-Clause (see LICENSE file)
