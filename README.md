# lisp-formatter

A small, focused formatter for Lisp-like languages. Produces consistent, readable output with configurable indentation and inline limits.

## Quick start

Build and run the formatter with Stack:

```bash
stack build
stack exec lisp-format -- --help
```

Format from stdin to stdout:

```bash
echo '(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))' | lisp-format
```

Format a file in place (write to stdout or a file):

```bash
lisp-format --in input.lisp --out output.lisp
# or
lisp-format --in input.lisp --out -  # write to stdout
```

## Installation

Using Stack:

```bash
stack build
stack install  # optional: install the binary to ~/.local/bin
```

## Usage

Command-line options of interest:

- `--indent N` or `-i N` — set indentation width (default: 2)
- `--inline-width N` or `-w N` — maximum width for keeping a form on a single line (default: 80)
- `--config FILE` or `-c FILE` — explicitly load configuration from `FILE`
- `--in FILE` — read input from `FILE` (`-` reads stdin)
- `--out FILE` or `-o FILE` — write output to `FILE` (`-` writes stdout)

Examples

Inline formatting when the form fits the configured width:

```lisp
(define (add x y) (+ x y))
```

Long forms are broken onto multiple lines with readable indentation:

```lisp
(some-long-function
  (arg1 arg2 arg3)
  (arg4 arg5))
```

## Format styles

The formatter supports a small set of layout styles that control how a list (a head followed by arguments) is rendered. These styles are used as the `defaultStyle` and can be applied to specific head atoms using the `specials` list in the Dhall configuration.

- InlineHead N

  Inline the head and the first N arguments, then place the remaining arguments on their own lines.
  This is useful for forms where a small number of leading arguments should stay with the head, and the remainder is structured vertically.

  Example (with N = 2):

```lisp
(foo a b
  c
  d)
```

- InlineHeadOneline N

  First, try to keep the whole form on a single line. If it doesn't fit, fall back to inlining the head with the first N arguments and break the rest across lines.
  This balances compact output with readable breaks when the form grows.

```lisp
; if it fits
(foo a b c)

; otherwise
(foo a
  b
  c)
```

- InlineAlign N

  Inline the head and the first N arguments when possible. The remaining arguments are printed on following lines aligned under the last inlined argument.
  Handy for column-like alignment where the first block acts as a tabstop.

```lisp
(foo a b c)

; If the inline part is longer and the form is broken, it looks like:
(foo a
     b
     c)
```

- Bindings
  The arguments are treated as bindings and formatted in pairs on separate lines.
  The remaining arguments are formatted normally. Useful for `let` and similar constructs.

```lisp
(let [a b
        c d]
  body)
```

- Newline
  Always break after the head and indent the subsequent lines.
  Use this for forms that should always be vertical for readability.

```lisp
(foo
  a
  b
  c)
```

- TryInline
  Try to render the entire form on a single line. If it exceeds the configured `inlineMaxWidth`, fall back to a multi-line representation determined by other rules.

When customizing formatting for particular forms, add `Special` entries in the `specials` list. Each `Special` maps an atom (the head) to a `FormatStyle`.

## Configuration

lisp-formatter reads configuration via Dhall. The formatter searches for a config file in this order (unless you pass `--config`):

1. Look for a file named `.lisp-format` in the current working directory and then each parent directory, up to the filesystem root.
2. If not found, look for `.lisp-format` in the user home directory.
3. If no configuration file is found or the file cannot be parsed, the built-in defaults are used.

You can override this search using `--config /path/to/yourconfig`.

Minimal example configuration (Dhall):

```dhall
let Style =
      < InlineHead : Integer
      | Bindings
      | Newline
      | TryInline
      >

let AlignStyle = < Normal | Align >

let AlignRule = { alignAtom : Text, alignStyle : AlignStyle }

let Special = { atom : Text, style : Style }

in { indentWidth = +2
   , inlineMaxWidth = +80
   , defaultStyle = Style.InlineHead +1
   , defaultAlign = AlignStyle.Align
   , specials = [] : List Special
   , aligns = [] : List AlignRule
   , preserveBlankLines = False
   }
```

Fields
- `indentWidth`: number of spaces used for each indentation level.
- `inlineMaxWidth`: maximum line width for keeping a list on one line.
- `defaultStyle`: default layout choice for unknown forms.
- `defaultAlign`: default alignment style for arguments.
- `specials`: list of special-case rules for particular head atoms.
- `aligns`: list of alignment rules for specific atoms.
- `preserveBlankLines`: when `True` blank lines are preserved; when `False` the formatter normalizes whitespace.

## Library use

A small API is exposed for programmatic use. Example:

```haskell
import Lib
import qualified Data.Text as T

main = case formatLispText defaultOptions (T.pack "(foo bar)") of
  Left err -> print err
  Right out -> T.putStrLn out
```

You can load configuration from Dhall using the library helpers (`readFormatOptionsFromPath` / `readFormatOptionsFromFile`).

## Development

Run the test suite and build locally with Stack:

```bash
stack build
stack test
```

If you want to debug or profile, use the Stack profiling flags as usual.

## Contributing

Contributions are welcome. Open an issue first to discuss larger changes. Small fixes and improvements can be submitted as pull requests. Keep changes focused and include tests when appropriate.

## License

This project is released under the BSD-3-Clause license. See the `LICENSE` file for details.