# Test Suite Documentation

## Golden Testing with hspec-golden

This test suite uses [hspec-golden](https://hackage.haskell.org/package/hspec-golden) for regression testing. Golden tests store expected outputs in separate files and compare them against actual outputs on each test run.

### How It Works

1. **First Run**: When a golden test runs for the first time, it creates a golden file in `.golden/<test-name>/golden` with the actual output.

2. **Subsequent Runs**: On future runs, the actual output is compared against the stored golden file. If they match, the test passes. If they differ, the test fails and shows the diff.

3. **Updating Golden Files**: When you intentionally change the formatter behavior, you need to update the golden files. Use the `hgold` CLI tool:
   ```bash
   # Update all golden files
   stack install hspec-golden  # Install hgold CLI (one time only)
   hgold

   # Or manually delete and regenerate
   rm -rf .golden
   stack test
   ```

### Test Structure

- **Basic Formatting**: Tests for atoms, lists, strings, quotes, comments
- **Indentation**: Tests for multi-line formatting and custom indent widths
- **Whitespace Handling**: Tests for leading/trailing whitespace, multiple expressions
- **Error Handling**: Tests for parse errors (unclosed parens, etc.)
- **Unicode Support**: Tests for unicode in atoms and strings
- **Different Delimiters**: Tests for `[]` brackets and `{}` braces (Clojure-style)
- **Special Inline Heads**: Tests for special forms like `if`, `define` with inline args
- **Different Format Styles**: Tests for various formatting strategies (InlineAlign, Newline, TryInline)

### Format Styles

The formatter supports different formatting strategies for special forms:

- **InlineFirst n**: Inline the first `n` arguments, rest on new lines (original behavior)
- **InlineAlign n**: Inline first `n` arguments (unless exceeding line width), rest align with last inlined argument
- **Newline**: Always put arguments on new lines
- **TryInline**: Try to inline all arguments unless exceeding line width

### Adding New Tests

To add a new golden test:

```haskell
describe "my new feature" $ do
  goldenTest "test-name" "(input code)"
```

The `goldenTest` helper:
1. Formats the input with `defaultOptions`
2. Stores/compares the result in `.golden/test-name/golden`
3. Reports success or failure

For tests with custom options:

```haskell
it "custom behavior" $ do
  let opts = setIndentWidth 4 defaultOptions
  let result = case formatLisp opts input of
        Right out -> out
        Left err  -> error $ "Format failed: " ++ show err
  defaultGolden "test-name" result
```

### Benefits

- **Regression Detection**: Automatically catches unintended formatting changes
- **Clear Diffs**: When output changes, you see exactly what changed
- **Easy Review**: Golden files are plain text, easy to review in git diffs
- **Maintainability**: No need to hardcode expected outputs in test code
- **Documentation**: Golden files serve as executable documentation of formatting behavior

### Golden File Management

Golden files are stored in `.golden/` directory:
```
.golden/
  ├── empty-program/
  │   └── golden
  ├── factorial/
  │   └── golden
  ├── special-if/
  │   └── golden
  └── ...
```

Each test gets its own subdirectory with a `golden` file containing the expected output.

### Troubleshooting

**Test fails after a legitimate change:**
- Review the diff to ensure the new behavior is correct
- Run `hgold` to update all golden files
- Or delete specific golden files and re-run tests

**Golden files show unexpected formatting:**
- Check if the formatter logic changed unintentionally
- Verify the input test case is correct
- Review the golden file content directly

**Need to see what changed:**
- When tests fail, hspec-golden shows diffs
- You can also manually diff: `diff .golden/test-name/golden .golden/test-name/actual`
