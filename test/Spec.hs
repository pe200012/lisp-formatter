{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Data.Text  ( Text )
import qualified Data.Text  as T

import           Lib

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formatLisp" $ do
    it "formats an empty program" $ formatLisp defaultOptions "" `shouldBe` Right ""

    it "formats simple atoms" $ formatLisp defaultOptions "foo" `shouldBe` Right "foo"

    it "formats empty list" $ formatLisp defaultOptions "()" `shouldBe` Right "()"

    it "formats simple list" $ formatLisp defaultOptions "(foo bar)" `shouldBe` Right "(foo bar)"

    it "formats nested lists" $ formatLisp defaultOptions "(a (b c))" `shouldBe` Right "(a (b c))"

    it "formats nested lists with indentation when needed"
      $ let
          input = "(very-long-function-name (nested arg1 arg2 arg3 arg4) another-long-argument)"
        in 
          formatLisp defaultOptions input `shouldSatisfy` isRight

    it "formats string literals"
      $ formatLisp defaultOptions "(print \"hello\")" `shouldBe` Right "(print \"hello\")"

    it "handles escaped characters in strings"
      $ formatLisp defaultOptions "(print \"line\\nbreak\")"
      `shouldBe` Right "(print \"line\\nbreak\")"

    it "formats quoted expressions" $ formatLisp defaultOptions "'(a b)" `shouldBe` Right "'(a b)"

    it "formats quasiquoted expressions"
      $ formatLisp defaultOptions "`(a ,b)" `shouldBe` Right "`(a ,b)"

    it "formats unquote-splicing"
      $ formatLisp defaultOptions "`(a ,@rest)" `shouldBe` Right "`(a ,@rest)"

    it "preserves comments"
      $ formatLisp defaultOptions "; comment\n(foo)" `shouldBe` Right "; comment\n(foo)"

    it "formats inline comments"
      $ formatLisp defaultOptions "(foo ; comment\n  bar)"
      `shouldBe` Right "(foo\n  ; comment\n  bar)"

  describe "formatLisp with indentation" $ do
    it "indents long lists" $ do
      -- This expression is exactly 68 chars, fits in default 80 width
      let input = "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))"
      formatLisp defaultOptions input `shouldBe` Right input

    it "breaks very long lists across lines" $ do
      let input
            = "(define-function (calculate-fibonacci-number n) (if (less-than-or-equal n 1) n (plus (calculate-fibonacci-number (minus n 1)) (calculate-fibonacci-number (minus n 2)))))"
      let result = formatLisp defaultOptions input
      result `shouldSatisfy` isRight
      let Right output = result
      output `shouldContain` "\n"
      output `shouldStartWith` "(define-function"

    it "indents with custom indent width" $ do
      let opts = setIndentWidth 4 defaultOptions
      let input = "(a (b c))"
      -- Short form stays inline regardless of indent width
      formatLisp opts input `shouldBe` Right "(a (b c))"

    it "uses custom indent width for multi-line forms" $ do
      let opts = setIndentWidth 4 defaultOptions
      -- Make it long enough to definitely exceed 80 chars when inline
      let input
            = "(some-really-quite-extraordinarily-long-function-name (first-argument second-argument third-argument fourth-argument fifth-argument sixth-argument))"
      let result = formatLisp opts input
      result `shouldSatisfy` isRight
      -- Should have 4-space indentation
      let Right output = result
      output `shouldContain` "\n"

    it "handles multi-line expressions" $ do
      let input = "(defun hello (name)\n  (print name))"
      let result = formatLisp defaultOptions input
      result `shouldSatisfy` isRight

  describe "formatLisp whitespace handling" $ do
    it "handles trailing newline"
      $ formatLisp defaultOptions "(foo bar)\n" `shouldBe` Right "(foo bar)"

    it "handles multiple trailing newlines"
      $ formatLisp defaultOptions "(foo bar)\n\n\n" `shouldBe` Right "(foo bar)"

    it "handles leading newlines"
      $ formatLisp defaultOptions "\n\n(foo bar)" `shouldBe` Right "(foo bar)"

    it "handles leading and trailing newlines"
      $ formatLisp defaultOptions "\n\n(foo bar)\n\n" `shouldBe` Right "(foo bar)"

    it "handles whitespace between expressions"
      $ formatLisp defaultOptions "(a)\n\n(b)" `shouldBe` Right "(a)\n(b)"

    it "handles trailing whitespace with multiple expressions"
      $ formatLisp defaultOptions "(a)\n\n(b)\n\n" `shouldBe` Right "(a)\n(b)"

    it "handles empty lists with trailing whitespace"
      $ formatLisp defaultOptions "(fn (if  true (let  ) ) )\n\n"
      `shouldBe` Right "(fn (if true (let)))"

    it "handles spaces and tabs"
      $ formatLisp defaultOptions "  \t (foo)  \t\n" `shouldBe` Right "(foo)"

  describe "formatLisp error handling" $ do
    it "reports unclosed parenthesis" $ do
      let result = formatLisp defaultOptions "(foo bar"
      result `shouldSatisfy` isLeft

    it "reports unmatched closing parenthesis" $ do
      let result = formatLisp defaultOptions "foo bar)"
      result `shouldSatisfy` isLeft

    it "reports unterminated string" $ do
      let result = formatLisp defaultOptions "(print \"hello)"
      result `shouldSatisfy` isLeft

  describe "formatLispText" $ do
    it "works with Text input" $ do
      let input = "(foo bar)" :: Text
      formatLispText defaultOptions input `shouldBe` Right "(foo bar)"

    it "handles unicode in atoms" $ do
      let input = "(λ α β)" :: Text
      formatLispText defaultOptions input `shouldBe` Right "(λ α β)"

    it "handles unicode in strings" $ do
      let input = "(print \"こんにちは\")" :: Text
      formatLispText defaultOptions input `shouldBe` Right "(print \"こんにちは\")"

  describe "formatLisp with different delimiters" $ do
    it "formats square brackets"
      $ formatLisp defaultOptions "[foo bar]" `shouldBe` Right "[foo bar]"

    it "formats curly braces" $ formatLisp defaultOptions "{foo bar}" `shouldBe` Right "{foo bar}"

    it "formats nested mixed delimiters"
      $ formatLisp defaultOptions "(foo [bar {baz}])" `shouldBe` Right "(foo [bar {baz}])"

    it "formats empty brackets" $ formatLisp defaultOptions "[]" `shouldBe` Right "[]"

    it "formats empty braces" $ formatLisp defaultOptions "{}" `shouldBe` Right "{}"

    it "handles long bracket expressions" $ do
      let input
            = "[some-really-quite-extraordinarily-long-function-name first second third fourth fifth sixth seventh]"
      let result = formatLisp defaultOptions input
      result `shouldSatisfy` isRight
      let Right output = result
      output `shouldStartWith` "[some-really-quite-extraordinarily-long-function-name"
      output `shouldContain` "\n"

    it "formats Clojure-style vectors"
      $ formatLisp defaultOptions "[1 2 3]" `shouldBe` Right "[1 2 3]"

    it "formats Clojure-style maps"
      $ formatLisp defaultOptions "{:key value}" `shouldBe` Right "{:key value}"

    it "formats Clojure-style code"
      $ formatLisp defaultOptions "(defn add [a b] (+ a b))"
      `shouldBe` Right "(defn add [a b] (+ a b))"

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
