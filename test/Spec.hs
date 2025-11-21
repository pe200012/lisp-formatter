{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Data.List         ( isSuffixOf )
import           Data.Text         ( Text )
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Lib               ( AlignStyle(..)
                                   , FormatOptions
                                   , FormatStyle(..)
                                   , defaultOptions
                                   , formatLisp
                                   , formatLispText
                                   , readFormatOptionsFromPath
                                   , setAlignRule
                                   , setIndentWidth
                                   , setPreserveBlankLines
                                   , setSpecialInlineHead
                                   )

import           System.Directory  ( doesDirectoryExist, listDirectory )

import           Test.Hspec
import           Test.Hspec.Golden hiding ( output )

main :: IO ()
main = do
  dataFiles <- discoverDataFiles "test/data"
  hspec (spec dataFiles)

discoverDataFiles :: FilePath -> IO [ FilePath ]
discoverDataFiles dir = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      files <- listDirectory dir
      return [ dir ++ "/" ++ f | f <- files, ".lisp" `isSuffixOf` f ]
    else return []

spec :: [ FilePath ] -> Spec
spec dataFiles = do
  describe "formatLisp - basic formatting" $ do
    goldenTest "empty-program" ""
    goldenTest "simple-atom" "foo"
    goldenTest "empty-list" "()"
    goldenTest "simple-list" "(foo bar)"
    goldenTest "nested-lists" "(a (b c))"
    goldenTest "string-literals" "(print \"hello\")"
    goldenTest "escaped-strings" "(print \"line\\nbreak\")"
    goldenTest "quoted-expressions" "'(a b)"
    goldenTest "quasiquoted-expressions" "`(a ,b)"
    goldenTest "unquote-splicing" "`(a ,@rest)"

  describe "formatLisp - comments" $ do
    goldenTest "comment-before-expr" "; comment\n(foo)"
    goldenTest "inline-comment" "(foo ; comment\n  bar)"

  describe "formatLisp - indentation" $ do
    goldenTest "factorial" "(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))"
    goldenTest
      "fibonacci-long"
      "(define-function (calculate-fibonacci-number n) (if (less-than-or-equal n 1) n (plus (calculate-fibonacci-number (minus n 1)) (calculate-fibonacci-number (minus n 2)))))"

    it "uses custom indent width for multi-line forms" $ do
      let opts = setIndentWidth 4 defaultOptions
      let input
            = "(some-really-quite-extraordinarily-long-function-name (first-argument second-argument third-argument fourth-argument fifth-argument sixth-argument))"
      let result = case formatLisp opts input of
            Right out -> out
            Left err  -> error $ "Format failed: " ++ show err
      defaultGolden "custom-indent-width" result

    goldenTest "multi-line-defun" "(defun hello (name)\n  (print name))"

  describe "formatLisp - whitespace handling" $ do
    whitespaceTest "trailing-newline" "(foo bar)\n"
    whitespaceTest "multiple-trailing-newlines" "(foo bar)\n\n\n"
    whitespaceTest "leading-newlines" "\n\n(foo bar)"
    whitespaceTest "leading-and-trailing-newlines" "\n\n(foo bar)\n\n"
    goldenTest "whitespace-between-expressions" "(a)\n\n(b)"
    goldenTest "trailing-whitespace-multiple-expressions" "(a)\n\n(b)\n\n"
    whitespaceTest "empty-lists-trailing-whitespace" "(fn (if  true (let  ) ) )\n\n"
    whitespaceTest "spaces-and-tabs" "  \t (foo)  \t\n"
    whitespaceTest "multiple-toplevel-items" "(define x 1)\n(define y 2)\n(define z 3)"
    whitespaceTest
      "multiple-toplevel-items-mixed"
      "(define x 1)\n(if true (print \"yes\"))\n(define z 3)"

  describe "formatLisp - error handling" $ do
    it "reports unclosed parenthesis" $ do
      let result = formatLisp defaultOptions "(foo bar"
      result `shouldSatisfy` isLeft

    it "reports unmatched closing parenthesis" $ do
      let result = formatLisp defaultOptions "foo bar)"
      result `shouldSatisfy` isLeft

    it "reports unterminated string" $ do
      let result = formatLisp defaultOptions "(print \"hello)"
      result `shouldSatisfy` isLeft

  describe "formatLispText - unicode support" $ do
    it "works with Text input" $ do
      let input = "(foo bar)" :: Text
      formatLispText defaultOptions input `shouldBe` Right "(foo bar)"

    it "handles unicode in atoms" $ do
      let input = "(λ α β)" :: Text
      let result = case formatLispText defaultOptions input of
            Right out -> T.unpack out
            Left err  -> error $ "Format failed: " ++ show err
      defaultGolden "unicode-atoms" result

    it "handles unicode in strings" $ do
      let input = "(print \"こんにちは\")" :: Text
      let result = case formatLispText defaultOptions input of
            Right out -> T.unpack out
            Left err  -> error $ "Format failed: " ++ show err
      defaultGolden "unicode-strings" result

  describe "formatLisp - different delimiters" $ do
    goldenTest "square-brackets" "[foo bar]"
    goldenTest "curly-braces" "{foo bar}"
    goldenTest "nested-mixed-delimiters" "(foo [bar {baz}])"
    goldenTest "empty-brackets" "[]"
    goldenTest "empty-braces" "{}"
    goldenTest
      "long-bracket-expression"
      "[some-really-quite-extraordinarily-long-function-name first second third fourth fifth sixth seventh]"
    goldenTest "clojure-vectors" "[1 2 3]"
    goldenTest "clojure-maps" "{:key value}"
    goldenTest "clojure-defn" "(defn add [a b] (+ a b))"

  describe "formatLisp - special inline heads" $ do
    it "formats if with first arg inline" $ do
      let input = "(if (> x 10) (print \"big\") (print \"small\"))"
      let result = case formatLisp defaultOptions input of
            Right out -> out
            Left err  -> error $ "Format failed: " ++ show err
      defaultGolden "special-if" result

    it "formats define with name and value inline" $ do
      let input
            = "(define my-long-variable-name (some-computation-that-is-quite-lengthy arg1 arg2 arg3))"
      let result = case formatLisp defaultOptions input of
            Right out -> out
            Left err  -> error $ "Format failed: " ++ show err
      defaultGolden "special-define" result

  describe "formatLisp - different format styles" $ do
    it "formats with InlineHead + Align style" $ do
      let opts
            = setAlignRule "test" Align $ setSpecialInlineHead "test" (InlineHead 2) defaultOptions
          input  = "(test arg1 arg2 arg3 arg4 arg5)"
          result = case formatLisp opts input of
            Right out -> out
            Left err  -> error $ "Format failed: " ++ show err
      defaultGolden "inline-align-style" result

    it "formats with NewlineAlign style" $ do
      let opts   = setSpecialInlineHead "test" (NewlineAlign 1) defaultOptions
          input  = "(test arg1 arg2 arg3)"
          result = case formatLisp opts input of
            Right out -> out
            Left err  -> error $ "Format failed: " ++ show err
      defaultGolden "newline-align-style" result

    it "formats with TryInline style" $ do
      let opts   = setSpecialInlineHead "test" TryInline defaultOptions
          input  = "(test arg1 arg2 arg3)"
          result = case formatLisp opts input of
            Right out -> out
            Left err  -> error $ "Format failed: " ++ show err
      defaultGolden "try-inline-style" result

  describe "formatLisp - test data files" $ mapM_ testDataFile dataFiles

-- Helper functions
testDataFile :: FilePath -> Spec
testDataFile filePath = do
  input <- runIO (T.unpack <$> TIO.readFile filePath)
  configPath <- runIO (readFormatOptionsFromPath (Just configFilePath))
  it ("formats " ++ name) $ do
    let result = case formatLisp configPath input of
          Right out -> out
          Left err  -> error $ "Format failed: " ++ show err
    defaultGolden name result
  where
    name           = reverse . takeWhile (/= '/') . reverse . takeWhile (/= '.') $ filePath

    configFilePath = filePath ++ "-format"

goldenTest :: String -> String -> Spec
goldenTest name input = it ("formats " ++ name) $ do
  let result = case formatLisp defaultOptions input of
        Right out -> out
        Left err  -> error $ "Format failed: " ++ show err
  defaultGolden name result

whitespaceTest :: String -> String -> Spec
whitespaceTest name input = it ("formats " ++ name) $ do
  let opts = setPreserveBlankLines False defaultOptions
  let result = case formatLisp opts input of
        Right out -> out
        Left err  -> error $ "Format failed: " ++ show err
  defaultGolden name result

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
