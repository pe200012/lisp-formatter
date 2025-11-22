{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Data.Foldable     ( for_ )
import           Data.List         ( isSuffixOf )
import           Data.Text         ( Text )
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           Lib               ( defaultOptions
                                   , formatLisp
                                   , formatLispText
                                   , parseProgram
                                   , readFormatOptionsFromPath
                                   )

import           System.Directory  ( doesDirectoryExist, doesFileExist, listDirectory )
import           System.FilePath   ( (</>), replaceExtension, takeBaseName )

import           Test.Hspec
import           Test.Hspec.Golden hiding ( output )

main :: IO ()
main = do
  hspec spec

discoverDataFiles :: FilePath -> IO [ FilePath ]
discoverDataFiles dir = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      files <- listDirectory dir
      return [ dir ++ "/" ++ f | f <- files, ".lisp" `isSuffixOf` f ]
    else return []

spec :: Spec
spec = do
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

  describe "formatLisp - normal" (formatTest "test/format/normal")

  describe "formatLisp - whitespace" (formatTest "test/format/whitespace")

  describe "parseProgram - parser tests" parserTest

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

parserTest :: Spec
parserTest = do
  files <- runIO (discoverDataFiles "test/parser")
  for_ files $ \filePath -> do
    input <- runIO (T.unpack <$> TIO.readFile filePath)
    let name = takeBaseName filePath
    it ("parses " <> name) $ do
      let result = case parseProgram (T.pack input) of
            Left err  -> error $ "Parse failed: " ++ err
            Right ast -> show ast
      defaultGolden ("parse-" <> name) result

formatTest :: FilePath -> Spec
formatTest folder = do
  files <- runIO (discoverDataFiles folder)
  for_ files $ \filePath -> do
    input <- runIO (T.unpack <$> TIO.readFile filePath)
    let name           = takeBaseName filePath
        configFilePath = replaceExtension filePath ".lisp-format"
        folderConfig   = folder </> ".lisp-format"
    specficConfigExists <- runIO (doesFileExist configFilePath)
    configPath <- if specficConfigExists
      then runIO (readFormatOptionsFromPath (Just configFilePath))
      else runIO (readFormatOptionsFromPath (Just folderConfig))
    it ("formats " <> name) $ do
      let result = case formatLisp configPath input of
            Right out -> out
            Left err  -> error $ "Format failed: " ++ show err
      defaultGolden name result
