{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

import           Lib                 ( defaultOptions
                                     , formatLispText
                                     , setIndentWidth
                                     , setInlineMaxWidth
                                     )

import           Options.Applicative

import           System.Exit         ( die )

data CliOptions
  = CliOptions { cliIndent      :: !Int
               , cliInlineWidth :: !Int
               , cliInputPath   :: !(Maybe FilePath)
               , cliOutputPath  :: !(Maybe FilePath)
               }
  deriving ( Eq, Show )

main :: IO ()
main = do
  opts <- execParser optsParser
  runFormatter opts
  where
    optsParser
      = info
        (cliOptionsParser <**> helper)
        (fullDesc
         <> progDesc "Format Lisp-like code"
         <> header "lisp-formatter - a general formatter for Lisp-like languages")

cliOptionsParser :: Parser CliOptions
cliOptionsParser
  = CliOptions
  <$> option
    auto
    (long "indent"
     <> metavar "N"
     <> value 2
     <> showDefault
     <> help "Number of spaces used for indentation")
  <*> option
    auto
    (long "inline-width"
     <> short 'w'
     <> metavar "N"
     <> value 80
     <> showDefault
     <> help "Maximum width for keeping a form on a single line")
  <*> optional
    (strOption
       (long "in"
        <> short 'i'
        <> metavar "FILE"
        <> help "Read from FILE instead of stdin (use '-' for stdin)"))
  <*> optional
    (strOption
       (long "out"
        <> short 'o'
        <> metavar "FILE"
        <> help "Write to FILE instead of stdout (use '-' for stdout)"))

runFormatter :: CliOptions -> IO ()
runFormatter cli = do
  let opts
        = setInlineMaxWidth (cliInlineWidth cli) $ setIndentWidth (cliIndent cli) $ defaultOptions
  input <- readInput (cliInputPath cli)
  case formatLispText opts input of
    Left err     -> die ("Failed to format input: " ++ show err)
    Right output -> writeOutput (cliOutputPath cli) output

readInput :: Maybe FilePath -> IO T.Text
readInput Nothing     = TIO.getContents
readInput (Just "-")  = TIO.getContents
readInput (Just path) = TIO.readFile path

writeOutput :: Maybe FilePath -> T.Text -> IO ()
writeOutput target text = case target of
  Nothing   -> writeStdout text
  Just "-"  -> writeStdout text
  Just path -> TIO.writeFile path text
  where
    writeStdout t
      | T.isSuffixOf "\n" t = TIO.putStr t
      | otherwise = TIO.putStrLn t
