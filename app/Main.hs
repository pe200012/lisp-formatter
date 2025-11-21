{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

import           Lib                 ( defaultOptions
                                     , formatLispText
                                     , readFormatOptionsFromPath
                                     , setIndentWidth
                                     , setInlineMaxWidth
                                     )

import           Options.Applicative

import           System.Exit         ( die )

data CliOptions
  = CliOptions { cliIndent      :: !(Maybe Int)
               , cliInlineWidth :: !(Maybe Int)
               , cliConfigPath  :: !(Maybe FilePath)
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
  <$> optional
    (option auto (long "indent" <> metavar "N" <> help "Number of spaces used for indentation"))
  <*> optional
    (option
       auto
       (long "inline-width"
        <> short 'w'
        <> metavar "N"
        <> help "Maximum width for keeping a form on a single line"))
  <*> optional
    (strOption
       (long "config"
        <> short 'c'
        <> metavar "FILE"
        <> help
          "Path to config file (searches current directory up to root, then home if not specified)"))
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
  dhallOpts <- readFormatOptionsFromPath (cliConfigPath cli)
  let baseOpts = case ( cliIndent cli, cliInlineWidth cli ) of
        ( Nothing, Nothing ) -> dhallOpts  -- Use Dhall options if no CLI options provided
        _ -> defaultOptions  -- Start with defaults if any CLI option provided
  let optsWithIndent = maybe baseOpts (`setIndentWidth` baseOpts) (cliIndent cli)
  let finalOpts = maybe optsWithIndent (`setInlineMaxWidth` optsWithIndent) (cliInlineWidth cli)
  input <- readInput (cliInputPath cli)
  case formatLispText finalOpts input of
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
