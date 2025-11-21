{-# LANGUAGE OverloadedStrings #-}

module Config
  ( readFormatOptionsFromFile
  , readFormatOptions
  , readFormatOptionsFromPath
  , setIndentWidth
  , setInlineMaxWidth
  , setDefaultStyle
  , setDefaultAlign
  , setAlignRule
  , removeAlignRule
  , setPreserveBlankLines
  , setSpecialInlineHead
  , removeSpecialInlineHead
  , defaultOptions
  ) where

import           Control.Exception ( SomeException, displayException )
import qualified Control.Exception as E

import           Data.Text         ( Text )
import qualified Data.Text         as T

import           Dhall             ( auto, input )

import           System.Directory  ( doesFileExist, getCurrentDirectory, getHomeDirectory )
import           System.FilePath   ( (</>), isAbsolute, takeDirectory )
import           System.IO         ( hPutStrLn, stderr )

import           Types             ( AlignRule(..)
                                   , AlignStyle(..)
                                   , FormatOptions(..)
                                   , FormatStyle(..)
                                   , Special(..)
                                   )

-- | Default formatter options.
defaultOptions :: FormatOptions
defaultOptions
  = FormatOptions
  { indentWidth = 2
  , inlineMaxWidth = 80
  , defaultStyle = TryInline
  , defaultAlign = Normal
  , specials = [ Special { atom = "if", style = InlineHead 1 }
               , Special { atom = "cond", style = InlineHead 1 }
               , Special { atom = "define", style = InlineHead 2 }
               , Special { atom = "let", style = BindingsHead 1 }
               , Special { atom = "lambda", style = InlineHead 1 }
               , Special { atom = "defn", style = TryInline }
               , Special { atom = "defmacro", style = InlineHead 2 }
               , Special { atom = "do", style = Newline }
               ]
  , aligns = [ AlignRule { alignAtom = "if", alignStyle = Align } ]
  , preserveBlankLines = True
  }

-- | Update the indentation width, clamping to non-negative values.
setIndentWidth :: Int -> FormatOptions -> FormatOptions
setIndentWidth n opts = opts { indentWidth = max 0 n }

-- | Update the inline max width, ensuring it stays positive.
setInlineMaxWidth :: Int -> FormatOptions -> FormatOptions
setInlineMaxWidth n opts = opts { inlineMaxWidth = max 1 n }

-- | Update the default formatting style for unknown atoms.
setDefaultStyle :: FormatStyle -> FormatOptions -> FormatOptions
setDefaultStyle formatStyle opts = opts { defaultStyle = formatStyle }

-- | Update whether to preserve blank lines.
setPreserveBlankLines :: Bool -> FormatOptions -> FormatOptions
setPreserveBlankLines preserve opts = opts { preserveBlankLines = preserve }

-- | Set the default align style.
setDefaultAlign :: AlignStyle -> FormatOptions -> FormatOptions
setDefaultAlign align opts = opts { defaultAlign = align }

-- | Add or update an align rule for a specific atom.
setAlignRule :: Text -> AlignStyle -> FormatOptions -> FormatOptions
setAlignRule atomName align opts
  = opts { aligns = AlignRule { alignAtom = atomName, alignStyle = align }
             : filter ((/= atomName) . alignAtom) (aligns opts)
         }

-- | Remove an align rule for a specific atom.
removeAlignRule :: Text -> FormatOptions -> FormatOptions
removeAlignRule atomName opts = opts { aligns = filter ((/= atomName) . alignAtom) (aligns opts) }

-- | Add or update a special inline head rule.
setSpecialInlineHead :: Text -> FormatStyle -> FormatOptions -> FormatOptions
setSpecialInlineHead atomName formatStyle opts
  = opts { specials = Special { atom = atomName, style = formatStyle }
             : filter ((/= atomName) . atom) (specials opts)
         }

-- | Remove a special inline head rule.
removeSpecialInlineHead :: Text -> FormatOptions -> FormatOptions
removeSpecialInlineHead atomName opts
  = opts { specials = filter ((/= atomName) . atom) (specials opts) }

-- | Read FormatOptions from a Dhall file. Returns default options if file doesn't exist or fails to parse.
readFormatOptionsFromFile :: FilePath -> IO FormatOptions
readFormatOptionsFromFile path = do
  -- Normalize path for Dhall: relative paths need ./ prefix to avoid being interpreted as variables
  let normalizedPath
        = if isAbsolute path
          then path
          else "./" ++ path
  result <- E.try (input auto (T.pack normalizedPath)) :: IO (Either SomeException FormatOptions)
  case result of
    Left err   -> do
      hPutStrLn stderr
        $ "Warning: Failed to parse config file '" ++ path ++ "': " ++ displayException err
      hPutStrLn stderr "Using default configuration."
      pure defaultOptions
    Right opts -> pure opts

-- | Find and read FormatOptions from config file, searching in standard locations.
-- Searches: current directory up to root, then user home directory.
-- Returns default options if no config file is found or parsing fails.
readFormatOptions :: IO FormatOptions
readFormatOptions = findConfigFile >>= readFormatOptionsFromFile

-- | Find config file path by searching standard locations.
-- Searches: current directory up to root, then user home directory.
findConfigFile :: IO FilePath
findConfigFile = do
  cwdConfig <- findConfigInHierarchy ".lisp-format"
  case cwdConfig of
    Just path -> pure path
    Nothing   -> do
      homeDir <- getHomeDirectory
      let homeConfig = homeDir </> ".lisp-format"
      exists <- doesFileExist homeConfig
      if exists
        then pure homeConfig
        else pure ".lisp-format"  -- fallback to default

-- | Search for config file starting from current directory up to root.
findConfigInHierarchy :: FilePath -> IO (Maybe FilePath)
findConfigInHierarchy configName = do
  cwd <- getCurrentDirectory
  go cwd
  where
    go dir = do
      let configPath = dir </> configName
      exists <- doesFileExist configPath
      if exists
        then pure (Just configPath)
        else do
          let parent = takeDirectory dir
          if parent == dir  -- reached root (takeDirectory "/" = "/")
            then pure Nothing
            else go parent

-- | Read FormatOptions from a specific config file path, or search for it if Nothing.
readFormatOptionsFromPath :: Maybe FilePath -> IO FormatOptions
readFormatOptionsFromPath Nothing     = readFormatOptions
readFormatOptionsFromPath (Just path) = readFormatOptionsFromFile path