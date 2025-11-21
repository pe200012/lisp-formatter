{-# LANGUAGE OverloadedStrings #-}

module Config
  ( readFormatOptionsFromFile
  , setIndentWidth
  , setInlineMaxWidth
  , setDefaultStyle
  , setSpecialInlineHead
  , removeSpecialInlineHead
  , defaultOptions
  ) where

import           Control.Exception ( SomeException, displayException )
import qualified Control.Exception as E

import           Data.Text         ( Text )
import qualified Data.Text         as T

import           Dhall             ( auto, input )

import           System.IO         ( hPutStrLn, stderr )

import           Types             ( FormatOptions(..), FormatStyle(..), Special(..) )

-- | Default formatter options.
defaultOptions :: FormatOptions
defaultOptions
  = FormatOptions
  { indentWidth    = 2
  , inlineMaxWidth = 80
  , defaultStyle   = InlineHeadOneline 1
  , specials
      = [ Special { atom = "if", style = InlineHeadOneline 1 }
        , Special { atom = "cond", style = InlineHeadOneline 1 }
        , Special { atom = "define", style = InlineHeadOneline 2 }
        , Special { atom = "let", style = InlineHeadOneline 1 }
        , Special { atom = "lambda", style = InlineHeadOneline 1 }
        , Special { atom = "defn", style = InlineHeadOneline 2 }
        , Special { atom = "defmacro", style = InlineHeadOneline 2 }
        , Special { atom = "do", style = NewlineAlign 0 }
        ]
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
  result <- E.try (input auto (T.pack ("./" ++ path))) :: IO (Either SomeException FormatOptions)
  case result of
    Left err   -> do
      hPutStrLn stderr
        $ "Warning: Failed to parse config file '" ++ path ++ "': " ++ displayException err
      hPutStrLn stderr "Using default configuration."
      pure defaultOptions
    Right opts -> pure opts