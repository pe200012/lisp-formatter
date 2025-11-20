module Config
  ( readFormatOptionsFromFile
  , setIndentWidth
  , setInlineMaxWidth
  , defaultOptions
  ) where

import           Control.Exception ( SomeException )
import qualified Control.Exception as E

import qualified Data.Text         as T

import           Dhall             ( auto, input )

import           Types             ( FormatOptions(..) )

-- | Default formatter options.
defaultOptions :: FormatOptions
defaultOptions = FormatOptions { indentWidth = 2, inlineMaxWidth = 80 }

-- | Update the indentation width, clamping to non-negative values.
setIndentWidth :: Int -> FormatOptions -> FormatOptions
setIndentWidth n opts = opts { indentWidth = max 0 n }

-- | Update the inline max width, ensuring it stays positive.
setInlineMaxWidth :: Int -> FormatOptions -> FormatOptions
setInlineMaxWidth n opts = opts { inlineMaxWidth = max 1 n }

-- | Read FormatOptions from a Dhall file. Returns default options if file doesn't exist or fails to parse.
readFormatOptionsFromFile :: FilePath -> IO FormatOptions
readFormatOptionsFromFile path = do
  result <- E.try (input auto (T.pack ("./" ++ path))) :: IO (Either SomeException FormatOptions)
  case result of
    Left _     -> pure defaultOptions
    Right opts -> pure opts