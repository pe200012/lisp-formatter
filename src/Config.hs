{-# LANGUAGE OverloadedStrings #-}

module Config
  ( readFormatOptionsFromFile
  , setIndentWidth
  , setInlineMaxWidth
  , setSpecialInlineHead
  , removeSpecialInlineHead
  , defaultOptions
  ) where

import           Control.Exception ( SomeException )
import qualified Control.Exception as E

import           Data.Text         ( Text )
import qualified Data.Text         as T

import           Dhall             ( auto, input )

import           Types             ( FormatOptions(..) )

-- | Default formatter options.
defaultOptions :: FormatOptions
defaultOptions
  = FormatOptions
  { indentWidth        = 2
  , inlineMaxWidth     = 80
  , specialInlineHeads
      = [ ( "if", 2 )      -- condition and then-branch inline
        , ( "cond", 1 )    -- first condition inline
        , ( "define", 2 )  -- name and value inline
        , ( "let", 1 )     -- bindings inline
        , ( "lambda", 1 )  -- params inline
        , ( "defn", 2 )    -- name and params inline (Clojure)
        , ( "defmacro", 2 ) -- name and params inline (Clojure)
        ]
  }

-- | Update the indentation width, clamping to non-negative values.
setIndentWidth :: Int -> FormatOptions -> FormatOptions
setIndentWidth n opts = opts { indentWidth = max 0 n }

-- | Update the inline max width, ensuring it stays positive.
setInlineMaxWidth :: Int -> FormatOptions -> FormatOptions
setInlineMaxWidth n opts = opts { inlineMaxWidth = max 1 n }

-- | Add or update a special inline head rule.
setSpecialInlineHead :: Text -> Int -> FormatOptions -> FormatOptions
setSpecialInlineHead atomName count opts
  = opts { specialInlineHeads = ( atomName, count )
             : filter ((/= atomName) . fst) (specialInlineHeads opts)
         }

-- | Remove a special inline head rule.
removeSpecialInlineHead :: Text -> FormatOptions -> FormatOptions
removeSpecialInlineHead atomName opts
  = opts { specialInlineHeads = filter ((/= atomName) . fst) (specialInlineHeads opts) }

-- | Read FormatOptions from a Dhall file. Returns default options if file doesn't exist or fails to parse.
readFormatOptionsFromFile :: FilePath -> IO FormatOptions
readFormatOptionsFromFile path = do
  result <- E.try (input auto (T.pack ("./" ++ path))) :: IO (Either SomeException FormatOptions)
  case result of
    Left _     -> pure defaultOptions
    Right opts -> pure opts