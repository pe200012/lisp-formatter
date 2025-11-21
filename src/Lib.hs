module Lib
  ( FormatError(..)
  , FormatOptions(..)
  , FormatStyle(..)
  , AlignStyle(..)
  , defaultOptions
  , setIndentWidth
  , setInlineMaxWidth
  , setDefaultStyle
  , setDefaultAlign
  , setAlignRule
  , removeAlignRule
  , setPreserveBlankLines
  , setSpecialInlineHead
  , removeSpecialInlineHead
  , readFormatOptionsFromFile
  , readFormatOptions
  , readFormatOptionsFromPath
  , formatLispText
  , formatLisp
  ) where

import           Config    ( defaultOptions
                           , readFormatOptions
                           , readFormatOptionsFromFile
                           , readFormatOptionsFromPath
                           , removeAlignRule
                           , removeSpecialInlineHead
                           , setAlignRule
                           , setDefaultAlign
                           , setDefaultStyle
                           , setIndentWidth
                           , setInlineMaxWidth
                           , setPreserveBlankLines
                           , setSpecialInlineHead
                           )

import           Data.Text ( Text )
import qualified Data.Text as T

import           Formatter ( renderProgram )

import           Parser    ( parseProgram )

import           Types     ( AlignStyle(..), FormatError(..), FormatOptions, FormatStyle(..) )

-- | Public API for formatting strict 'Text'.
formatLispText :: FormatOptions -> Text -> Either FormatError Text
formatLispText opts txt = case parseProgram txt of
  Left err  -> Left . ParseError $ err
  Right ast -> Right $ renderProgram opts ast

-- | Convenience wrapper around 'formatLispText' for 'String'.
formatLisp :: FormatOptions -> String -> Either FormatError String
formatLisp opts = fmap T.unpack . formatLispText opts . T.pack
