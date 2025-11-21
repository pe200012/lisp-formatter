{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( FormatError(..)
  , FormatOptions(..)
  , FormatStyle(..)
  , Special(..)
  , QuoteKind(..)
  , DelimiterType(..)
  , SExpr(..)
  , Node(..)
  ) where

import           Data.Text    ( Text )

import           Dhall        ( FromDhall )

import           GHC.Generics ( Generic )

-- | Errors that may occur during formatting.
newtype FormatError = ParseError String
  deriving ( Eq, Show )

-- | Formatting style for special forms.
data FormatStyle
  = InlineHead !Int          -- ^ Inline first N arguments, then force newlines for rest (no line width check for rest)
  | InlineHeadOneline !Int   -- ^ Try inline all first, if fails try inline first N arguments
  | InlineAlign !Int         -- ^ Inline first N arguments (unless exceed line width), rest align with last inlined
  | NewlineAlign !Int        -- ^ Always newline and indent by N
  | TryInline                -- ^ Try inline all unless line width not enough
  deriving ( Eq, Show, Generic )

instance FromDhall FormatStyle

-- | Configuration for the formatter.
data Special = Special { atom :: !Text, style :: !FormatStyle }
  deriving ( Eq, Show, Generic )

instance FromDhall Special

data FormatOptions
  = FormatOptions
  { indentWidth        :: !Int
  , inlineMaxWidth     :: !Int
  , defaultStyle       :: !FormatStyle  -- default formatting for atoms not in specials
  , specials           :: ![ Special ]  -- dedicated type for inline head rules
  , preserveBlankLines :: !Bool         -- whether to preserve blank lines in source code
  }
  deriving ( Eq, Show, Generic )

instance FromDhall FormatOptions

--------------------------------------------------------------------------------
-- AST definitions

data QuoteKind = Quote | Quasiquote | Unquote | UnquoteSplicing
  deriving ( Eq, Show )

data DelimiterType = Paren | Bracket | Brace
  deriving ( Eq, Show )

data SExpr = Atom Text | StringLit Text | List DelimiterType [ Node ] | QuoteExpr QuoteKind SExpr
  deriving ( Eq, Show )

data Node = NodeExpr SExpr | NodeComment Text | NodeBlankLine
  deriving ( Eq, Show )