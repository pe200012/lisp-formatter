{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( FormatError(..)
  , FormatOptions(..)
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

-- | Configuration for the formatter.
data Special = Special { atom :: !Text, style :: !Int }
  deriving ( Eq, Show, Generic )

instance FromDhall Special

data FormatOptions
  = FormatOptions { indentWidth        :: !Int
                  , inlineMaxWidth     :: !Int
                  , specialInlineHeads :: ![ Special ]  -- dedicated type for inline head rules
                  }
  deriving ( Eq, Show, Generic )

instance FromDhall FormatOptions

-- | Default formatter options.
defaultOptions :: FormatOptions
defaultOptions
  = FormatOptions
  { indentWidth        = 2
  , inlineMaxWidth     = 80
  , specialInlineHeads
      = [ Special { atom = "if", style = 1 }
        , Special { atom = "define", style = 2 }
        , Special { atom = "let", style = 1 }
        , Special { atom = "lambda", style = 1 }
        , Special { atom = "defn", style = 2 }
        , Special { atom = "defmacro", style = 2 }
        ]
  }

--------------------------------------------------------------------------------
-- AST definitions

data QuoteKind = Quote | Quasiquote | Unquote | UnquoteSplicing
  deriving ( Eq, Show )

data DelimiterType = Paren | Bracket | Brace
  deriving ( Eq, Show )

data SExpr = Atom Text | StringLit Text | List DelimiterType [ Node ] | QuoteExpr QuoteKind SExpr
  deriving ( Eq, Show )

data Node = NodeExpr SExpr | NodeComment Text
  deriving ( Eq, Show )