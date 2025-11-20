{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( FormatError(..)
  , FormatOptions(..)
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
data FormatOptions
  = FormatOptions
  { indentWidth        :: !Int
  , inlineMaxWidth     :: !Int
  , specialInlineHeads :: ![ ( Text, Int ) ]  -- atom name -> number of args to keep inline
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
      = [ ( "if", 1 )      -- condition and then-branch inline
        , ( "define", 2 )  -- name and value inline
        , ( "let", 1 )     -- bindings inline
        , ( "lambda", 1 )  -- params inline
        , ( "defn", 2 )    -- name and params inline (Clojure)
        , ( "defmacro", 2 ) -- name and params inline (Clojure)
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