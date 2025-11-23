{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( FormatError(..)
  , FormatOptions(..)
  , FormatStyle(..)
  , StrategyStyle(..)
  , AlignStyle(..)
  , Special(..)
  , AlignRule(..)
  , QuoteKind(..)
  , DelimiterType(..)
  , SExpr(..)
  , Node(..)
  ) where

import           Data.Text    ( Text )

import           Dhall        ( FromDhall(..) )
import qualified Dhall

import           GHC.Generics ( Generic )

-- | Errors that may occur during formatting.
newtype FormatError = ParseError String
  deriving ( Eq, Show )

-- | Alignment style for arguments.
data AlignStyle
  = Normal  -- ^ Normal indentation
  | Align   -- ^ Align with the beginning of the last inlined argument
  deriving ( Eq, Show, Generic )

instance FromDhall AlignStyle

-- | Formatting style for special forms.
data FormatStyle
  = InlineFirst !Int          -- ^ Inline first N arguments, then force newlines for rest
  | InlineWidth !Int          -- ^ Inline as many arguments as possible until width N is reached
  | Bindings                 -- ^ Bindings (formatted in pairs)
  | Newline                  -- ^ Always newline
  | TryInline                -- ^ Try inline all unless line width not enough
  | Strategy [ StrategyStyle ] -- ^ Try a list of candidate styles in order
  deriving ( Eq, Show, Generic )

instance FromDhall FormatStyle

-- | Individual steps used by the 'Strategy' style.
data StrategyStyle
  = StrategyInlineFirst !Int
  | StrategyInlineWidth !Int
  | StrategyBindings
  | StrategyNewline
  | StrategyTryInline
  deriving ( Eq, Show, Generic )

instance FromDhall StrategyStyle where
  autoWith _ = strategyStyleDecoder

strategyStyleDecoder :: Dhall.Decoder StrategyStyle
strategyStyleDecoder
  = Dhall.union
    (Dhall.constructor "InlineFirst" (StrategyInlineFirst <$> Dhall.auto)
     <> Dhall.constructor "InlineWidth" (StrategyInlineWidth <$> Dhall.auto)
     <> Dhall.constructor "Bindings" (StrategyBindings <$ Dhall.unit)
     <> Dhall.constructor "Newline" (StrategyNewline <$ Dhall.unit)
     <> Dhall.constructor "TryInline" (StrategyTryInline <$ Dhall.unit))

-- | Configuration for the formatter.
data Special = Special { atom :: !Text, style :: !FormatStyle }
  deriving ( Eq, Show, Generic )

instance FromDhall Special

data AlignRule = AlignRule { alignAtom :: !Text, alignStyle :: !AlignStyle }
  deriving ( Eq, Show, Generic )

instance FromDhall AlignRule

data FormatOptions
  = FormatOptions
  { indentWidth :: !Int
  , inlineMaxWidth :: !Int
  , defaultStyle :: !FormatStyle   -- default formatting for atoms not in specials
  , defaultAlign :: !AlignStyle    -- default alignment style
  , specials :: ![ Special ]   -- dedicated type for inline head rules
  , aligns :: ![ AlignRule ] -- alignment rules for specific atoms
  , preserveBlankLines :: !Bool          -- whether to preserve blank lines in source code
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

data Node = NodeExpr SExpr | NodeExprRaw SExpr Text | NodeComment Text | NodeBlankLine Int
  deriving ( Eq, Show )