{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Formatter
  ( renderProgram
  , formatNode
  , formatExpr
  , formatList
  , exprLine
  , indentText
  , delimiterPair
  , emptyDelim
  , quotePrefix
  , encodeString
  ) where

import           Data.List  ( find, last )
import           Data.Maybe ( fromMaybe )
import           Data.Text  ( Text )
import qualified Data.Text  as T

import           Types      ( DelimiterType(..)
                            , FormatOptions(..)
                            , Node(..)
                            , QuoteKind(..)
                            , SExpr(..)
                            , Special(..)
                            )

--------------------------------------------------------------------------------
-- Main formatting functions

renderProgram :: FormatOptions -> [ Node ] -> Text
renderProgram opts = T.intercalate "\n" . map rlText . concatMap (formatNode opts 0)

formatNode :: FormatOptions -> Int -> Node -> [ RenderLine ]
formatNode opts level = \case
  NodeComment txt -> [ RenderLine (indentText opts level <> ";" <> txt) KindComment ]
  NodeExpr expr   -> formatExpr opts level expr

formatExpr :: FormatOptions -> Int -> SExpr -> [ RenderLine ]
formatExpr opts level = \case
  Atom t -> [ exprLine opts level t ]
  StringLit t -> [ exprLine opts level (encodeString t) ]
  QuoteExpr kind expr -> case formatExpr opts level expr of
    [] -> [ exprLine opts level (quotePrefix kind) ]
    (line0 : rest) -> let
        prefix   = indentText opts level <> quotePrefix kind
        stripped = fromMaybe (rlText line0) (T.stripPrefix (indentText opts level) (rlText line0))
        newFirst = line0 { rlText = prefix <> stripped }
      in 
        newFirst : rest
  List delim nodes -> formatList opts level delim nodes

formatList :: FormatOptions -> Int -> DelimiterType -> [ Node ] -> [ RenderLine ]
formatList opts level delim nodes = case nodes of
  [] -> [ exprLine opts level (emptyDelim delim) ]
  _
    | Just inline <- renderInlineSimple opts level delim nodes -> [ RenderLine inline KindExpr ]
    | Just ( lineHead, restNodes ) <- renderSpecialInlineHead opts level delim nodes
      -> finalize (lineHead : formatRest restNodes) nodes
    | Just ( lineHead, restNodes ) <- renderInlineHead opts level delim nodes
      -> finalize (lineHead : formatRest restNodes) nodes
    | otherwise -> finalize (baseLine : body) nodes
  where
    indentCur = indentText opts level

    ( openDelim, closeDelim ) = delimiterPair delim

    baseLine = RenderLine (indentCur <> openDelim) KindExpr

    body = concatMap (formatNode opts (level + 1)) nodes

    formatRest = concatMap (formatNode opts (level + 1))

    finalize ls ns
      = if isLastComment ns
        then ls ++ [ RenderLine (indentCur <> closeDelim) KindExpr ]
        else attachClosing ls closeDelim

    renderInlineSimple o lvl d ns = do
      inline <- renderCompactListSimple d ns
      let lineText = indentText o lvl <> inline
      if T.length lineText <= inlineMaxWidth o
        then Just lineText
        else Nothing

    renderInlineHead o lvl _ (NodeExpr first : rest) = do
      inlineFirst <- renderCompactExpr first
      let firstText = indentText o lvl <> openDelim <> inlineFirst
      if T.length firstText <= inlineMaxWidth o
        then Just ( RenderLine firstText KindExpr, rest )
        else Nothing
    renderInlineHead _ _ _ _ = Nothing

    renderSpecialInlineHead o lvl d (NodeExpr (Atom atomName) : args) = do
      headRule <- find ((== atomName) . atom) (specialInlineHeads o)
      let inlineCount = style headRule
          ( inlineArgs, restArgs ) = splitAt inlineCount args
      inlineParts <- traverse renderCompactNode inlineArgs
      let atomText
            = indentText o lvl <> openDelim <> atomName <> " " <> T.intercalate " " inlineParts
      if T.length atomText <= inlineMaxWidth o
        then Just ( RenderLine atomText KindExpr, restArgs )
        else Nothing
      where
        renderCompactNode (NodeExpr expr) = renderCompactExpr expr
        renderCompactNode (NodeComment _) = Nothing
    renderSpecialInlineHead _ _ _ _ = Nothing

--------------------------------------------------------------------------------
-- Helper data types and functions

data LineKind = KindExpr | KindComment
  deriving ( Eq, Show )

data RenderLine = RenderLine { rlText :: !Text, rlKind :: !LineKind }

isLastComment :: [ Node ] -> Bool
isLastComment [] = False
isLastComment ns = case last ns of
  NodeComment _ -> True
  _ -> False

attachClosing :: [ RenderLine ] -> Text -> [ RenderLine ]
attachClosing [] closingText   = [ RenderLine closingText KindExpr ]
attachClosing rows closingText = case breakLastExpr rows of
  Nothing -> rows ++ [ RenderLine closingText KindExpr ]
  Just ( prefix, lastExpr, suffix )
    -> prefix ++ [ lastExpr { rlText = rlText lastExpr <> closingText } ] ++ suffix

breakLastExpr :: [ RenderLine ] -> Maybe ( [ RenderLine ], RenderLine, [ RenderLine ] )
breakLastExpr xs = go [] (reverse xs)
  where
    go _ [] = Nothing
    go suffix (y : ys)
      | rlKind y == KindExpr = Just ( reverse ys, y, reverse suffix )
      | otherwise = go (y : suffix) ys

exprLine :: FormatOptions -> Int -> Text -> RenderLine
exprLine opts level content = RenderLine (indentText opts level <> content) KindExpr

indentText :: FormatOptions -> Int -> Text
indentText opts level = T.replicate (level * max 0 (indentWidth opts)) " "

delimiterPair :: DelimiterType -> ( Text, Text )
delimiterPair = \case
  Paren   -> ( "(", ")" )
  Bracket -> ( "[", "]" )
  Brace   -> ( "{", "}" )

emptyDelim :: DelimiterType -> Text
emptyDelim delim
  = let
      ( open, close ) = delimiterPair delim
    in 
      open <> close

renderCompactListSimple :: DelimiterType -> [ Node ] -> Maybe Text
renderCompactListSimple delim nodes = do
  parts <- traverse inline nodes
  let ( open, close ) = delimiterPair delim
  pure $ open <> T.intercalate " " parts <> close
  where
    inline (NodeExpr expr) = renderCompactExpr expr
    inline _ = Nothing

renderCompactExpr :: SExpr -> Maybe Text
renderCompactExpr = \case
  Atom t -> Just t
  StringLit t -> Just (encodeString t)
  QuoteExpr kind expr -> do
    inner <- renderCompactExpr expr
    Just (quotePrefix kind <> inner)
  List delim nodes -> do
    parts <- traverse renderNode nodes
    let ( open, close ) = delimiterPair delim
    Just $ open <> T.intercalate " " parts <> close
    where
      renderNode (NodeExpr e)   = renderCompactExpr e
      renderNode NodeComment {} = Nothing

quotePrefix :: QuoteKind -> Text
quotePrefix = \case
  Quote           -> "'"
  Quasiquote      -> "`"
  Unquote         -> ","
  UnquoteSplicing -> ",@"

encodeString :: Text -> Text
encodeString = (<> "\"") . ("\"" <>) . T.concatMap encodeChar
  where
    encodeChar '"'  = "\\\""
    encodeChar '\\' = "\\\\"
    encodeChar '\n' = "\\n"
    encodeChar '\r' = "\\r"
    encodeChar '\t' = "\\t"
    encodeChar c    = T.singleton c