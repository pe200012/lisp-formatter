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

import           Data.List  ( find )
import           Data.Maybe ( fromMaybe )
import           Data.Text  ( Text )
import qualified Data.Text  as T

import           Types      ( DelimiterType(..)
                            , FormatOptions(..)
                            , FormatStyle(..)
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
    | Just ( lineHead, restNodes, alignCol ) <- renderSpecialInlineAlign opts level delim nodes
      -> finalize (lineHead : formatRestAligned alignCol restNodes) nodes
    | Just ( lineHead, restNodes ) <- renderSpecialInlineHead opts level delim nodes
      -> finalize (lineHead : formatRest restNodes) nodes
    | Just inline <- renderInlineSimple opts level delim nodes -> [ RenderLine inline KindExpr ]
    | Just ( lineHead, restNodes ) <- renderInlineHead opts level delim nodes
      -> finalize (lineHead : formatRest restNodes) nodes
    | otherwise -> case nodes of
      (NodeExpr (Atom atomName) : args) -> case defaultStyle opts of
        InlineHead n        -> maybe
          (finalize (baseLine : body) nodes)
          (\( lh, rest ) -> finalize (lh : formatRest rest) nodes)
          (renderInlineHeadStyle opts level delim atomName args n)
        InlineHeadOneline n -> maybe
          (finalize (baseLine : body) nodes)
          (\( lh, rest ) -> finalize (lh : formatRest rest) nodes)
          (renderInlineHeadOnelineStyle opts level delim atomName args n)
        InlineAlign n       -> maybe
          (finalize (baseLine : body) nodes)
          (\( lh, rest, alignCol ) -> finalize (lh : formatRestAligned alignCol rest) nodes)
          (renderInlineAlignStyle opts level delim atomName args n)
        NewlineAlign n      -> maybe
          (finalize (baseLine : body) nodes)
          (\( lh, rest ) -> finalize (lh : formatRest rest) nodes)
          (renderNewlineAlignStyle opts level delim atomName args n)
        TryInline           -> maybe
          (finalize (baseLine : body) nodes)
          (\( lh, rest ) -> finalize (lh : formatRest rest) nodes)
          (renderTryInlineStyle opts level delim atomName args)
      _ -> finalize (baseLine : body) nodes
  where
    indentCur = indentText opts level

    ( openDelim, closeDelim ) = delimiterPair delim

    baseLine = RenderLine (indentCur <> openDelim) KindExpr

    body = concatMap (formatNode opts (level + 1)) nodes

    formatRest = concatMap (formatNode opts (level + 1))

    formatRestAligned alignCol = concatMap formatAligned
      where
        alignIndent = T.replicate alignCol " "

        formatAligned (NodeExpr expr)   = case renderCompactExpr expr of
          Just compact -> [ RenderLine (alignIndent <> compact) KindExpr ]
          Nothing      -> formatNode opts (level + 1) (NodeExpr expr)  -- Fall back to regular formatting
        formatAligned (NodeComment txt) = [ RenderLine (alignIndent <> ";" <> txt) KindComment ]

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

    renderSpecialInlineAlign o lvl d (NodeExpr (Atom atomName) : args) = do
      headRule <- find ((== atomName) . atom) (specials o)
      case style headRule of
        InlineAlign n -> renderInlineAlignStyle o lvl d atomName args n
        _ -> Nothing
    renderSpecialInlineAlign _ _ _ _ = Nothing

    renderSpecialInlineHead o lvl d (NodeExpr (Atom atomName) : args) = do
      headRule <- find ((== atomName) . atom) (specials o)
      case style headRule of
        InlineHead n        -> renderInlineHeadStyle o lvl d atomName args n
        InlineHeadOneline n -> renderInlineHeadOnelineStyle o lvl d atomName args n
        NewlineAlign n      -> renderNewlineAlignStyle o lvl d atomName args n
        TryInline           -> renderTryInlineStyle o lvl d atomName args
        InlineAlign _       -> Nothing  -- Handled separately by renderSpecialInlineAlign
    renderSpecialInlineHead _ _ _ _ = Nothing

    -- InlineHead: Always inline first n arguments, then force newlines for rest
    renderInlineHeadStyle o lvl _ atomName args inlineCount = do
      let ( inlineArgs, restArgs ) = splitAt inlineCount args
      inlineParts <- traverse renderCompactNode inlineArgs
      let atomText
            = indentText o lvl <> openDelim <> atomName <> " " <> T.intercalate " " inlineParts
      if T.length atomText <= inlineMaxWidth o
        then Just ( RenderLine atomText KindExpr, restArgs )
        else Nothing

    -- InlineHeadOneline: Try to inline all first, if that fails try inline first n
    renderInlineHeadOnelineStyle o lvl d atomName args inlineCount = do
      -- First try to inline everything
      case renderTryInlineStyle o lvl d atomName args of
        Just result -> Just result
        Nothing     -> do
          -- Fall back to inlining first n
          let ( inlineArgs, restArgs ) = splitAt inlineCount args
          inlineParts <- traverse renderCompactNode inlineArgs
          let atomText
                = indentText o lvl <> openDelim <> atomName <> " " <> T.intercalate " " inlineParts
          if T.length atomText <= inlineMaxWidth o
            then Just ( RenderLine atomText KindExpr, restArgs )
            else Nothing

    renderInlineAlignStyle o lvl _ atomName args alignCount = do
      let ( inlineArgs, restArgs ) = splitAt alignCount args
      inlineParts <- traverse renderCompactNode inlineArgs
      let atomText
            = indentText o lvl <> openDelim <> atomName <> " " <> T.intercalate " " inlineParts
          -- Calculate where the last inlined argument starts
          atomAndPrefix    = indentText o lvl <> openDelim <> atomName <> " "
          allButLastInline = take (length inlineParts - 1) inlineParts
          beforeLastLength
            = if null allButLastInline
              then 0
              else T.length (T.intercalate " " allButLastInline) + 1  -- +1 for the space
          alignColumn      = T.length atomAndPrefix + beforeLastLength
      if T.length atomText <= inlineMaxWidth o
        then Just ( RenderLine atomText KindExpr, restArgs, alignColumn )
        else Nothing  -- Fall back to regular formatting

    renderNewlineAlignStyle o lvl _ atomName args _
      = let
          firstLine = RenderLine (indentText o lvl <> openDelim <> atomName) KindExpr
        in 
          Just ( firstLine, args )  -- Always format with newline

    renderTryInlineStyle o lvl _ atomName args = do
      inlineParts <- traverse renderCompactNode args
      let atomText
            = indentText o lvl <> openDelim <> atomName <> " " <> T.intercalate " " inlineParts
      if T.length atomText <= inlineMaxWidth o
        then Just ( RenderLine atomText KindExpr, [] )
        else Nothing  -- Fall back to regular formatting

    renderCompactNode (NodeExpr expr) = renderCompactExpr expr
    renderCompactNode (NodeComment _) = Nothing

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