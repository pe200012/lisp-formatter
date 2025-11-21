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

import           Types      ( AlignRule(..)
                            , AlignStyle(..)
                            , DelimiterType(..)
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
renderProgram opts nodes = T.intercalate "\n" $ concat $ addSeparators formattedNodes
  where
    -- Format each node to its text lines
    formattedNodes = map (\node -> ( node, map rlText $ formatNode opts 0 node )) nodes

    -- Group consecutive formatted outputs, inserting empty lines between top-level expressions
    addSeparators [] = []
    addSeparators [ ( _, nodeLines ) ] = [ nodeLines ]
    addSeparators (( node1, lines1 ) : ( node2, lines2 ) : rest) = case ( node1, node2 ) of
      ( NodeExpr _, NodeExpr _ ) -> lines1 : [ "" ] : addSeparators (( node2, lines2 ) : rest)
      _ -> lines1 : addSeparators (( node2, lines2 ) : rest)

formatNode :: FormatOptions -> Int -> Node -> [ RenderLine ]
formatNode opts level = \case
  NodeComment txt       -> [ RenderLine (indentText opts level <> ";" <> txt) KindComment ]
  NodeExpr expr         -> formatExpr opts level expr
  NodeExprRaw _ rawText -> [ RenderLine rawText KindExpr ]  -- Output raw text for skipped nodes
  NodeBlankLine         -> ([ RenderLine "" KindBlankLine | preserveBlankLines opts ])

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
        InlineHead n   -> maybe
          (finalize (baseLine : body) nodes)
          (\( lh, rest, mbAlignCol ) -> case mbAlignCol of
             Just alignCol -> finalize (lh : formatRestAligned alignCol rest) nodes
             Nothing       -> finalize (lh : formatRest rest) nodes)
          (renderInlineHeadStyleWithAlign opts level delim atomName args n)
        BindingsHead n -> maybe
          (finalize (baseLine : body) nodes)
          (\( lh, rest ) -> finalize (lh : formatRest rest) nodes)
          (renderBindingsHeadStyle opts level delim atomName args n)
        Newline        -> maybe
          (finalize (baseLine : body) nodes)
          (\( lh, rest ) -> finalize (lh : formatRest rest) nodes)
          (renderNewlineAlignStyle opts level delim atomName args)
        TryInline      -> maybe
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

        -- Calculate the level that corresponds to alignCol
        alignLevel = alignCol `div` max 1 (indentWidth opts)

        formatAligned (NodeExpr expr)         = case renderCompactExpr opts expr of
          Just compact -> [ RenderLine (alignIndent <> compact) KindExpr ]
          Nothing      ->
            -- For non-compact expressions, format at the alignment level
            -- and adjust the first line's indentation to match alignCol
            let
                formatted = formatNode opts alignLevel (NodeExpr expr)
              in 
                case formatted of
                  [] -> []
                  (firstLine : rest) -> let
                      currentIndent = indentText opts alignLevel
                      newIndent     = alignIndent
                      adjusted
                        = firstLine { rlText = newIndent
                                        <> fromMaybe
                                          (rlText firstLine)
                                          (T.stripPrefix currentIndent (rlText firstLine))
                                    }
                    in 
                      adjusted : rest
        formatAligned (NodeExprRaw _ rawText) = [ RenderLine rawText KindExpr ]  -- Output raw text
        formatAligned (NodeComment txt)
          = [ RenderLine (alignIndent <> ";" <> txt) KindComment ]
        formatAligned NodeBlankLine           = []  -- Blank lines don't make sense in aligned context

    finalize ls ns
      = if isLastComment ns
        then ls ++ [ RenderLine (indentCur <> closeDelim) KindExpr ]
        else attachClosing ls closeDelim

    renderInlineSimple o lvl d ns = do
      inline <- renderCompactListSimple o d ns
      let lineText = indentText o lvl <> inline
      if T.length lineText <= inlineMaxWidth o
        then Just lineText
        else Nothing

    renderInlineHead o lvl _ (NodeExpr first : rest) = do
      inlineFirst <- renderCompactExpr o first
      let firstText = indentText o lvl <> openDelim <> inlineFirst
      if T.length firstText <= inlineMaxWidth o
        then Just ( RenderLine firstText KindExpr, rest )
        else Nothing
    renderInlineHead _ _ _ _ = Nothing

    renderSpecialInlineAlign o lvl d (NodeExpr (Atom atomName) : args) = do
      headRule <- find ((== atomName) . atom) (specials o)
      case style headRule of
        InlineHead n   -> do
          ( lh, rest, mbAlignCol ) <- renderInlineHeadStyleWithAlign o lvl d atomName args n
          alignCol <- mbAlignCol
          Just ( lh, rest, alignCol )
        BindingsHead _ -> Nothing
        Newline        -> Nothing
        TryInline      -> Nothing
    renderSpecialInlineAlign _ _ _ _ = Nothing

    renderSpecialInlineHead o lvl d (NodeExpr (Atom atomName) : args) = do
      headRule <- find ((== atomName) . atom) (specials o)
      case style headRule of
        InlineHead n   -> do
          ( lh, rest, _ ) <- renderInlineHeadStyleWithAlign o lvl d atomName args n
          Just ( lh, rest )
        BindingsHead n -> renderBindingsHeadStyle o lvl d atomName args n
        Newline        -> renderNewlineAlignStyle o lvl d atomName args
        TryInline      -> renderTryInlineStyle o lvl d atomName args
    renderSpecialInlineHead _ _ _ _ = Nothing

    -- Helper to find align style for an atom
    findAlignStyle atomName = case find ((== atomName) . alignAtom) (aligns opts) of
      Just rule -> alignStyle rule
      Nothing   -> defaultAlign opts

    -- InlineHead with alignment support
    -- Returns (line, rest, Maybe alignCol) where alignCol is Just if Align style is used
    renderInlineHeadStyleWithAlign o lvl _ atomName args inlineCount = do
      let ( inlineArgs, restArgs ) = splitAt inlineCount args
      inlineParts <- traverse (renderCompactNode o) inlineArgs
      let atomText
            = indentText o lvl <> openDelim <> atomName <> " " <> T.intercalate " " inlineParts
      if T.length atomText <= inlineMaxWidth o
        then case findAlignStyle atomName of
          Align  -> do
            -- Calculate where the last inlined argument starts
            let atomAndPrefix    = indentText o lvl <> openDelim <> atomName <> " "
                allButLastInline = take (length inlineParts - 1) inlineParts
                beforeLastLength
                  = if null allButLastInline
                    then 0
                    else T.length (T.intercalate " " allButLastInline) + 1  -- +1 for the space
                alignColumn      = T.length atomAndPrefix + beforeLastLength
            Just ( RenderLine atomText KindExpr, restArgs, Just alignColumn )
          Normal -> Just ( RenderLine atomText KindExpr, restArgs, Nothing )
        else Nothing

    renderNewlineAlignStyle o lvl _ atomName args
      = let
          firstLine = RenderLine (indentText o lvl <> openDelim <> atomName) KindExpr
        in 
          Just ( firstLine, args )  -- Always format with newline

    renderBindingsHeadStyle opt lev outerDelim atomName args bindingsCount = do
      let ( bindingArgs, restArgs ) = splitAt bindingsCount args
      case bindingArgs of
        [ NodeExpr (List bindingDelim bindingNodes) ] -> do
          -- Prepare binding block and body lines
          let ( outerOpen, _ )  = delimiterPair outerDelim
              bodyLines         = concatMap (formatNode opt (lev + 1)) restArgs
              bindingPrefix     = indentText opt lev <> outerOpen <> atomName <> " "
              bindingIndentCols
                = T.length bindingPrefix + T.length (fst (delimiterPair bindingDelim))
              bindingLines
                = formatBindingsList opt bindingDelim bindingNodes bindingIndentCols
              lineGroups        = case bindingLines of
                [] -> [ bindingPrefix
                        <> fst (delimiterPair bindingDelim)
                        <> snd (delimiterPair bindingDelim)
                      ]
                (firstBindingLine : restBindingLines) -> (bindingPrefix <> firstBindingLine)
                  : restBindingLines
              allLines          = lineGroups ++ map rlText bodyLines
              fullText          = T.intercalate "\n" allLines
          Just ( RenderLine fullText KindExpr, [] )
        _ -> Nothing

    formatBindingsList :: FormatOptions -> DelimiterType -> [ Node ] -> Int -> [ Text ]
    formatBindingsList opt delimType bindingNodes indentCols
      = let
          ( openDel, closeDel ) = delimiterPair delimType
          pairs = case bindingNodes of
            [] -> []
            _  -> map (formatBindingPair opt) (groupIntoPairs bindingNodes)
          appendClosing :: [ Text ] -> [ Text ]
          appendClosing = \case
            [] -> []
            [ single ] -> [ single <> closeDel ]
            (line : rest) -> line : appendClosing rest
        in 
          case pairs of
            [] -> [ openDel <> closeDel ]
            (firstPair : restPairs) -> let
                firstLine = openDel <> firstPair
                spacing   = T.replicate indentCols " "
                restLines = case restPairs of
                  [] -> []
                  xs -> map (spacing <>) xs
              in 
                appendClosing (firstLine : restLines)

    groupIntoPairs :: [ a ] -> [ [ a ] ]
    groupIntoPairs [] = []
    groupIntoPairs (x : y : rest) = [ x, y ] : groupIntoPairs rest
    groupIntoPairs [ x ] = [ [ x ] ]

    formatBindingPair :: FormatOptions -> [ Node ] -> Text
    formatBindingPair opt nodeList = T.intercalate " " $ map (formatNodeForBinding opt) nodeList

    formatNodeForBinding :: FormatOptions -> Node -> Text
    formatNodeForBinding opt (NodeExpr expr) = fromMaybe "" (renderCompactExpr opt expr)
    formatNodeForBinding _ _ = ""

    renderTryInlineStyle o lvl _ atomName args = do
      inlineParts <- traverse (renderCompactNode o) args
      let atomText
            = indentText o lvl <> openDelim <> atomName <> " " <> T.intercalate " " inlineParts
      if T.length atomText <= inlineMaxWidth o
        then Just ( RenderLine atomText KindExpr, [] )
        else Nothing  -- Fall back to regular formatting

    renderCompactNode o (NodeExpr expr)   = renderCompactExpr o expr
    renderCompactNode _ (NodeExprRaw _ _) = Nothing  -- Cannot compact skipped nodes
    renderCompactNode _ (NodeComment _)   = Nothing
    renderCompactNode _ NodeBlankLine     = Nothing

--------------------------------------------------------------------------------
-- Helper data types and functions

data LineKind = KindExpr | KindComment | KindBlankLine
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

renderCompactListSimple :: FormatOptions -> DelimiterType -> [ Node ] -> Maybe Text
renderCompactListSimple opts delim nodes = do
  parts <- traverse inline nodes
  let ( open, close ) = delimiterPair delim
  pure $ open <> T.intercalate " " parts <> close
  where
    inline (NodeExpr expr) = renderCompactExpr opts expr
    inline _ = Nothing

renderCompactExpr :: FormatOptions -> SExpr -> Maybe Text
renderCompactExpr _ (Atom t) = Just t
renderCompactExpr _ (StringLit t) = Just (encodeString t)
renderCompactExpr opts (QuoteExpr kind expr) = do
  inner <- renderCompactExpr opts expr
  Just (quotePrefix kind <> inner)
renderCompactExpr opts (List delim nodes)
  =
  -- Check if this list has a special formatting rule that prevents inlining
  case nodes of
    (NodeExpr (Atom atomName) : args) ->
      -- First check for common block constructs that should never be inlined
      if atomName `elem` [ "do", "begin", "progn" ] && not (null args)
        then Nothing
        else 
          -- Check if there's a special rule for this atom
          case find ((== atomName) . atom) (specials opts) of
            Just rule -> case style rule of
              -- BindingsHead style should not be inlined if it has content
              -- Empty bindings heads like (let) can still be inlined
              BindingsHead _ -> if null args
                then compactList opts delim nodes
                else Nothing
              -- Other styles can potentially be inlined
              _ -> compactList opts delim nodes
            Nothing   -> compactList opts delim nodes
    _ -> compactList opts delim nodes

compactList :: FormatOptions -> DelimiterType -> [ Node ] -> Maybe Text
compactList opts delim nodes = do
  parts <- traverse renderNode nodes
  let ( open, close ) = delimiterPair delim
  Just $ open <> T.intercalate " " parts <> close
  where
    renderNode (NodeExpr e)      = renderCompactExpr opts e
    renderNode (NodeExprRaw _ _) = Nothing  -- Cannot compact skipped nodes
    renderNode NodeComment {}    = Nothing
    renderNode NodeBlankLine     = Nothing

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