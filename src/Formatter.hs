{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Formatter ( renderProgram ) where

import           Control.Monad                  ( foldM )
import           Control.Monad.Reader ( MonadReader(local), Reader, asks, runReader )

import           Data.List            ( find, intersperse )
import           Data.List.NonEmpty   ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty   as NL
import           Data.Text            ( Text )
import qualified Data.Text            as T

import           Types                ( AlignRule(..)
                                      , AlignStyle(..)
                                      , DelimiterType(..)
                                      , FormatOptions(..)
                                      , FormatStyle(..)
                                      , Node(..)
                                      , QuoteKind(..)
                                      , SExpr(..)
                                      , Special(..)
                                      , StrategyStyle(..)
                                      )

data Doc
  = Embrace Text Text Doc
  | Indent Int (NonEmpty Doc)
  | Seq (NonEmpty Doc)
  | Liner (NonEmpty Text)
  | TextNode Text
  | EmptyNode
  | NewlineNode Int
  | Prefix Text Doc
  deriving ( Show )

estimateLength :: Doc -> Int
estimateLength (Embrace _ _ content) = estimateLength content + 2
estimateLength (Indent indent (x :| _)) = estimateLength x + indent
estimateLength (Seq docs) = sum (estimateLength <$> NL.toList docs)
estimateLength (Liner txts) = sum (T.length <$> txts) + (length txts - 1)
estimateLength (TextNode txt) = T.length txt
estimateLength EmptyNode = 0
estimateLength NewlineNode {} = 0
estimateLength (Prefix pre doc) = T.length pre + estimateLength doc

data FormatState = FormatState { options :: FormatOptions, currentColumn :: Int }

renderDoc :: Int -> Doc -> Text
renderDoc indent (Embrace open close content) = open <> renderDoc indent content <> close
renderDoc indent (Indent ind docs)
  = T.intercalate "\n" $ NL.toList $ fmap (\d -> T.replicate (indent + ind) " "
                                           <> renderDoc (indent + ind) d) docs
renderDoc indent (Seq docs) = T.concat $ NL.toList $ fmap (renderDoc indent) docs
renderDoc _ (Liner txt) = T.unwords $ NL.toList txt
renderDoc _ (TextNode txt) = txt
renderDoc _ EmptyNode = ""
renderDoc _ (NewlineNode n) = T.replicate n "\n"
renderDoc indent (Prefix pre doc) = pre <> renderDoc indent doc

renderProgram :: FormatOptions -> [ Node ] -> Text
renderProgram opts nodes = T.concat $ map (renderDoc 0) (sep activeNodes txt [])
  where
    activeNodes
      | preserveBlankLines opts = nodes
      | otherwise = filter (\case
                              NodeBlankLine {} -> False
                              _ -> True) nodes

    txt = mapM formatNode activeNodes `runReader` FormatState { options = opts, currentColumn = 0 }

    sep [] _ acc = reverse acc
    sep _ [] acc = reverse acc
    sep [ _ ] [ y ] acc = reverse (y : acc)
    sep (NodeComment {} : xs) (y : ys) acc = sep xs ys (NewlineNode 1 : y : acc)
    sep (NodeBlankLine {} : xs) (y : ys) acc = sep xs ys (y : acc)
    sep (_ : xs@(NodeBlankLine {} : _)) (y : ys) acc = sep xs ys (NewlineNode 1 : y : acc)
    sep (_ : xs) (y : ys) acc = sep xs ys (NewlineNode 2 : y : acc)

formatNode :: Node -> Reader FormatState Doc
formatNode (NodeExpr sexpr)         = formatSExpr sexpr
formatNode (NodeExprRaw _sexpr raw) = return (TextNode raw)
formatNode (NodeComment text)       = return $ TextNode (";" <> text)
formatNode (NodeBlankLine n)        = return (NewlineNode n)

formatSExpr :: SExpr -> Reader FormatState Doc
formatSExpr (Atom txt) = return (TextNode txt)
formatSExpr (StringLit txt) = return (TextNode (encodeString txt))
formatSExpr (QuoteExpr qkind sexpr) = do
  innerDoc <- formatSExpr sexpr
  return (Prefix quoteSymbol innerDoc)
  where
    quoteSymbol = case qkind of
      Quote           -> "'"
      Quasiquote      -> "`"
      Unquote         -> ","
      UnquoteSplicing -> ",@"
formatSExpr (List delimTyp []) = return $ Embrace open close EmptyNode
  where
    ( open, close ) = delimiters delimTyp
formatSExpr (List delimTyp nodes) = do
  opt <- asks options
  case nodes of
    [ x ] -> do
      innerDoc <- formatNode x
      return $ Embrace open close innerDoc
    (at : rh : rest) -> let
        atStyle
          = maybe (defaultStyle opt) style (find (\s -> atom s == atText) (specials opt))
        atAlignStyle
          = maybe (defaultAlign opt) alignStyle (find (\ar -> alignAtom ar == atText) (aligns opt))
        atText       = case at of
          NodeExpr (Atom t) -> t
          NodeExprRaw (Atom t) _ -> t
          _ -> ""
      in
        formatList atStyle atAlignStyle open close at (rh :| rest)
  where
    ( open, close ) = delimiters delimTyp

formatList
  :: FormatStyle -> AlignStyle -> Text -> Text -> Node -> NonEmpty Node -> Reader FormatState Doc
formatList atStyle atAlignStyle open close at rest = case atStyle of
  InlineFirst n -> do
    maxWidth <- asks (inlineMaxWidth . options)
    indSize <- asks (indentWidth . options)
    let ( inlineArgs, newlineArgs ) = splitArguments n (NL.toList rest)
    oneline <- do
      newlineDocs <- mapM formatNode newlineArgs
      let inlined = plainNode <$> at :| inlineArgs
          indent  = case atAlignStyle of
            Align  -> estimateAlignWidth (NL.toList inlined) + 1
            Normal -> indSize
      return $ Embrace open close $ Seq $ Liner inlined :| case newlineDocs of
        []        -> []
        (hd : tl) -> case NL.last (at :| inlineArgs) of
          NodeComment {} -> [ Indent indent (hd :| tl) ]
          _ -> [ NewlineNode 1, Indent indent (hd :| tl) ]
    stair <- do
      atDoc <- formatNode at
      let inlineDocs = plainNode <$> inlineArgs
          indent     = case atAlignStyle of
            Align  -> estimateAlignWidth inlineDocs + 1 + indSize
            Normal -> indSize
      newlineDocs <- mapM formatNode newlineArgs
      return
        $ Embrace open close
        $ Seq
        $ atDoc
        :| (case inlineDocs of
              []        -> []
              (hd : tl)
                -> [ NewlineNode 1, Indent (indSize * 2) $ NL.singleton (Liner (hd :| tl)) ])
        <> (case newlineDocs of
              []        -> []
              (hd : tl) -> [ NewlineNode 1, Indent indent (hd :| tl) ])
    if
      | estimateLength oneline <= maxWidth -> return oneline
      | estimateLength stair <= maxWidth -> return stair
      | otherwise -> formatList Newline atAlignStyle open close at rest
  FlexibleInlineFirst n -> do
    indSize <- asks (indentWidth . options)
    maxWidth <- asks (inlineMaxWidth . options)
    col <- asks currentColumn
    let (inlineArgs, newlineArgs) = splitArguments n (NL.toList rest)
    atDoc <- formatNode at
    let atLen = estimateLength atDoc
        initialCol = col + 1 + atLen + 1  -- After "( at "
    -- Format inline args sequentially, tracking column
    (inlineDocs, _finalCol) <- foldM
      (\(docs, currentCol) arg -> do
        let reducedMaxWidth = maxWidth - currentCol
        doc <- local (\st -> st { currentColumn = currentCol, options = (options st) { inlineMaxWidth = reducedMaxWidth } }) $ formatNode arg
        let docLen = estimateLength doc
            nextCol = currentCol + docLen + 1  -- +1 for space after this arg
        return (docs ++ [doc], nextCol))
      ([], initialCol)
      inlineArgs
    newlineDocs <- mapM formatNode newlineArgs
    let allInlined = atDoc : inlineDocs
        inlinedSeq = case allInlined of
          []    -> EmptyNode
          [x]   -> x
          xs    -> Seq (NL.fromList $ intersperse (TextNode " ") xs)
        indent = case atAlignStyle of
          -- For Align: col + opening paren + width of all but last inline + spaces between
          Align  -> col + 1 + (if length allInlined >= 2
                               then sum (estimateLength <$> take (length allInlined - 1) allInlined) + (length allInlined - 1)
                               else 0)
          Normal -> indSize
        resultDocs = case newlineDocs of
          []        -> inlinedSeq :| []
          (hd : tl) -> case NL.last (at :| inlineArgs) of
            NodeComment {} -> inlinedSeq :| [Indent indent (hd :| tl)]
            _              -> inlinedSeq :| [NewlineNode 1, Indent indent (hd :| tl)]
    return $ Embrace open close $ Seq resultDocs
  InlineWidth n -> do
    maxWidth <- asks (inlineMaxWidth . options)
    indSize <- asks (indentWidth . options)
    let ( inlineArgs, newlineArgs ) = splitByWidth n (NL.toList rest)
    oneline <- do
      newlineDocs <- mapM formatNode newlineArgs
      let inlined = plainNode <$> at :| inlineArgs
          indent  = case atAlignStyle of
            Align  -> estimateAlignWidth (NL.toList inlined) + 1
            Normal -> indSize
      return $ Embrace open close $ Seq $ Liner inlined :| case newlineDocs of
        []        -> []
        (hd : tl) -> case NL.last (at :| inlineArgs) of
          NodeComment {} -> [ Indent indent (hd :| tl) ]
          _ -> [ NewlineNode 1, Indent indent (hd :| tl) ]
    stair <- do
      atDoc <- formatNode at
      let inlineDocs = plainNode <$> inlineArgs
          indent     = case atAlignStyle of
            Align  -> estimateAlignWidth inlineDocs + 1 + indSize
            Normal -> indSize
      newlineDocs <- mapM formatNode newlineArgs
      return
        $ Embrace open close
        $ Seq
        $ atDoc
        :| (case inlineDocs of
              []        -> []
              (hd : tl)
                -> [ NewlineNode 1, Indent (indSize * 2) $ NL.singleton (Liner (hd :| tl)) ])
        <> (case newlineDocs of
              []        -> []
              (hd : tl) -> [ NewlineNode 1, Indent indent (hd :| tl) ])
    if
      | estimateLength oneline <= maxWidth -> return oneline
      | estimateLength stair <= maxWidth -> return stair
      | otherwise -> formatList Newline atAlignStyle open close at rest
  Bindings -> let
      binding :| bodyArgs = rest
    in
      case binding of
        NodeExpr (List delim nodes) -> do
          atNode <- formatNode at
          maxWidth <- asks (inlineMaxWidth . options)
          indSize <- asks (indentWidth . options)
          body <- mapM formatNode bodyArgs
          let firstLineBinding = take 2 nodes
              oneline          = formatBindings delim (estimateLength atNode + 5 - indSize) nodes
              onelineLen
                = estimateLength atNode + sum (T.length . plainNode <$> firstLineBinding) + 4
              twoline          = formatBindings delim 1 nodes
              onelineIndent    = case atAlignStyle of
                Align  -> estimateLength atNode + 2
                Normal -> indSize
              twolineIndent    = case atAlignStyle of
                Align  -> 2 * indSize
                Normal -> indSize
          if onelineLen <= maxWidth
            then return
              $ Embrace open close
              $ Seq
              $ atNode :| [ TextNode " ", oneline ] <> case body of
                []        -> []
                (hd : tl) -> [ NewlineNode 1, Indent onelineIndent (hd :| tl) ]
            else return
              $ Embrace open close
              $ Seq
              $ atNode :| [ NewlineNode 1, Indent (2 * indSize) (NL.singleton twoline) ]
              <> case body of
                []        -> []
                (hd : tl) -> [ NewlineNode 1, Indent twolineIndent (hd :| tl) ]
        _ -> formatList Newline atAlignStyle open close at rest
  Newline -> do
    atDoc <- formatNode at
    restDocs <- mapM formatNode rest
    indent <- asks (indentWidth . options)
    return $ Embrace open close (Seq (atDoc :| [ NewlineNode 1, Indent indent restDocs ]))
  TryInline -> do
    maxWidth <- asks (inlineMaxWidth . options)
    indent <- asks (indentWidth . options)
    let oneline  = Embrace open close $ Liner $ NL.cons (plainNode at) (plainNode <$> rest)
        twolines
          = Embrace open close
          $ Seq
          $ TextNode (plainNode at)
          :| [ Indent indent (NL.singleton $ Liner (plainNode <$> rest)) ]
    if
      | estimateLength oneline <= maxWidth -> return oneline
      | estimateLength twolines <= maxWidth -> return twolines
      | otherwise -> formatList Newline atAlignStyle open close at rest
  Strategy steps -> do
    opts <- asks options
    let actualMaxWidth = inlineMaxWidth opts
        candidateStyles = fmap strategyStepToStyle steps
        tryStyles [] = fallback
        tryStyles (styleCandidate : remaining) = do
          doc <- withUnlimitedWidth $ formatList styleCandidate atAlignStyle open close at rest
          if estimateLength doc <= actualMaxWidth
            then pure doc
            else tryStyles remaining
        fallbackOptions = defaultStyle opts
        fallback = case fallbackOptions of
          Strategy fallbackSteps
            | fmap strategyStepToStyle fallbackSteps
              == candidateStyles -> formatList Newline atAlignStyle open close at rest
            | otherwise -> formatList (Strategy fallbackSteps) atAlignStyle open close at rest
          other -> formatList other atAlignStyle open close at rest
        withUnlimitedWidth = local (\st -> let
                                        opts' = (options st) { inlineMaxWidth = maxBound }
                                      in
                                        st { options = opts' })
    if null candidateStyles
      then fallback
      else tryStyles candidateStyles

formatBindings :: DelimiterType -> Int -> [ Node ] -> Doc
formatBindings delim align nodes = case chunksOf 2 nodes of
  [] -> Embrace open close EmptyNode
  [ x ] -> Embrace open close $ Liner $ NL.fromList (plainNode <$> x)
  (firstPair : restPairs) -> let
      firstLine = Liner (NL.fromList (plainNode <$> firstPair))
      restLines = (\pair -> Liner (NL.fromList (plainNode <$> pair))) <$> restPairs
    in
      Embrace open close
      $ Seq
      $ firstLine :| [ NewlineNode 1, Indent align (NL.fromList restLines) ]
  where
    ( open, close ) = delimiters delim

estimateAlignWidth :: [ Text ] -> Int
estimateAlignWidth [] = 0
estimateAlignWidth [ _ ] = 0
estimateAlignWidth nodes@(_ : _ : _)
  = sum (T.length <$> take (length nodes - 1) nodes) + (length nodes - 1)

plainNode :: Node -> Text
plainNode (NodeExpr sexpr)         = plainSExpr sexpr
plainNode (NodeExprRaw _sexpr raw) = raw
plainNode (NodeComment text)       = ";" <> text <> "\n"
plainNode (NodeBlankLine n)        = T.replicate n "\n"

plainSExpr :: SExpr -> Text
plainSExpr (Atom txt) = txt
plainSExpr (StringLit txt) = encodeString txt
plainSExpr (QuoteExpr qkind sexpr) = quoteSymbol <> plainSExpr sexpr
  where
    quoteSymbol = case qkind of
      Quote           -> "'"
      Quasiquote      -> "`"
      Unquote         -> ","
      UnquoteSplicing -> ",@"
plainSExpr (List delimTyp nodes) = open <> inner <> close
  where
    ( open, close ) = delimiters delimTyp

    inner           = T.unwords (map plainNode nodes)

splitArguments :: Int -> [ Node ] -> ( [ Node ], [ Node ] )
splitArguments n xs = go n xs []
  where
    go 0 ys acc = ( reverse acc, ys )
    go _ [] acc = ( reverse acc, [] )
    -- comments cause line breaks
    go _ (y@NodeComment {} : ys) acc = ( reverse (y : acc), ys )
    go m (y : ys) acc = go (m - 1) ys (y : acc)

splitByWidth :: Int -> [ Node ] -> ( [ Node ], [ Node ] )
splitByWidth maxWidth xs = go xs [] 0
  where
    go [] acc _ = ( reverse acc, [] )
    go (y@NodeComment {} : ys) acc _ = ( reverse (y : acc), ys )
    go (y : ys) acc currentWidth
      = let
          yWidth   = T.length (plainNode y) + 1  -- +1 for space
          newWidth = currentWidth + yWidth
        in
          if newWidth <= maxWidth
            then go ys (y : acc) newWidth
            else ( reverse acc, y : ys )

delimiters :: DelimiterType -> ( Text, Text )
delimiters delimTyp = case delimTyp of
  Paren   -> ( "(", ")" )
  Bracket -> ( "[", "]" )
  Brace   -> ( "{", "}" )

chunksOf :: Int -> [ e ] -> [ [ e ] ]
chunksOf i ls = map (take i) (splitter ls (:) [])
  where
    splitter :: [ e ] -> ([ e ] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n  = l `c` splitter (drop i l) c n

encodeString :: Text -> Text
encodeString txt = "\"" <> T.concatMap escapeChar txt <> "\""
  where
    escapeChar :: Char -> Text
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = T.singleton c

strategyStepToStyle :: StrategyStyle -> FormatStyle
strategyStepToStyle = \case
  StrategyInlineFirst n -> InlineFirst n
  StrategyFlexibleInlineFirst n -> FlexibleInlineFirst n
  StrategyInlineWidth n -> InlineWidth n
  StrategyBindings -> Bindings
  StrategyNewline -> Newline
  StrategyTryInline -> TryInline