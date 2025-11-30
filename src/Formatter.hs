{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Formatter ( renderProgram ) where

import           Control.Monad            ( foldM )
import           Control.Monad.Reader     ( MonadReader(local), Reader, asks, runReader )

import           Data.Foldable            ( Foldable(foldl') )
import           Data.Functor.Foldable    ( para )
import           Data.Functor.Foldable.TH ( MakeBaseFunctor(makeBaseFunctor) )
import           Data.List                ( find, intersperse )
import           Data.List.NonEmpty       ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty       as NL
import           Data.Maybe               ( catMaybes )
import           Data.Text                ( Text )
import qualified Data.Text                as T

import           Types                    ( AlignRule(..)
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

makeBaseFunctor ''Doc

estimateLength :: Doc -> Int
estimateLength = para alg
  where
    alg :: DocF ( Doc, Int ) -> Int
    alg (EmbraceF _ _ ( _, len )) = len + 2
    alg (IndentF indent ls) = maximum (snd <$> NL.toList ls) + indent
    alg (SeqF docsLen) = either id id $ foldl' phi (Right 0) docsLen
    alg (LinerF txts) = sum (T.length <$> txts) + (NL.length txts - 1)
    alg (TextNodeF txt) = T.length txt
    alg EmptyNodeF = 0
    alg (NewlineNodeF _) = 0
    alg (PrefixF pre ( _, len )) = T.length pre + len

    phi :: Either Int Int -> ( Doc, Int ) -> Either Int Int
    phi acc ( NewlineNode {}, _ ) = Left (either id id acc)
    phi (Left acc) ( _, len )     = Right (max acc len)
    phi (Right acc) ( _, len )    = Right (acc + len)

data FormattedDoc = Success Doc | Overflow Int Doc

getOverflow :: FormattedDoc -> Int
getOverflow (Success _)    = 0
getOverflow (Overflow o _) = o

instance Semigroup FormattedDoc where
  Success d1 <> _ = Success d1
  _ <> Success d2 = Success d2
  Overflow o1 d1 <> Overflow o2 d2
    | o1 <= o2 = Overflow o1 d1
    | otherwise = Overflow o2 d2

fromFormatted :: FormattedDoc -> Doc
fromFormatted (Success doc)    = doc
fromFormatted (Overflow _ doc) = doc

mapFormatted :: (Doc -> Doc) -> FormattedDoc -> FormattedDoc
mapFormatted f (Success doc)    = Success (f doc)
mapFormatted f (Overflow o doc) = Overflow o (f doc)

data FormatState = FormatState { options :: FormatOptions, currentColumn :: Int }

setCurrentColumn :: Int -> Reader FormatState a -> Reader FormatState a
setCurrentColumn col = local (\st -> st { currentColumn = col })

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

    txt
      = (map fromFormatted <$> mapM formatNode activeNodes)
      `runReader` FormatState { options = opts, currentColumn = 0 }

    sep [] _ acc = reverse acc
    sep _ [] acc = reverse acc
    sep [ _ ] [ y ] acc = reverse (y : acc)
    sep (NodeComment {} : xs) (y : ys) acc = sep xs ys (NewlineNode 1 : y : acc)
    sep (NodeBlankLine {} : xs) (y : ys) acc = sep xs ys (y : acc)
    sep (_ : xs@(NodeBlankLine {} : _)) (y : ys) acc = sep xs ys (NewlineNode 1 : y : acc)
    sep (_ : xs) (y : ys) acc = sep xs ys (NewlineNode 2 : y : acc)

formatNode :: Node -> Reader FormatState FormattedDoc
formatNode (NodeExpr sexpr) = formatSExpr sexpr
formatNode (NodeExprRaw _sexpr rawText) = return $ Success (TextNode rawText)
formatNode (NodeComment txt) = return $ Success (Prefix ";" (Liner (txt :| [])))
formatNode (NodeBlankLine n) = return $ Success (NewlineNode n)

formatSExpr :: SExpr -> Reader FormatState FormattedDoc
formatSExpr (Atom txt) = checkOverflow txt
formatSExpr (StringLit txt) = checkOverflow (encodeString txt)
formatSExpr (QuoteExpr qk sexpr) = do
  formatted <- formatSExpr sexpr
  return $ mapFormatted (Prefix (quote qk)) formatted
formatSExpr (List delimTyp nodes) = do
  opt <- asks options
  case nodes of
    [] -> return $ Success (Embrace open close EmptyNode)
    [ x ] -> do
      col <- asks currentColumn
      innerDoc <- setCurrentColumn (col + 1) $ formatNode x
      return $ Embrace open close `mapFormatted` innerDoc
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

formatList :: FormatStyle
           -> AlignStyle
           -> Text
           -> Text
           -> Node
           -> NonEmpty Node
           -> Reader FormatState FormattedDoc
formatList fmtStyle alignPref open close at rest = case fmtStyle of
  Newline -> do
    curCol <- asks currentColumn
    indent <- asks (indentWidth . options)
    atDoc <- setCurrentColumn (curCol + 1) $ formatNode at
    restDocs <- mapM (setCurrentColumn (curCol + indent) . formatNode) rest
    let overflow = docsOverflow (atDoc : NL.toList restDocs)
        doc
          = embraceList
            open
            close
            rest
            (Seq
             $ fromFormatted atDoc :| [ NewlineNode 1, Indent indent (fromFormatted <$> restDocs) ])
    return
      $ if overflow == 0
        then Success doc
        else Overflow overflow doc

  TryInline -> do
    maxWidth <- asks (inlineMaxWidth . options)
    indent <- asks (indentWidth . options)
    col <- asks currentColumn
    fallback <- formatList Newline alignPref open close at rest
    let onelineDoc = Embrace open close $ Liner $ plainNode <$> NL.cons at rest
        oneline    = layoutResult col maxWidth onelineDoc []
        twolineDoc
          = let
              secondLine = Indent indent $ NL.singleton $ Liner $ plainNode <$> rest
            in 
              Embrace open close (Seq (TextNode (plainNode at) :| [ NewlineNode 1, secondLine ]))
        twoline    = layoutResult col maxWidth twolineDoc []
    return $ preferLayouts [ oneline, twoline, fallback ]

  InlineFirst n -> do
    col <- asks currentColumn
    maxWidth <- asks (inlineMaxWidth . options)
    indSize <- asks (indentWidth . options)
    let ( inlineArgs, newlineArgs ) = splitArguments n (NL.toList rest)
        inlined = plainNode <$> at :| inlineArgs
        indentInline = case alignPref of
          Align  -> estimateAlignWidth (NL.toList inlined) + 1
          Normal -> indSize
    newlineDocsInline <- mapM (setCurrentColumn (col + indentInline) . formatNode) newlineArgs
    let onelineDoc
          = Embrace
            open
            close
            (Seq $ Liner inlined :| case newlineDocsInline of
               []        -> []
               (hd : tl) -> case NL.last (at :| inlineArgs) of
                 NodeComment {} -> [ Indent indentInline (docsToNonEmpty hd tl) ]
                 _ -> [ NewlineNode 1, Indent indentInline (docsToNonEmpty hd tl) ])
        oneline     = layoutResult col maxWidth onelineDoc newlineDocsInline

        inlineTexts = plainNode <$> inlineArgs
        indentStair = case alignPref of
          Align  -> estimateAlignWidth inlineTexts + 1 + indSize
          Normal -> indSize
    atDoc <- formatNode at
    newlineDocsStair <- mapM (setCurrentColumn (col + indentStair) . formatNode) newlineArgs
    let indentDouble = indSize * 2
    inlineDocsDouble <- mapM (setCurrentColumn (col + indentDouble) . formatNode) inlineArgs
    let inlineBlock = case inlineTexts of
          []        -> []
          (hd : tl) -> [ NewlineNode 1, Indent (indSize * 2) (NL.singleton (Liner (hd :| tl))) ]
        newlineBlock = case newlineDocsStair of
          []        -> []
          (hd : tl) -> [ NewlineNode 1, Indent indentStair (docsToNonEmpty hd tl) ]
        stairDoc
          = embraceList open close rest (Seq $ fromFormatted atDoc :| inlineBlock <> newlineBlock)
        stair = layoutResult col maxWidth stairDoc (atDoc : newlineDocsStair)
        doubleInlineBlock = case inlineDocsDouble of
          []        -> []
          (hd : tl) -> [ NewlineNode 1, Indent indentDouble (docsToNonEmpty hd tl) ]
        doubleStairDoc
          = embraceList
            open
            close
            rest
            (Seq $ fromFormatted atDoc :| doubleInlineBlock <> newlineBlock)
        doubleStair = case newlineDocsStair of
          [] -> Nothing
          _  -> Just
            (layoutResult
               col
               maxWidth
               doubleStairDoc
               (atDoc : inlineDocsDouble ++ newlineDocsStair))
    fallback <- formatList Newline alignPref open close at rest
    return $ preferLayouts (catMaybes [ Just oneline, Just stair, doubleStair, Just fallback ])

  FlexibleInlineFirst n -> do
    indSize <- asks (indentWidth . options)
    maxWidth <- asks (inlineMaxWidth . options)
    col <- asks currentColumn
    let ( inlineArgs, newlineArgs ) = splitArguments n (NL.toList rest)
    atDoc <- formatNode at
    let atLen      = estimateLength (fromFormatted atDoc)
        initialCol = col + 1 + atLen + 1
    ( inlineDocs, _ ) <- foldM
      (\( docs, currentCol ) arg -> do
         let reducedMaxWidth = maxWidth - currentCol
         formatted
           <- local (\st -> st { currentColumn = currentCol
                               , options       = (options st) { inlineMaxWidth = reducedMaxWidth }
                               }) (formatNode arg)
         let docLen = estimateLength (fromFormatted formatted)
         return ( docs ++ [ formatted ], currentCol + docLen + 1 ))
      ( [], initialCol )
      inlineArgs
    newlineDocs <- mapM formatNode newlineArgs
    let allInlined = atDoc : inlineDocs
        allInlinedDocs = fmap fromFormatted allInlined
        inlinedSeq = case allInlinedDocs of
          []    -> EmptyNode
          [ x ] -> x
          xs    -> Seq (NL.fromList $ intersperse (TextNode " ") xs)
        indentInline = case alignPref of
          Align  -> col
            + 1
            + if length allInlinedDocs >= 2
              then sum (estimateLength <$> take (length allInlinedDocs - 1) allInlinedDocs)
                + (length allInlinedDocs - 1)
              else 0
          Normal -> indSize
        newlineBlock = case newlineDocs of
          []        -> []
          (hd : tl) -> case NL.last (at :| inlineArgs) of
            NodeComment {} -> [ Indent indentInline (docsToNonEmpty hd tl) ]
            _ -> [ NewlineNode 1, Indent indentInline (docsToNonEmpty hd tl) ]
        doc = embraceList open close rest (Seq (inlinedSeq :| newlineBlock))
        deps = allInlined ++ newlineDocs
    let result = layoutResult col maxWidth doc deps
    fallback <- formatList Newline alignPref open close at rest
    return $ preferLayouts [ result, fallback ]

  InlineWidth n -> do
    col <- asks currentColumn
    maxWidth <- asks (inlineMaxWidth . options)
    indSize <- asks (indentWidth . options)
    let ( inlineArgs, newlineArgs ) = splitByWidth n (NL.toList rest)
        inlined = plainNode <$> at :| inlineArgs
        indentInline = case alignPref of
          Align  -> estimateAlignWidth (NL.toList inlined) + 1
          Normal -> indSize
    newlineDocsInline <- mapM (setCurrentColumn (col + indentInline) . formatNode) newlineArgs
    let onelineDoc
          = Embrace
            open
            close
            (Seq $ Liner inlined :| case newlineDocsInline of
               []        -> []
               (hd : tl) -> case NL.last (at :| inlineArgs) of
                 NodeComment {} -> [ Indent indentInline (docsToNonEmpty hd tl) ]
                 _ -> [ NewlineNode 1, Indent indentInline (docsToNonEmpty hd tl) ])
        oneline     = layoutResult col maxWidth onelineDoc newlineDocsInline

        inlineTexts = plainNode <$> inlineArgs
        indentStair = case alignPref of
          Align  -> estimateAlignWidth inlineTexts + 1 + indSize
          Normal -> indSize
    atDoc <- formatNode at
    newlineDocsStair <- mapM (setCurrentColumn (col + indentStair) . formatNode) newlineArgs
    let inlineBlock  = case inlineTexts of
          []        -> []
          (hd : tl) -> [ NewlineNode 1, Indent (indSize * 2) (NL.singleton (Liner (hd :| tl))) ]
        newlineBlock = case newlineDocsStair of
          []        -> []
          (hd : tl) -> [ NewlineNode 1, Indent indentStair (docsToNonEmpty hd tl) ]
        stairDoc
          = embraceList open close rest (Seq $ fromFormatted atDoc :| inlineBlock <> newlineBlock)
        stair        = layoutResult col maxWidth stairDoc (atDoc : newlineDocsStair)
    fallback <- formatList Newline alignPref open close at rest
    return $ preferLayouts [ oneline, stair, fallback ]

  Bindings -> case rest of
    binding :| bodyArgs -> case binding of
      NodeExpr (List delim nodes) -> do
        col <- asks currentColumn
        atDoc <- setCurrentColumn (col + 1) $ formatNode at
        maxWidth <- asks (inlineMaxWidth . options)
        indSize <- asks (indentWidth . options)
        let onelineIndent   = case alignPref of
              Align  -> estimateLength (fromFormatted atDoc) + 2
              Normal -> indSize
            twolineIndent   = case alignPref of
              Align  -> 2 * indSize
              Normal -> indSize
            onelineBindings
              = formatBindings delim (estimateLength (fromFormatted atDoc) + 5 - indSize) nodes
            twolineBindings = formatBindings delim 1 nodes

        bodyOneline <- mapM (setCurrentColumn (col + onelineIndent) . formatNode) bodyArgs
        let docOneline
              = embraceList
                open
                close
                rest
                (Seq
                 $ fromFormatted atDoc :| [ TextNode " ", onelineBindings ] <> case bodyOneline of
                   []        -> []
                   (hd : tl) -> [ NewlineNode 1, Indent onelineIndent (docsToNonEmpty hd tl) ])
            depsOneline = atDoc : bodyOneline
            oneline     = layoutResult col maxWidth docOneline depsOneline

        bodyTwoline <- mapM (setCurrentColumn (col + twolineIndent) . formatNode) bodyArgs
        let docTwoline
              = embraceList
                open
                close
                rest
                (Seq
                 $ fromFormatted atDoc
                 :| [ NewlineNode 1, Indent (2 * indSize) (NL.singleton twolineBindings) ]
                 <> case bodyTwoline of
                   []        -> []
                   (hd : tl) -> [ NewlineNode 1, Indent twolineIndent (docsToNonEmpty hd tl) ])
            depsTwoline = atDoc : bodyTwoline
            twoline     = layoutResult col maxWidth docTwoline depsTwoline

        fallback <- formatList Newline alignPref open close at rest
        return $ preferLayouts [ oneline, twoline, fallback ]
      _ -> formatList Newline alignPref open close at rest

  Strategy steps -> do
    opts <- asks options
    col <- asks currentColumn
    let actualMaxWidth = inlineMaxWidth opts
        candidateStyles = fmap strategyStepToStyle steps
        fallbackOptions = defaultStyle opts
        fallback = case fallbackOptions of
          Strategy fallbackSteps
            | fmap strategyStepToStyle fallbackSteps
              == candidateStyles -> formatList Newline alignPref open close at rest
          other -> formatList other alignPref open close at rest
        tryStyles [] = fallback
        tryStyles (candidate : remaining) = do
          layoutDoc <- withUnlimitedWidth $ formatList candidate alignPref open close at rest
          let doc      = fromFormatted layoutDoc
              overflow
                = max (getOverflow layoutDoc) (max 0 (col + estimateLength doc - actualMaxWidth))
          if overflow == 0
            then return (Success doc)
            else tryStyles remaining
        withUnlimitedWidth
          = local (\st -> st { options = (options st) { inlineMaxWidth = maxBound } })
    if null candidateStyles
      then fallback
      else tryStyles candidateStyles

preferLayouts :: [ FormattedDoc ] -> FormattedDoc
preferLayouts []       = Success EmptyNode
preferLayouts (x : xs) = foldl' (<>) x xs

-- | Combine a rendered doc with the current column and inline limit,
-- | computing any width overflow as well as propagating overflow from
-- | already formatted child docs.
layoutResult :: Int -> Int -> Doc -> [ FormattedDoc ] -> FormattedDoc
layoutResult col maxWidth doc deps
  = let
      widthOverflow = max 0 (col + estimateLength doc - maxWidth)
      overflow      = max widthOverflow (docsOverflow deps)
    in 
      if overflow == 0
        then Success doc
        else Overflow overflow doc

docsOverflow :: [ FormattedDoc ] -> Int
docsOverflow = foldr (max . getOverflow) 0

docsToNonEmpty :: FormattedDoc -> [ FormattedDoc ] -> NonEmpty Doc
docsToNonEmpty hd tl = fromFormatted hd :| fmap fromFormatted tl

formatBindings :: DelimiterType -> Int -> [ Node ] -> Doc
formatBindings delim align nodes = case chunksOf 2 nodes of
  [] -> Embrace open close EmptyNode
  [ x ] -> Embrace open close $ Liner $ NL.fromList (plainNode <$> x)
  (firstPair : restPairs) -> let
      firstLine = Liner (NL.fromList (plainNode <$> firstPair))
      restLines = fmap (Liner . NL.fromList . fmap plainNode) restPairs
    in 
      Embrace
        open
        close
        (Seq (firstLine :| [ NewlineNode 1, Indent align (NL.fromList restLines) ]))
  where
    ( open, close ) = delimiters delim

estimateAlignWidth :: [ Text ] -> Int
estimateAlignWidth []    = 0
estimateAlignWidth [ _ ] = 0
estimateAlignWidth nodes = sum (T.length <$> take (length nodes - 1) nodes) + (length nodes - 1)

splitByWidth :: Int -> [ Node ] -> ( [ Node ], [ Node ] )
splitByWidth maxWidth xs = go xs [] 0
  where
    go [] acc _ = ( reverse acc, [] )
    go (y@NodeComment {} : ys) acc _ = ( reverse (y : acc), ys )
    go (y : ys) acc currentWidth
      = let
          yWidth   = T.length (plainNode y) + 1
          newWidth = currentWidth + yWidth
        in 
          if newWidth <= maxWidth
            then go ys (y : acc) newWidth
            else ( reverse acc, y : ys )

plainNode :: Node -> Text
plainNode (NodeExpr sexpr)         = plainSExpr sexpr
plainNode (NodeExprRaw _sexpr raw) = raw
plainNode (NodeComment text)       = ";" <> text <> "\n"
plainNode (NodeBlankLine n)        = T.replicate n "\n"

plainSExpr :: SExpr -> Text
plainSExpr (Atom txt) = txt
plainSExpr (StringLit txt) = encodeString txt
plainSExpr (QuoteExpr qkind sexpr) = quote qkind <> plainSExpr sexpr
plainSExpr (List delimTyp nodes) = open <> inner <> close
  where
    ( open, close ) = delimiters delimTyp

    inner           = T.unwords (map plainNode nodes)

checkOverflow :: Text -> Reader FormatState FormattedDoc
checkOverflow txt = do
  st <- asks currentColumn
  maxWidth <- asks (inlineMaxWidth . options)
  if
    | st + T.length txt <= maxWidth -> return $ Success (TextNode txt)
    | T.length txt > maxWidth -> return $ Success (TextNode txt)
    | otherwise -> return $ Overflow (st + T.length txt - maxWidth) (TextNode txt)

splitArguments :: Int -> [ Node ] -> ( [ Node ], [ Node ] )
splitArguments n xs = go n xs []
  where
    go 0 ys acc = ( reverse acc, ys )
    go _ [] acc = ( reverse acc, [] )
    -- comments cause line breaks
    go _ (y@NodeComment {} : ys) acc = ( reverse (y : acc), ys )
    go m (y : ys) acc = go (m - 1) ys (y : acc)

-- splitByWidth :: Int -> [ Node ] -> ( [ Node ], [ Node ] )
-- splitByWidth maxWidth xs = go xs [] 0
--   where
--     go [] acc _ = ( reverse acc, [] )
--     go (y@NodeComment {} : ys) acc _ = ( reverse (y : acc), ys )
--     go (y : ys) acc currentWidth
--       = let
--           yWidth   = T.length (plainNode y) + 1 -- +1 for space
--           newWidth = currentWidth + yWidth
--         in
--           if newWidth <= maxWidth
--             then go ys (y : acc) newWidth
--             else ( reverse acc, y : ys )

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

quote :: QuoteKind -> Text
quote qk = case qk of
  Quote           -> "'"
  Quasiquote      -> "`"
  Unquote         -> ","
  UnquoteSplicing -> ",@"

{-# INLINE quote #-}

strategyStepToStyle :: StrategyStyle -> FormatStyle
strategyStepToStyle = \case
  StrategyInlineFirst n -> InlineFirst n
  StrategyFlexibleInlineFirst n -> FlexibleInlineFirst n
  StrategyInlineWidth n -> InlineWidth n
  StrategyBindings -> Bindings
  StrategyNewline -> Newline
  StrategyTryInline -> TryInline

appendNewlineIfComment :: NonEmpty Node -> Doc -> Doc
appendNewlineIfComment nodes doc = case NL.last nodes of
  NodeComment _ -> case doc of
    Seq docs -> Seq (docs <> (NewlineNode 1 :| []))
    _        -> Seq (doc :| [ NewlineNode 1 ])
  _ -> doc

embraceList :: Text -> Text -> NonEmpty Node -> Doc -> Doc
embraceList open close nodes doc = Embrace open close (appendNewlineIfComment nodes doc)
