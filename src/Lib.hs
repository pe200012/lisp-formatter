{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( FormatError(..)
  , FormatOptions(..)
  , defaultOptions
  , setIndentWidth
  , setInlineMaxWidth
  , readFormatOptionsFromFile
  , formatLispText
  , formatLisp
  ) where

import           Control.Applicative  ( (<|>) )
import           Control.Exception    ( SomeException )
import qualified Control.Exception    as E
import           Control.Monad        ( void )

import           Data.Char            ( isSpace )
import           Data.Maybe           ( fromMaybe )
import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Data.Void            ( Void )

import           Dhall                ( FromDhall, auto, input )

import           GHC.Generics         ( Generic )

import           Text.Megaparsec      ( Parsec
                                      , anySingle
                                      , choice
                                      , eof
                                      , many
                                      , manyTill
                                      , runParser
                                      , satisfy
                                      , takeWhile1P
                                      , takeWhileP
                                      , try
                                      )
import           Text.Megaparsec.Char ( char, string )

-- | Configuration for the formatter.
data FormatOptions = FormatOptions { indentWidth :: !Int, inlineMaxWidth :: !Int }
  deriving ( Eq, Show, Generic )

instance FromDhall FormatOptions

-- | Default formatter options.
defaultOptions :: FormatOptions
defaultOptions = FormatOptions { indentWidth = 2, inlineMaxWidth = 80 }

-- | Update the indentation width, clamping to non-negative values.
setIndentWidth :: Int -> FormatOptions -> FormatOptions
setIndentWidth n opts = opts { indentWidth = max 0 n }

-- | Update the inline max width, ensuring it stays positive.
setInlineMaxWidth :: Int -> FormatOptions -> FormatOptions
setInlineMaxWidth n opts = opts { inlineMaxWidth = max 1 n }

-- | Read FormatOptions from a Dhall file. Returns default options if file doesn't exist or fails to parse.
readFormatOptionsFromFile :: FilePath -> IO FormatOptions
readFormatOptionsFromFile path = do
  result <- E.try (input auto ("./" <> T.pack path)) :: IO (Either SomeException FormatOptions)
  case result of
    Left _     -> pure defaultOptions
    Right opts -> pure opts

-- | Errors that may occur during formatting.
newtype FormatError = ParseError String
  deriving ( Eq, Show )

-- | Public API for formatting strict 'Text'.
formatLispText :: FormatOptions -> Text -> Either FormatError Text
formatLispText opts txt = case runParser parser "lisp" txt of
  Left err  -> Left . ParseError $ show err
  Right ast -> Right $ renderProgram opts ast
  where
    parser = do
      skipSpace
      nodes <- many (Text.Megaparsec.try (parseComment <|> NodeExpr <$> parseExpr))
      skipSpace
      eof
      pure nodes

-- | Convenience wrapper around 'formatLispText' for 'String'.
formatLisp :: FormatOptions -> String -> Either FormatError String
formatLisp opts = fmap T.unpack . formatLispText opts . T.pack

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

--------------------------------------------------------------------------------
-- Parsing

type Parser = Parsec Void Text

parseExpr :: Parser SExpr
parseExpr = do
  skipSpace
  choice [ parseQuoted, parseList, parseString, parseAtom ]

parseList :: Parser SExpr
parseList
  = choice
    [ parseDelimitedList '(' ')' Paren
    , parseDelimitedList '[' ']' Bracket
    , parseDelimitedList '{' '}' Brace
    ]

parseDelimitedList :: Char -> Char -> DelimiterType -> Parser SExpr
parseDelimitedList open close delim = do
  _ <- char open
  nodes <- manyTill parseNodeInList (Text.Megaparsec.try (skipSpace *> char close))
  pure $ List delim nodes

parseNodeInList :: Parser Node
parseNodeInList = do
  skipSpace
  parseComment <|> NodeExpr <$> parseExpr

parseComment :: Parser Node
parseComment = do
  _ <- char ';'
  content <- takeWhileP Nothing (/= '\n')
  _ <- optionalNewline
  pure . NodeComment $ T.stripEnd content

parseQuoted :: Parser SExpr
parseQuoted
  = choice
    [ QuoteExpr UnquoteSplicing <$> Text.Megaparsec.try (string ",@" *> parseExpr)
    , QuoteExpr Unquote <$> (char ',' *> parseExpr)
    , QuoteExpr Quasiquote <$> (char '`' *> parseExpr)
    , QuoteExpr Quote <$> (char '\'' *> parseExpr)
    ]

parseString :: Parser SExpr
parseString = do
  _ <- char '"'
  chars <- manyTill stringChar (char '"')
  pure . StringLit $ T.pack chars

stringChar :: Parser Char
stringChar = escaped <|> satisfy notTerminator
  where
    notTerminator c = c /= '"' && c /= '\\'

    escaped         = do
      _ <- char '\\'
      c <- anySingle
      pure $ case c of
        '"'   -> '"'
        '\\'  -> '\\'
        'n'   -> '\n'
        'r'   -> '\r'
        't'   -> '\t'
        other -> other

parseAtom :: Parser SExpr
parseAtom = Atom <$> takeWhile1P (Just "atom") isAtomChar

isAtomChar :: Char -> Bool
isAtomChar c = not (isSpace c) && c `notElem` ("()[]{}\";" :: String)

skipSpace :: Parser ()
skipSpace = void $ many (satisfy isSpace)

optionalNewline :: Parser ()
optionalNewline = void (char '\n') <|> void (string "\r\n") <|> void (char '\r') <|> pure ()

--------------------------------------------------------------------------------
-- Rendering

data LineKind = KindExpr | KindComment
  deriving ( Eq, Show )

data RenderLine = RenderLine { rlText :: !Text, rlKind :: !LineKind }

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
