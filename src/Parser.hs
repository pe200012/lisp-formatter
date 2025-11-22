{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( Parser
  , parseExpr
  , parseList
  , parseDelimitedList
  , parseNodeInList
  , parseComment
  , parseQuoted
  , parseString
  , parseAtom
  , skipSpace
  , optionalNewline
  , parseProgram
  ) where

import           Control.Applicative  ( (<|>) )
import           Control.Monad        ( void, when )
import           Control.Monad.State  ( State, evalState, gets, modify )

import           Data.Char            ( isSpace )
import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Data.Void            ( Void )

import qualified Text.Megaparsec      as MP
import           Text.Megaparsec      ( (<?>)
                                      , ParsecT
                                      , anySingle
                                      , choice
                                      , eof
                                      , getOffset
                                      , many
                                      , manyTill
                                      , runParserT
                                      , satisfy
                                      , takeWhile1P
                                      , takeWhileP
                                      )
import           Text.Megaparsec.Char ( char, string )

import           Types                ( DelimiterType(..), Node(..), QuoteKind(..), SExpr(..) )

--------------------------------------------------------------------------------
-- Parser type

data ParserState = ParserState { skipFormatting :: Bool, fullInput :: Text }

type Parser = ParsecT Void Text (State ParserState)

--------------------------------------------------------------------------------
-- Main parsing functions

parseProgram :: Text -> Either String [ Types.Node ]
parseProgram input = case evalState (runParserT parser "lisp" input) (ParserState False input) of
  Left err  -> Left $ show err
  Right ast -> Right ast
  where
    parser = many parseNode <* eof

parseNode :: Parser Node
parseNode = MP.try parseBlankLines <|> parseComment <|> parseNodeExpr

parseBlankLines :: Parser Node
parseBlankLines = do
  newlines <- countNewlines
  if newlines > 0
    then pure (NodeBlankLine newlines)
    else MP.empty <?> "expected blank lines"

parseNodeExpr :: Parser Node
parseNodeExpr = do
  isSkipped <- gets skipFormatting
  if isSkipped
    then do
      startPos <- getOffset
      expr <- MP.try parseExpr
      endPos <- getOffset
      optionalNewline
      whole <- gets fullInput
      let rawText = T.take (endPos - startPos) $ T.drop startPos whole
      modify (\s -> s { skipFormatting = False })
      pure (NodeExprRaw expr rawText)
    else NodeExpr <$> parseExpr <* optionalNewline

skipNonNewlineSpace :: Parser ()
skipNonNewlineSpace = void $ many (satisfy (\c -> isSpace c && c /= '\n' && c /= '\r'))

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
  nodes <- concat <$> manyTill parseNodeInList (MP.try (skipSpace *> char close))
  pure $ List delim nodes

parseNodeInList :: Parser [ Node ]
parseNodeInList = do
  skipNonNewlineSpace
  newlines <- countNewlines
  node <- parseComment <|> NodeExpr <$> parseExpr
  if newlines > 1
    then pure (NodeBlankLine (newlines - 1) : [ node ])
    else pure [ node ]

countNewlines :: Parser Int
countNewlines = go 0
  where
    go n = do
      skipNonNewlineSpace
      rn <- MP.optional (string "\n" <|> string "\r\n" <|> string "\r")
      case rn of
        Just _  -> go (n + 1)
        Nothing -> pure n

parseComment :: Parser Node
parseComment = do
  _ <- char ';'
  content <- takeWhileP Nothing (/= '\n')
  when
    (T.dropWhile isSpace content == "lisp-format skip")
    (modify (\s -> s { skipFormatting = True }))
  optionalNewline
  pure . NodeComment $ T.stripEnd content

parseQuoted :: Parser SExpr
parseQuoted
  = choice
    [ QuoteExpr UnquoteSplicing <$> MP.try (string ",@" *> parseExpr)
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

--------------------------------------------------------------------------------
-- Helper functions

isAtomChar :: Char -> Bool
isAtomChar c = not (isSpace c) && c `notElem` ("()[]{}\";" :: String)

skipSpace :: Parser ()
skipSpace = void $ many (satisfy isSpace)

optionalNewline :: Parser ()
optionalNewline = void (char '\n') <|> void (string "\r\n") <|> void (char '\r') <|> pure ()