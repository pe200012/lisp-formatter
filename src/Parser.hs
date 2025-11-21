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
import           Control.Monad        ( void )

import           Data.Char            ( isSpace )
import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Data.Void            ( Void )

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

import           Types                ( DelimiterType(..), Node(..), QuoteKind(..), SExpr(..) )

--------------------------------------------------------------------------------
-- Parser type

type Parser = Parsec Void Text

--------------------------------------------------------------------------------
-- Main parsing functions

parseProgram :: Text -> Either String [ Types.Node ]
parseProgram input = case runParser parser "lisp" input of
  Left err  -> Left $ show err
  Right ast -> Right ast
  where
    parser = do
      skipNonNewlineSpace
      nodes <- many (Text.Megaparsec.try parseTopLevelNode)
      trailingNewlines <- countNewlines
      skipNonNewlineSpace
      eof
      let allNodes = concat nodes
      pure $ allNodes ++ replicate (trailingNewlines - 1) NodeBlankLine

parseTopLevelNode :: Parser [ Node ]
parseTopLevelNode = do
  skipNonNewlineSpace
  newlines <- countNewlines
  node <- parseComment <|> NodeExpr <$> parseExpr
  if newlines > 1
    then pure (replicate (newlines - 1) NodeBlankLine ++ [ node ])
    else pure [ node ]

countNewlines :: Parser Int
countNewlines = do
  spaces <- many (satisfy isSpace)
  pure $ length $ filter (== '\n') spaces

skipNonNewlineSpace :: Parser ()
skipNonNewlineSpace = void $ many (satisfy (\c -> isSpace c && c /= '\n'))

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

--------------------------------------------------------------------------------
-- Helper functions

isAtomChar :: Char -> Bool
isAtomChar c = not (isSpace c) && c `notElem` ("()[]{}\";" :: String)

skipSpace :: Parser ()
skipSpace = void $ many (satisfy isSpace)

optionalNewline :: Parser ()
optionalNewline = void (char '\n') <|> void (string "\r\n") <|> void (char '\r') <|> pure ()