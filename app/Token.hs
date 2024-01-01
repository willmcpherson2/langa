module Token (Match, Source, Loc, Token (..), parseTokens) where

import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.List.NonEmpty
import Parss.Combinators
import Parss.Parser
import Parss.Trans

type Match = String

type Source = (String, String)

type Loc = (String, String, String)

data Token
  = TokOpenParen Loc
  | TokCloseParen Loc
  | TokOpenBracket Loc
  | TokCloseBracket Loc
  | TokOpenBrace Loc
  | TokCloseBrace Loc
  | TokStr String Loc
  | TokFloat (NonEmpty Char) Loc
  | TokInt (NonEmpty Char) Loc
  | TokVar (NonEmpty Char) Loc
  | TokSpace Loc
  deriving (Eq, Show)

parseTokens :: Parser Match Source [Token]
parseTokens = many parseToken

parseToken :: Parser Match Source (Maybe Token)
parseToken =
  parseSeparator
    <|> try parseStr
    <|> try parseFloat
    <|> try parseInt
    <|> try parseVar

parseSeparator :: Parser Match Source (Maybe Token)
parseSeparator = try parseSpace <|> parseParen

parseSpace :: Parser Match Source (Maybe Token)
parseSpace = locateM . fallible $ do
  need $ try $ satisfy isSpace
  pure TokSpace

parseParen :: Parser Match Source (Maybe Token)
parseParen =
  try parseOpenParen
    <|> try parseCloseParen
    <|> try parseOpenBracket
    <|> try parseCloseBracket
    <|> try parseOpenBrace
    <|> try parseCloseBrace

parseOpenParen :: Parser Match Source (Maybe Token)
parseOpenParen = locateM . fallible $ do
  need $ try $ is '('
  pure TokOpenParen

parseCloseParen :: Parser Match Source (Maybe Token)
parseCloseParen = locateM . fallible $ do
  need $ try $ is ')'
  pure TokCloseParen

parseOpenBracket :: Parser Match Source (Maybe Token)
parseOpenBracket = locateM . fallible $ do
  need $ try $ is '['
  pure TokOpenBracket

parseCloseBracket :: Parser Match Source (Maybe Token)
parseCloseBracket = locateM . fallible $ do
  need $ try $ is ']'
  pure TokCloseBracket

parseOpenBrace :: Parser Match Source (Maybe Token)
parseOpenBrace = locateM . fallible $ do
  need $ try $ is '{'
  pure TokOpenBrace

parseCloseBrace :: Parser Match Source (Maybe Token)
parseCloseBrace = locateM . fallible $ do
  need $ try $ is '}'
  pure TokCloseBrace

parseStr :: Parser Match Source (Maybe Token)
parseStr = locateM . fallible $ do
  eof <- ok $ many $ try $ neg $ liftResult (try $ is '`') <|> end
  need $ try $ is '`'
  let close = '`' : concat eof
  str <- ok $ many $ try $ neg $ try (string close) <|> end
  need $ try $ string close
  pure $ TokStr $ concat str

parseFloat :: Parser Match Source (Maybe Token)
parseFloat = locateM . fallible $ do
  sign <- ok parseSign
  nat <- need parseNat
  point <- need $ try $ string $ '.' :| ""
  frac <- need parseNat
  let num = nat <> point <> frac
  pure $ TokFloat $ maybe num (<> num) sign

parseInt :: Parser Match Source (Maybe Token)
parseInt = locateM . fallible $ do
  sign <- ok parseSign
  nat <- need parseNat
  pure $ TokInt $ maybe nat (<> nat) sign

parseSign :: Parser Match Source (Maybe (NonEmpty Char))
parseSign = try (string $ '+' :| "") <|> try (string $ '-' :| "")

parseNat :: Parser Match Source (Maybe (NonEmpty Char))
parseNat = some $ try $ satisfy isDigit

parseVar :: Parser Match Source (Maybe Token)
parseVar = locateM . fallible $ do
  notSep <- need $ some $ try $ neg $ void <$> parseSeparator <|> void <$> end
  var <- need $ pure $ nonEmpty $ concat notSep
  pure $ TokVar var
