module Token (Name, Token (..), parseTokens) where

import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.List.NonEmpty
import Parss.Combinators
import Parss.Parser
import Parss.Stream
import Parss.Trans

type Match = String

type Source = (String, String)

type Loc = (String, String, String)

type Name = NonEmpty Char

data Token
  = TokOpenParen Loc
  | TokCloseParen Loc
  | TokOpenBracket Loc
  | TokCloseBracket Loc
  | TokOpenBrace Loc
  | TokCloseBrace Loc
  | TokStr String Loc
  | TokChar Char Loc
  | TokFloat Name Loc
  | TokInt Name Loc
  | TokVar Name Loc
  deriving (Show)

parseTokens :: String -> [Token]
parseTokens = parse (many parseToken) . ("",)

parseToken :: Parser Match Source (Maybe Token)
parseToken = do
  parseSpace
  parseParen
    <|> try parseStr
    <|> try parseChar
    <|> try parseFloat
    <|> try parseInt
    <|> try parseVar

parseSeparator :: Parser Match Source (Maybe ())
parseSeparator = try parseSpace <|> void <$> parseParen

parseSpace :: Parser Match Source (Maybe ())
parseSpace = void <$> try (satisfy isSpace)

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

parseChar :: Parser Match Source (Maybe Token)
parseChar = locateM . fallible $ do
  need $ try $ is '\''
  char <- need stream
  pure $ TokChar char

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

parseSign :: Parser Match Source (Maybe Name)
parseSign = try (string $ '+' :| "") <|> try (string $ '-' :| "")

parseNat :: Parser Match Source (Maybe Name)
parseNat = some $ try $ satisfy isDigit

parseVar :: Parser Match Source (Maybe Token)
parseVar = locateM . fallible $ do
  notSep <- need $ some $ try $ neg $ parseSeparator <|> void <$> end
  var <- need $ pure $ nonEmpty $ concat notSep
  pure $ TokVar var
