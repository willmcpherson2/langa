module Tree (Tree (..), parseTrees) where

import Data.Char (isDigit, isSpace)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList)
import Parss.Combinators
import Parss.Parser
import Parss.Stream
import Parss.Trans
import Ast

data Tree
  = TreeParens [Tree] Loc
  | TreeBrackets [Tree] Loc
  | TreeBraces [Tree] Loc
  | TreeStr StrLit
  | TreeChar CharLit
  | TreeFloat FloatLit
  | TreeInt IntLit
  | TreeVar Var
  deriving (Show)

type Match = String

type Source = (String, String)

parseTrees :: String -> [Tree]
parseTrees = parse (many parseTree) . ("",)

parseTree :: Parser Match Source (Maybe Tree)
parseTree = do
  parseSpace
  try parseParens
    <|> try parseBrackets
    <|> try parseBraces
    <|> try parseStr
    <|> try parseChar
    <|> try parseFloat
    <|> try parseInt
    <|> try parseVar

parseParens :: Parser Match Source (Maybe Tree)
parseParens = locateM . fallible $ do
  '(' <- need stream
  trees <- ok $ many parseTree
  ')' <- need stream
  pure $ TreeParens trees

parseBrackets :: Parser Match Source (Maybe Tree)
parseBrackets = locateM . fallible $ do
  '[' <- need stream
  trees <- ok $ many parseTree
  ']' <- need stream
  pure $ TreeBrackets trees

parseBraces :: Parser Match Source (Maybe Tree)
parseBraces = locateM . fallible $ do
  '{' <- need stream
  trees <- ok $ many parseTree
  '}' <- need stream
  pure $ TreeBraces trees

parseStr :: Parser Match Source (Maybe Tree)
parseStr = locateM . fallible $ do
  eof <- ok $ many $ try $ neg $ liftResult (try $ is '`') <|> end
  need $ try $ is '`'
  let close = '`' : concat eof
  str <- ok $ many $ try $ neg $ try (string close) <|> end
  need $ try $ string close
  pure $ TreeStr . StrLit (concat str)

parseChar :: Parser Match Source (Maybe Tree)
parseChar = locateM . fallible $ do
  need $ try $ is '\''
  char <- need stream
  pure $ TreeChar . CharLit char

parseFloat :: Parser Match Source (Maybe Tree)
parseFloat = locateM . fallible $ do
  sign <- ok parseSign
  nat <- need parseNat
  point <- need $ try $ string $ '.' :| ""
  frac <- need parseNat
  let num = nat <> point <> frac
  pure $ TreeFloat . FloatLit (maybe num (<> num) sign)

parseInt :: Parser Match Source (Maybe Tree)
parseInt = locateM . fallible $ do
  sign <- ok parseSign
  nat <- need parseNat
  pure $ TreeInt . IntLit (maybe nat (<> nat) sign)

parseSign :: Parser Match Source (Maybe Chars)
parseSign = try (string $ '+' :| "") <|> try (string $ '-' :| "")

parseNat :: Parser Match Source (Maybe Chars)
parseNat = some $ try $ satisfy isDigit

parseVar :: Parser Match Source (Maybe Tree)
parseVar = locateM . fallible $ do
  notSep <- need $ some $ try $ neg $ parseSeparator <|> end
  var <- need $ pure $ nonEmpty $ concat notSep
  pure $ TreeVar . Var var

parseSpace :: Parser Match Source (Maybe Match)
parseSpace = fallible $ do
  space <- need $ some $ try $ satisfy isSpace
  pure $ toList space

parseSeparator :: Parser Match Source (Maybe Match)
parseSeparator =
  parseSpace
    <|> try (string "(")
    <|> try (string ")")
    <|> try (string "[")
    <|> try (string "]")
    <|> try (string "{")
    <|> try (string "}")
