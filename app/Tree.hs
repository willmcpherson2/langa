module Tree (Tree (..), parseTrees) where

import Ast
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList)
import Parss.Combinators
import Parss.Parser
import Parss.Stream
import Parss.Trans
import Text.Read (readMaybe)

data Tree
  = TreeParens [Tree] Loc
  | TreeBrackets [Tree] Loc
  | TreeBraces [Tree] Loc
  | TreeChar CharLit
  | TreeFloat FloatLit
  | TreeInt IntLit
  | TreeNat NatLit
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
    <|> try parseNat
    <|> try parseInt
    <|> try parseFloat
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
  eof <- ok $ many $ try $ neg $ liftResult (try $ is '"') <|> end
  need $ try $ is '"'
  let close = '"' : concat eof
  str <- ok $ many $ try $ locateM $ fallible $ do
    [char] <- need $ neg $ try (string close) <|> end
    pure $ TreeChar . CharLit char
  need $ try $ string close
  pure $ \loc -> case str of
    [] -> TreeVar (Var ('n' :| "il") loc)
    str -> TreeBrackets (str <> [TreeVar (Var ('n' :| "il") loc)]) loc

parseChar :: Parser Match Source (Maybe Tree)
parseChar = locateM . fallible $ do
  TreeVar (Var s _) <- need parseVar
  char <- need $ pure $ readMaybe $ toList s
  pure $ TreeChar . CharLit char

parseNat :: Parser Match Source (Maybe Tree)
parseNat = locateM . fallible $ do
  TreeVar (Var s _) <- need parseVar
  nat <- need $ pure $ readMaybe $ toList s
  need $ pure $ guard (nat >= 0)
  pure $ TreeNat . NatLit nat

parseInt :: Parser Match Source (Maybe Tree)
parseInt = locateM . fallible $ do
  TreeVar (Var s _) <- need parseVar
  nat <- need $ pure $ readMaybe $ toList s
  pure $ TreeInt . IntLit nat

parseFloat :: Parser Match Source (Maybe Tree)
parseFloat = locateM . fallible $ do
  TreeVar (Var s _) <- need parseVar
  float <- need $ pure $ readMaybe $ toList s
  pure $ TreeFloat . FloatLit float

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
