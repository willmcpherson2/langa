module Tree (Tree (..), parseTrees) where

import Token
import Parss.Parser
import Parss.Combinators
import Parss.Trans
import Parss.Stream

type Match = [Token]

type Source = ([Token], [Token])

type Loc = ([Token], [Token], [Token])

data Tree
  = TreeParens [Tree] Loc
  | TreeBrackets [Tree] Loc
  | TreeBraces [Tree] Loc
  | TreeStr String Loc
  | TreeChar Char Loc
  | TreeFloat Name Loc
  | TreeInt Name Loc
  | TreeVar Name Loc
  deriving (Show)

parseTrees :: [Token] -> [Tree]
parseTrees = parse (many parseTree) . ([],)

parseTree :: Parser Match Source (Maybe Tree)
parseTree =
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
  TokOpenParen _ <- need stream
  trees <- ok $ many parseTree
  TokCloseParen _ <- need stream
  pure $ TreeParens trees

parseBrackets :: Parser Match Source (Maybe Tree)
parseBrackets = locateM . fallible $ do
  TokOpenBracket _ <- need stream
  trees <- ok $ many parseTree
  TokCloseBracket _ <- need stream
  pure $ TreeBrackets trees

parseBraces :: Parser Match Source (Maybe Tree)
parseBraces = locateM . fallible $ do
  TokOpenBrace _ <- need stream
  trees <- ok $ many parseTree
  TokCloseBrace _ <- need stream
  pure $ TreeBraces trees

parseStr :: Parser Match Source (Maybe Tree)
parseStr = locateM . fallible $ do
  TokStr str _ <- need stream
  pure $ TreeStr str

parseChar :: Parser Match Source (Maybe Tree)
parseChar = locateM . fallible $ do
  TokChar char _ <- need stream
  pure $ TreeChar char

parseFloat :: Parser Match Source (Maybe Tree)
parseFloat = locateM . fallible $ do
  TokFloat float _ <- need stream
  pure $ TreeFloat float

parseInt :: Parser Match Source (Maybe Tree)
parseInt = locateM . fallible $ do
  TokInt int _ <- need stream
  pure $ TreeInt int

parseVar :: Parser Match Source (Maybe Tree)
parseVar = locateM . fallible $ do
  TokVar var _ <- need stream
  pure $ TreeVar var
