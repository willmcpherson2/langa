module Main (main) where

import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)
import Data.List.NonEmpty
import Parss.Combinators
import Parss.Parser
import Parss.Trans

type Match = String

type Source = (String, String)

type Loc = (String, String, String)

data Err
  = Something
  deriving (Eq, Show)

data Exp
  = ExpApp [Exp] Loc
  | ExpArr [Exp] Loc
  | ExpObj [Exp] Loc
  | ExpStr String Loc
  | ExpVar (NonEmpty Char) Loc
  | ExpNum (NonEmpty Char) Loc
  | ExpInt (NonEmpty Char) Loc
  | ExpErr Err Loc
  deriving (Eq, Show)

main :: IO ()
main = pure ()

parseVar :: Parser Match Source (Maybe Exp)
parseVar = locateM . fallible $ do
  let first = try $ satisfy isAlpha
      rest = many $ try $ satisfy isAlphaNum
  first <- need first
  rest <- ok rest
  pure $ ExpVar $ first :| rest

parseStr :: Parser Match Source (Maybe Exp)
parseStr = locateM . fallible $ do
  eof <- ok $ many $ try $ neg $ liftResult (try $ is '`') <|> end
  need $ try $ is '`'
  let close = '`' : concat eof
  str <- ok $ many $ try $ neg $ try (string close) <|> end
  need $ try $ string close
  pure $ ExpStr $ concat str

parseNum :: Parser Match Source (Maybe Exp)
parseNum = locateM . fallible $ do
  let sign = try $ string $ '-' :| ""
      nan = try $ string $ 'N' :| "aN"
      infinity = try $ string $ 'I' :| "nfinity"
      int = some $ try $ satisfy isDigit
      dot = try $ string $ '.' :| ""
      float = try $ int ><> dot ><> int
  sign <- ok sign
  num <- need $ nan <|> infinity <|> float <|> int
  pure $ ExpNum $ maybe num (<> num) sign

parseInt :: Parser Match Source (Maybe Exp)
parseInt = locateM . fallible $ do
  let sign = try $ string $ '-' :| ""
      int = some $ try $ satisfy isDigit
  sign <- ok sign
  int <- need int
  suffix <- need $ string $ 'n' :| ""
  let num = int <> suffix
  pure $ ExpInt $ maybe num (<> num) sign

skipSpace :: Parser Match Source String
skipSpace = many $ try $ satisfy isSpace
