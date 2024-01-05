module Main (main) where

import Ast
import Token
import Tree

data Pipeline = Pipeline
  { tokens :: [Token],
    trees :: [Tree],
    ast :: ExpAst
  }
  deriving (Show)

run :: String -> Pipeline
run s =
  let tokens = parseTokens s
      trees = parseTrees tokens
      ast = parseAst trees
   in Pipeline {tokens, trees, ast}

main :: IO ()
main = pure ()
