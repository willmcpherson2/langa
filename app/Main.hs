module Main (main) where

import Ast
import Parse
import Tree

data Pipeline = Pipeline
  { trees :: [Tree],
    ast :: ExpAst
  }
  deriving (Show)

run :: String -> Pipeline
run s =
  let trees = parseTrees s
      ast = parseAst trees
   in Pipeline {trees, ast}

main :: IO ()
main = pure ()
