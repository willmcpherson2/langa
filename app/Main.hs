module Main (main, run) where

import Ast
import Parse
import Tree
import Display
import Locate

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
