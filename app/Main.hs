module Main (main, run) where

import Ast
import Parse
import Tree
import Display

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

repl :: IO ()
repl = do
  putStrLn "Code:"
  input <- getLine
  if input == "exit"
    then return ()
    else do
      putStrLn "Data:"
      putStrLn $ display $ ast $ run input
      repl

main :: IO ()
main = do
  putStrLn "Langa REPL. Type exit to quit."
  repl
  putStrLn "Goodbye!"
