module Main (main, run) where

import Ast
import Parse
import Tree
import Display
import Check

data Pipeline = Pipeline
  { trees :: [Tree],
    ast :: ExpAst,
    checked :: TermAst
  }
  deriving (Show)

run :: String -> Pipeline
run s =
  let trees = parseTrees s
      ast = parseAst trees
      checked = checkAst ast
   in Pipeline {trees, ast, checked}

repl :: IO ()
repl = do
  putStrLn "Code:"
  input <- getLine
  if input == "exit"
    then return ()
    else do
      let pipeline = run input

      putStrLn "Trees:"
      print $ trees pipeline
      putStrLn $ display $ trees pipeline

      putStrLn "Ast:"
      print $ ast pipeline
      putStr $ display $ ast pipeline

      putStrLn "Checked:"
      print $ checked pipeline
      putStrLn $ display $ checked pipeline

      repl

main :: IO ()
main = do
  putStrLn "Langa REPL. Type exit to quit."
  repl
  putStrLn "Goodbye!"
