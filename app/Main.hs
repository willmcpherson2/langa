module Main (main, run) where

import Ast
import Check
import Data.List (intercalate)
import Display
import Options.Applicative
import Parse
import Tree

data Pipeline = Pipeline
  { trees :: [Tree],
    ast :: Ast,
    checked :: Ast
  }
  deriving (Show)

run :: String -> Pipeline
run s =
  let trees = parseTrees s
      ast = parseAst trees
      checked = checkAst ast
   in Pipeline {trees, ast, checked}

data Options = Options
  { optTarget :: String,
    optEval :: Bool,
    optLex :: Bool,
    optParse :: Bool,
    optCheck :: Bool
  }

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    "0.1.0"
    ( long "version"
        <> short 'v'
        <> help "Show version information and exit"
    )

options :: Parser Options
options =
  Options
    <$> argument
      str
      ( metavar "TARGET"
          <> help "The file or string containing a program"
      )
    <*> switch
      ( long "eval"
          <> short 'e'
          <> help "Evaluate the target string instead of reading from a file"
      )
    <*> switch
      ( long "lex"
          <> short 'l'
          <> help "Print the lexed trees"
      )
    <*> switch
      ( long "parse"
          <> short 'p'
          <> help "Print the parsed AST"
      )
    <*> switch
      ( long "check"
          <> short 'c'
          <> help "Print the inferred AST"
      )

opts :: ParserInfo Options
opts =
  info
    (helper <*> versionOption <*> options)
    ( fullDesc
        <> progDesc "Process a file or evaluate a string"
        <> header "langa - typed functional lisp"
    )

data StageFormat = StageFormat
  { title :: String,
    include :: Bool,
    output :: String
  }
  deriving (Show)

main :: IO ()
main = do
  opts <- execParser opts
  program <- if optEval opts then pure (optTarget opts) else readFile (optTarget opts)
  let Pipeline {trees, ast, checked} = run program
  let stages =
        [ StageFormat {title = "lex", include = optLex opts, output = display trees},
          StageFormat {title = "parse", include = optParse opts, output = display ast},
          StageFormat {title = "check", include = optCheck opts, output = display checked}
        ]
  let filtered = filter include stages
  let formatted = map (\StageFormat {title, output} -> title <> ":\n" <> output <> "\n") filtered
  let output = intercalate "\n" formatted
  putStr output
