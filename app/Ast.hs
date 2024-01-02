module Ast
  ( Ast,
    Item (..),
    Exp (..),
    Type (..),
    Pat (..),
    parseAst,
  )
where

import Parss.Combinators
import Parss.Parser
import Token
import Tree

type Match = [Tree]

type Source = ([Tree], [Tree])

type Loc = Tree

type Ast = [Item] -- item*

data Item
  = Import Name [Name] Loc -- (<- module name*)
  | Export [Name] Loc -- (-> name*)
  | Declare Name Exp Loc -- {name type}
  | Def Name Exp Loc -- (= name exp)
  deriving (Show)

data Exp
  = ExpData (Data Exp) Loc -- data
  | ExpType Type Loc -- type
  | ExpTyped Exp Exp Loc -- {type exp}
  | ExpFun Pat Exp Loc -- (-> pat+ exp)
  | ExpCase Exp Exp Loc -- (? exp exp+)
  | ExpApp Exp Exp Loc -- (exp exp+)
  | ExpDo Pat Exp Exp Loc -- (do (pat exp)+ exp)
  | ExpLet Pat Exp Exp Loc -- (= (pat exp)+ exp)
  deriving (Show)

data Type
  = TypeStr Loc -- String
  | TypeChar Loc -- Char
  | TypeFloat Loc -- Float
  | TypeInt Loc -- Int
  | TypeFun Exp Exp Loc -- (=> type type+)
  | TypeDo Exp Loc -- (Do type)
  | TypeSet Exp Exp Loc -- (Set type type+)
  | TypeFor Name Exp Loc -- (For name+ type)
  | TypeKind Exp Loc -- (Type n)
  deriving (Show)

newtype Pat = Pat (Data Pat) -- pat
  deriving (Show)

data Data a
  = DataCons a a Loc -- [data data+]
  | DataStr String Loc -- "foo"
  | DataChar Char Loc -- 'a
  | DataFloat Name Loc -- 3.14
  | DataInt Name Loc -- 42
  | DataVar Name Loc -- x
  deriving (Show)

parseAst :: [Tree] -> Ast
parseAst = parse (many parseItem) . ([],)

parseItem :: Parser Match Source (Maybe Item)
parseItem = _
