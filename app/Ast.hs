module Ast
  ( Ast,
    Module,
    Item (..),
    Exp (..),
    Type (..),
    Pat (..),
    Lit (..),
    parseAst,
  )
where

import Token
import Tree

type Loc = Tree

type Ast = Module

type Module = [Item]

data Item
  = Import [(Name, [Name])] Loc
  | Declare [(Name, Exp)] Loc
  | Export [Name] Loc
  | Def Name Exp Loc
  deriving (Show)

data Exp
  = ExpType Type Loc
  | ExpLit Lit Loc
  | ExpVar Name Loc
  | ExpCons Exp Exp Loc
  | ExpFun Pat Exp Loc
  | ExpCase Pat Exp Loc
  | ExpApp Exp Exp Loc
  | ExpDo Pat Exp Exp Loc
  | ExpLet Pat Exp Exp Loc
  | ExpTyped Exp Exp Loc
  deriving (Show)

data Type
  = TypeKind Exp Loc
  | TypeSet Exp Exp Loc
  | TypeFor Name Exp Loc
  | TypeFun Exp Exp Loc
  | TypeDo Exp Loc
  | TypeStr Loc
  | TypeChar Loc
  | TypeFloat Loc
  | TypeInt Loc
  deriving (Show)

data Pat
  = PatLit Lit Loc
  | PatVar Name Loc
  | PatCons Pat Pat Loc
  deriving (Show)

data Lit
  = LitStr String Loc
  | LitChar Char Loc
  | LitFloat Name Loc
  | LitInt Name Loc
  deriving (Show)

parseAst :: [Tree] -> Ast
parseAst = undefined
