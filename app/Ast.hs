module Ast
  ( Ast,
    Item (..),
    Exp (..),
    Type (..),
    Pat (..),
    parseAst,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Token
import Tree

type Loc = ([Token], [Token], [Token])

type Ast = [Item] -- item*

data Item
  = Import Var [Var] Loc -- (<- module name*)
  | Export [Var] Loc -- (-> name*)
  | Declare Var Exp Loc -- {name type}
  | Def Var Exp Loc -- (= name exp)
  | ItemErr Err
  deriving (Show)

data Exp
  = ExpType Type Loc -- type
  | ExpData (Data Exp) Loc -- data
  | ExpTyped Exp Exp Loc -- {type exp}
  | ExpFun Pat Exp Loc -- (-> pat+ exp)
  | ExpCase Exp Exp Loc -- (? exp exp+)
  | ExpDo Pat Exp Exp Loc -- (do (pat exp)+ exp)
  | ExpLet Pat Exp Exp Loc -- (= (pat exp)+ exp)
  | ExpApp Exp Exp Loc -- (exp exp+)
  | ExpErr Err
  deriving (Show)

data Type
  = TypeStr Loc -- String
  | TypeChar Loc -- Char
  | TypeFloat Loc -- Float
  | TypeInt Loc -- Int
  | TypeFun Exp Exp Loc -- (=> type type+)
  | TypeDo Exp Loc -- (Do type)
  | TypeSet Exp Exp Loc -- (Set type type+)
  | TypeFor Var Exp Loc -- (For name+ type)
  | TypeKind Exp Loc -- (Type n)
  | TypeErr Err
  deriving (Show)

data Pat
  = Pat (Data Pat) Loc -- pat
  | PatErr Err
  deriving (Show)

data Data a
  = DataCons a a Loc -- [data data+]
  | DataStr String Loc -- "foo"
  | DataChar Char Loc -- 'a
  | DataFloat Name Loc -- 3.14
  | DataInt Name Loc -- 42
  | DataVar Name Loc -- x
  | DataErr Err
  deriving (Show)

data Var
  = Var Name Loc
  | VarErr Err
  deriving (Show)

data Err
  = ErrNoItem Loc
  | ErrNoExp Loc
  | ErrNoPat Loc
  | ErrNoVar Loc
  | ErrEmptyImport Loc
  | ErrEmptyExport Loc
  | ErrEmptyDeclare Loc
  | ErrEmptyDef Loc
  | ErrEmptyTyped Loc
  | ErrEmptyFun Loc
  | ErrEmptyCase Loc
  | ErrEmptyCons Loc
  | ErrEmptyApp Loc
  | ErrEmptyDo Loc
  | ErrEmptyLet Loc
  | ErrEmptyFor Loc
  | ErrEmptyKind Loc
  | ErrEmptySet Loc
  | ErrOneDeclare Loc
  | ErrOneDef Loc
  | ErrOneTyped Loc
  | ErrOneFun Loc
  | ErrOneCase Loc
  | ErrOneCons Loc
  | ErrOneApp Loc
  | ErrOneFor Loc
  | ErrOneDo Loc
  | ErrOneLet Loc
  | ErrOneSet Loc
  | ErrTwoDo Loc
  | ErrTwoLet Loc
  | ErrExcessDeclare Loc
  | ErrExcessDef Loc
  | ErrExcessTyped Loc
  | ErrExcessDo Loc
  | ErrExcessKind Loc
  deriving (Show)

parseAst :: [Tree] -> Ast
parseAst = map parseItem

parseItem :: Tree -> Item
parseItem = \case
  TreeParens (TreeVar ('<' :| "-") _ : trees) loc ->
    case trees of
      [] -> ItemErr $ ErrEmptyImport loc
      name : names -> Import (parseVar name) (map parseVar names) loc
  TreeParens (TreeVar ('-' :| ">") _ : trees) loc ->
    case trees of
      [] -> ItemErr $ ErrEmptyExport loc
      _ -> Export (map parseVar trees) loc
  TreeBraces trees loc ->
    case trees of
      [] -> ItemErr $ ErrEmptyDeclare loc
      [_] -> ItemErr $ ErrOneDeclare loc
      [name, exp] -> Declare (parseVar name) (parseExp exp) loc
      _ -> ItemErr $ ErrExcessDeclare loc
  TreeParens (TreeVar ('=' :| "") _ : trees) loc -> case trees of
    [] -> ItemErr $ ErrEmptyDef loc
    [_] -> ItemErr $ ErrOneDef loc
    [name, exp] -> Def (parseVar name) (parseExp exp) loc
    _ -> ItemErr $ ErrExcessDef loc
  tree -> ItemErr $ ErrNoItem (locate tree)

parseExp :: Tree -> Exp
parseExp tree = case parseType tree of
  Just exp -> ExpType exp (locate tree)
  Nothing -> case parseData parseExp ExpData tree of
    Just exp -> ExpData exp (locate tree)
    Nothing -> case tree of
      TreeBraces trees loc ->
        case trees of
          [] -> ExpErr $ ErrEmptyTyped loc
          [_] -> ExpErr $ ErrOneTyped loc
          [ty, exp] -> ExpTyped (parseExp ty) (parseExp exp) loc
          _ -> ExpErr $ ErrExcessTyped loc
      TreeParens (TreeVar ('-' :| ">") _ : trees) loc ->
        case trees of
          [] -> ExpErr $ ErrEmptyFun loc
          [_] -> ExpErr $ ErrOneFun loc
          a : b : trees ->
            let go a = \case
                  [] -> parseExp a
                  b : trees -> ExpFun (parsePat a) (go b trees) loc
             in ExpFun (parsePat a) (go b trees) loc
      TreeParens (TreeVar ('?' :| "") _ : trees) loc ->
        case map parseExp trees of
          [] -> ExpErr $ ErrEmptyCase loc
          [_] -> ExpErr $ ErrOneCase loc
          a : b : trees ->
            let go a = \case
                  [] -> a
                  b : trees -> ExpCase a (go b trees) loc
             in ExpCase a (go b trees) loc
      TreeParens (TreeVar ('d' :| "o") _ : trees) loc ->
        case trees of
          [] -> ExpErr $ ErrEmptyDo loc
          [_] -> ExpErr $ ErrOneDo loc
          [_, _] -> ExpErr $ ErrTwoDo loc
          a : b : c : trees ->
            let go a = \case
                  [] -> parseExp a
                  [_] -> ExpErr $ ErrOneDo loc
                  b : c : trees -> ExpDo (parsePat b) (parseExp c) (go a trees) loc
             in ExpDo (parsePat a) (parseExp b) (go c trees) loc
      TreeParens (TreeVar ('=' :| "") _ : trees) loc ->
        case trees of
          [] -> ExpErr $ ErrEmptyLet loc
          [_] -> ExpErr $ ErrOneLet loc
          [_, _] -> ExpErr $ ErrTwoLet loc
          a : b : c : trees ->
            let go a = \case
                  [] -> parseExp a
                  [_] -> ExpErr $ ErrOneLet loc
                  b : c : trees -> ExpLet (parsePat b) (parseExp c) (go a trees) loc
             in ExpLet (parsePat a) (parseExp b) (go c trees) loc
      TreeParens trees loc ->
        case map parseExp trees of
          [] -> ExpErr $ ErrEmptyApp loc
          [_] -> ExpErr $ ErrOneApp loc
          a : b : trees ->
            let go a = \case
                  [] -> a
                  b : trees -> go (ExpApp a b loc) trees
             in go (ExpApp a b loc) trees
      _ -> ExpErr $ ErrNoExp (locate tree)

parseType :: Tree -> Maybe Type
parseType = \case
  TreeVar ('S' :| "tring") loc -> Just $ TypeStr loc
  TreeVar ('C' :| "har") loc -> Just $ TypeChar loc
  TreeVar ('F' :| "loat") loc -> Just $ TypeFloat loc
  TreeVar ('I' :| "nt") loc -> Just $ TypeInt loc
  TreeParens (TreeVar ('=' :| ">") _ : trees) loc ->
    Just $ case map parseExp trees of
      [] -> TypeErr $ ErrEmptyFun loc
      [_] -> TypeErr $ ErrOneFun loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> ExpType (TypeFun a (go b trees) loc) loc
         in TypeFun a (go b trees) loc
  TreeVar ('D' :| "o") loc ->
    Just $ TypeDo (ExpData (DataInt ('0' :| "") loc) loc) loc
  TreeParens (TreeVar ('D' :| "o") _ : trees) loc ->
    Just $ case trees of
      [] -> TypeErr $ ErrEmptyDo loc
      [exp] -> TypeDo (parseExp exp) loc
      _ -> TypeErr $ ErrExcessDo loc
  TreeParens (TreeVar ('S' :| "et") _ : trees) loc ->
    Just $ case map parseExp trees of
      [] -> TypeErr $ ErrEmptySet loc
      [_] -> TypeErr $ ErrOneSet loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> ExpType (TypeSet a (go b trees) loc) loc
         in TypeSet a (go b trees) loc
  TreeParens (TreeVar ('F' :| "or") _ : trees) loc ->
    Just $ case trees of
      [] -> TypeErr $ ErrEmptyFor loc
      [_] -> TypeErr $ ErrOneFor loc
      a : b : trees ->
        let go a = \case
              [] -> parseExp a
              b : trees -> ExpType (TypeFor (parseVar a) (go b trees) loc) loc
         in TypeFor (parseVar a) (go b trees) loc
  TreeVar ('T' :| "ype") loc ->
    Just $ TypeKind (ExpData (DataInt ('0' :| "") loc) loc) loc
  TreeParens (TreeVar ('T' :| "ype") _ : trees) loc ->
    Just $ case trees of
      [] -> TypeErr $ ErrEmptyKind loc
      [exp] -> TypeKind (parseExp exp) loc
      _ -> TypeErr $ ErrExcessKind loc
  _ -> Nothing

parsePat :: Tree -> Pat
parsePat tree = case parseData parsePat Pat tree of
  Just pat -> Pat pat (locate tree)
  Nothing -> PatErr $ ErrNoPat (locate tree)

parseData :: (Tree -> a) -> (Data a -> Loc -> a) -> Tree -> Maybe (Data a)
parseData parse ctor = \case
  TreeBrackets trees loc ->
    case map parse trees of
      [] -> Just $ DataErr $ ErrEmptyCons loc
      [_] -> Just $ DataErr $ ErrOneCons loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> ctor (DataCons a (go b trees) loc) loc
         in Just $ DataCons a (go b trees) loc
  TreeStr var loc -> Just $ DataStr var loc
  TreeChar var loc -> Just $ DataChar var loc
  TreeFloat var loc -> Just $ DataFloat var loc
  TreeInt var loc -> Just $ DataInt var loc
  TreeVar var loc -> Just $ DataVar var loc
  _ -> Nothing

parseVar :: Tree -> Var
parseVar = \case
  TreeVar var loc -> Var var loc
  tree -> VarErr $ ErrNoVar (locate tree)
