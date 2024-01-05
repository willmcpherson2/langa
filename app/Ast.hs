{-# LANGUAGE UndecidableInstances #-}

module Ast
  ( Ast,
    ExpAst,
    TypedAst,
    TermAst,
    Item (..),
    Import (..),
    Export (..),
    Declare (..),
    Def (..),
    Exp (..),
    Typed (..),
    Term (..),
    Fun (..),
    Case (..),
    Do (..),
    Let (..),
    App (..),
    Type (..),
    StrType (..),
    CharType (..),
    FloatType (..),
    IntType (..),
    FunType (..),
    DoType (..),
    Set (..),
    For (..),
    Kind (..),
    Pat (..),
    Data (..),
    Cons (..),
    Str (..),
    Chr (..),
    Flt (..),
    Intg (..),
    Var (..),
    Fix (..),
    Fix2 (..),
    Loc,
    parseAst,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Token
import Tree

type Ast a b = [Item a b] -- item*

type ExpAst = Ast (Fix2 Exp) (Fix2 Exp) -- {exp exp}

type TypedAst = Ast (Fix Type) (Fix2 Exp) -- {type exp}

type TermAst = Ast () (Fix Term) -- term

--------------------------------------------------------------------------------

data Item a b
  = ItemImport Import
  | ItemExport Export
  | ItemDeclare (Declare a)
  | ItemDef (Def a b)
  | ItemNone Loc
  deriving (Show)

data Import
  = Import Var [Var] Loc -- (<- name name*)
  | ImportZero Loc
  deriving (Show)

data Export
  = Export [Var] Loc -- (-> name*)
  | ExportZero Loc
  deriving (Show)

data Declare a
  = Declare Var a Loc -- {name type}
  | DeclareZero Loc
  | DeclareOne Loc
  | DeclareMore Loc
  deriving (Show)

data Def a b
  = Def Var (Exp a b) Loc -- (= name exp)
  | DefZero Loc
  | DefOne Loc
  | DefMore Loc
  deriving (Show)

--------------------------------------------------------------------------------

data Exp a b
  = ExpTyped (Typed a b)
  | ExpType (Type a)
  | ExpTerm (Term b)
  | ExpNone Loc
  deriving (Show)

--------------------------------------------------------------------------------

data Typed a b
  = Typed a b Loc -- {type exp}
  | TypedZero Loc
  | TypedOne Loc
  | TypedMore Loc
  deriving (Show)

--------------------------------------------------------------------------------

data Type a
  = TypeData (Data a)
  | TypeStr StrType
  | TypeChar CharType
  | TypeFloat FloatType
  | TypeInt IntType
  | TypeFun (FunType a)
  | TypeDo (DoType a)
  | TypeSet (Set a)
  | TypeFor (For a)
  | TypeKind (Kind a)
  deriving (Show)

newtype StrType = StrType Loc -- String
  deriving (Show)

newtype CharType = CharType Loc -- Char
  deriving (Show)

newtype FloatType = FloatType Loc -- Float
  deriving (Show)

newtype IntType = IntType Loc -- Int
  deriving (Show)

data FunType a
  = FunType a a Loc -- (=> type type+)
  | FunTypeZero Loc
  | FunTypeOne Loc
  deriving (Show)

data DoType a
  = DoType a Loc -- (Do type)
  | DoTypeZero Loc
  | DoTypeMany Loc
  deriving (Show)

data Set a
  = Set a a Loc -- (Set type type+)
  | SetZero Loc
  | SetOne Loc
  deriving (Show)

data For a
  = For Var a Loc -- (For name+ type)
  | ForZero Loc
  | ForOne Loc
  deriving (Show)

data Kind a
  = Kind a Loc -- (Type n)
  | KindZero Loc
  | KindMany Loc
  deriving (Show)

--------------------------------------------------------------------------------

data Term b
  = TermData (Data b)
  | TermFun (Fun b)
  | TermCase (Case b)
  | TermDo (Do b)
  | TermLet (Let b)
  | TermApp (App b)
  deriving (Show)

data Fun b
  = Fun Pat b Loc -- (-> pat+ exp)
  | FunZero Loc
  | FunOne Loc
  deriving (Show)

data Case b
  = Case b b Loc -- (? exp exp+)
  | CaseZero Loc
  | CaseOne Loc
  deriving (Show)

data Do b
  = Do Pat b b Loc -- (do (pat exp)+ exp)
  | DoZero Loc
  | DoOne Loc
  | DoTwo Loc
  deriving (Show)

data Let b
  = Let Pat b b Loc -- (= (pat exp)+ exp)
  | LetZero Loc
  | LetOne Loc
  | LetTwo Loc
  deriving (Show)

data App b
  = App b b Loc -- (exp exp+)
  | AppZero Loc
  | AppOne Loc
  deriving (Show)

--------------------------------------------------------------------------------

data Pat
  = Pat (Data Pat) Loc -- pat
  | PatNone Loc
  deriving (Show)

--------------------------------------------------------------------------------

data Data a
  = DataCons (Cons a)
  | DataStr Str
  | DataChar Chr
  | DataFloat Flt
  | DataInt Intg
  | DataVar Var
  deriving (Show)

data Cons a
  = Cons a a Loc -- [data data+]
  | ConsZero Loc
  | ConsOne Loc
  deriving (Show)

data Str = Str String Loc -- "foo"
  deriving (Show)

data Chr = Chr Char Loc -- 'a
  deriving (Show)

data Flt = Flt Name Loc -- 3.14
  deriving (Show)

data Intg = Intg Name Loc -- 42
  deriving (Show)

--------------------------------------------------------------------------------

data Var
  = Var Name Loc -- x
  | VarNone Loc
  deriving (Show)

--------------------------------------------------------------------------------

newtype Fix f = Fix {unfix :: OfFix f}

type OfFix f = f (Fix f)

instance (Show (OfFix f)) => Show (Fix f) where
  show x = "(Fix (" <> show (unfix x) <> "))"

newtype Fix2 f = Fix2 {unfix2 :: OfFix2 f}

type OfFix2 f = f (Fix2 f) (Fix2 f)

instance (Show (OfFix2 f)) => Show (Fix2 f) where
  show x = "(Fix2 (" <> show (unfix2 x) <> "))"

--------------------------------------------------------------------------------

type Loc = ([Token], [Token], [Token])

--------------------------------------------------------------------------------

parseAst :: [Tree] -> ExpAst
parseAst = map parseItem

parseItem :: Tree -> Item (Fix2 Exp) (Fix2 Exp)
parseItem = \case
  TreeParens (TreeVar ('<' :| "-") _ : trees) loc ->
    ItemImport $ case trees of
      [] -> ImportZero loc
      name : names -> Import (parseVar name) (map parseVar names) loc
  TreeParens (TreeVar ('-' :| ">") _ : trees) loc ->
    ItemExport $ case trees of
      [] -> ExportZero loc
      _ -> Export (map parseVar trees) loc
  TreeBraces trees loc ->
    ItemDeclare $ case trees of
      [] -> DeclareZero loc
      [_] -> DeclareOne loc
      [name, exp] -> Declare (parseVar name) (Fix2 $ parseExp exp) loc
      _ -> DeclareMore loc
  TreeParens (TreeVar ('=' :| "") _ : trees) loc ->
    ItemDef $ case trees of
      [] -> DefZero loc
      [_] -> DefOne loc
      [name, exp] -> Def (parseVar name) (parseExp exp) loc
      _ -> DefMore loc
  tree -> ItemNone $ locate tree

parseExp :: Tree -> Exp (Fix2 Exp) (Fix2 Exp)
parseExp tree = case parseTyped tree of
  Just typed -> ExpTyped typed
  Nothing -> case parseType tree of
    Just ty -> ExpType ty
    Nothing -> case parseData parseExp Fix2 (ExpTerm . TermData) tree of
      Just dat -> ExpTerm $ TermData dat
      Nothing -> case parseTerm tree of
        Just term -> ExpTerm term
        Nothing -> ExpNone (locate tree)

parseTyped :: Tree -> Maybe (Typed (Fix2 Exp) (Fix2 Exp))
parseTyped = \case
  TreeBraces trees loc ->
    Just $ case trees of
      [] -> TypedZero loc
      [_] -> TypedOne loc
      [ty, exp] -> Typed (Fix2 $ parseExp ty) (Fix2 $ parseExp exp) loc
      _ -> TypedMore loc
  _ -> Nothing

parseTerm :: Tree -> Maybe (Term (Fix2 Exp))
parseTerm tree = case tree of
  TreeParens (TreeVar ('-' :| ">") _ : trees) loc ->
    Just . TermFun $ case trees of
      [] -> FunZero loc
      [_] -> FunOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 $ parseExp a
              b : trees -> Fix2 . ExpTerm . TermFun $ Fun (parsePat a) (go b trees) loc
         in Fun (parsePat a) (go b trees) loc
  TreeParens (TreeVar ('?' :| "") _ : trees) loc ->
    Just . TermCase $ case map parseExp trees of
      [] -> CaseZero loc
      [_] -> CaseOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 a
              b : trees -> Fix2 . ExpTerm . TermCase $ Case (Fix2 a) (go b trees) loc
         in Case (Fix2 a) (go b trees) loc
  TreeParens (TreeVar ('d' :| "o") _ : trees) loc ->
    Just . TermDo $ case trees of
      [] -> DoZero loc
      [_] -> DoOne loc
      [_, _] -> DoTwo loc
      a : b : c : trees ->
        let go a = \case
              [] -> Fix2 $ parseExp a
              [_] -> Fix2 . ExpTerm . TermDo $ DoOne loc
              b : c : trees -> Fix2 . ExpTerm . TermDo $ Do (parsePat b) (Fix2 $ parseExp c) (go a trees) loc
         in Do (parsePat a) (Fix2 $ parseExp b) (go c trees) loc
  TreeParens (TreeVar ('=' :| "") _ : trees) loc ->
    Just . TermLet $ case trees of
      [] -> LetZero loc
      [_] -> LetOne loc
      [_, _] -> LetTwo loc
      a : b : c : trees ->
        let go a = \case
              [] -> Fix2 $ parseExp a
              [_] -> Fix2 . ExpTerm . TermLet $ LetOne loc
              b : c : trees -> Fix2 . ExpTerm . TermLet $ Let (parsePat b) (Fix2 $ parseExp c) (go a trees) loc
         in Let (parsePat a) (Fix2 $ parseExp b) (go c trees) loc
  TreeParens trees loc ->
    Just . TermApp $ case map parseExp trees of
      [] -> AppZero loc
      [_] -> AppOne loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> go (App (Fix2 . ExpTerm . TermApp $ a) (Fix2 b) loc) trees
         in go (App (Fix2 a) (Fix2 b) loc) trees
  _ -> Nothing

parseType :: Tree -> Maybe (Type (Fix2 Exp))
parseType = \case
  TreeVar ('S' :| "tring") loc -> Just . TypeStr $ StrType loc
  TreeVar ('C' :| "har") loc -> Just . TypeChar $ CharType loc
  TreeVar ('F' :| "loat") loc -> Just . TypeFloat $ FloatType loc
  TreeVar ('I' :| "nt") loc -> Just . TypeInt $ IntType loc
  TreeParens (TreeVar ('=' :| ">") _ : trees) loc ->
    Just . TypeFun $ case map parseExp trees of
      [] -> FunTypeZero loc
      [_] -> FunTypeOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 a
              b : trees -> Fix2 . ExpType . TypeFun $ FunType (Fix2 a) (go b trees) loc
         in FunType (Fix2 a) (go b trees) loc
  TreeVar ('D' :| "o") loc ->
    Just . TypeDo $ DoType (Fix2 . ExpTerm $ TermData (DataInt $ Intg ('0' :| "") loc)) loc
  TreeParens (TreeVar ('D' :| "o") _ : trees) loc ->
    Just . TypeDo $ case trees of
      [] -> DoTypeZero loc
      [exp] -> DoType (Fix2 $ parseExp exp) loc
      _ -> DoTypeMany loc
  TreeParens (TreeVar ('S' :| "et") _ : trees) loc ->
    Just . TypeSet $ case map parseExp trees of
      [] -> SetZero loc
      [_] -> SetOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 a
              b : trees -> Fix2 . ExpType . TypeSet $ Set (Fix2 a) (go b trees) loc
         in Set (Fix2 a) (go b trees) loc
  TreeParens (TreeVar ('F' :| "or") _ : trees) loc ->
    Just . TypeFor $ case trees of
      [] -> ForZero loc
      [_] -> ForOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 $ parseExp a
              b : trees -> Fix2 . ExpType . TypeFor $ For (parseVar a) (go b trees) loc
         in For (parseVar a) (go b trees) loc
  TreeVar ('T' :| "ype") loc ->
    Just . TypeKind $ Kind (Fix2 . ExpTerm $ TermData (DataInt $ Intg ('0' :| "") loc)) loc
  TreeParens (TreeVar ('T' :| "ype") _ : trees) loc ->
    Just . TypeKind $ case trees of
      [] -> KindZero loc
      [exp] -> Kind (Fix2 $ parseExp exp) loc
      _ -> KindMany loc
  _ -> Nothing

parsePat :: Tree -> Pat
parsePat tree = case parseData parsePat id (\dat -> Pat dat (locate tree)) tree of
  Just pat -> Pat pat (locate tree)
  Nothing -> PatNone (locate tree)

parseData :: (Tree -> a) -> (a -> b) -> (Data b -> a) -> Tree -> Maybe (Data b)
parseData parse fix ctor = \case
  TreeBrackets trees loc ->
    Just . DataCons $ case map parse trees of
      [] -> ConsZero loc
      [_] -> ConsOne loc
      a : b : trees ->
        let go a = \case
              [] -> fix a
              b : trees -> fix . ctor . DataCons $ Cons (fix a) (go b trees) loc
         in Cons (fix a) (go b trees) loc
  TreeStr var loc -> Just . DataStr $ Str var loc
  TreeChar var loc -> Just . DataChar $ Chr var loc
  TreeFloat var loc -> Just . DataFloat $ Flt var loc
  TreeInt var loc -> Just . DataInt $ Intg var loc
  TreeVar var loc -> Just . DataVar $ Var var loc
  _ -> Nothing

parseVar :: Tree -> Var
parseVar = \case
  TreeVar var loc -> Var var loc
  tree -> VarNone (locate tree)

--------------------------------------------------------------------------------

noLoc :: Loc
noLoc = ([], [], [])

-- (= id {(For a (=> a a)) (-> x x)})
expAst :: ExpAst
expAst =
  [ ItemDef $
      Def
        (Var ('i' :| "d") noLoc)
        ( ExpTyped $
            Typed
              ( Fix2 . ExpType . TypeFor $
                  For
                    (Var ('a' :| "") noLoc)
                    ( Fix2 . ExpType . TypeFun $
                        FunType
                          (Fix2 . ExpType . TypeData . DataVar $ Var ('a' :| "") noLoc)
                          (Fix2 . ExpType . TypeData . DataVar $ Var ('a' :| "") noLoc)
                          noLoc
                    )
                    noLoc
              )
              ( Fix2 . ExpTerm . TermFun $
                  Fun
                    ( Pat
                        (DataVar $ Var ('x' :| "") noLoc)
                        noLoc
                    )
                    (Fix2 . ExpTerm . TermData . DataVar $ Var ('x' :| "") noLoc)
                    noLoc
              )
              noLoc
        )
        noLoc
  ]

-- (= id {(For a (=> a a)) (-> x x)})
typedAst :: TypedAst
typedAst =
  [ ItemDef $
      Def
        (Var ('i' :| "d") noLoc)
        ( ExpTyped $
            Typed
              ( Fix . TypeFor $
                  For
                    (Var ('a' :| "") noLoc)
                    ( Fix . TypeFun $
                        FunType
                          (Fix . TypeData . DataVar $ Var ('a' :| "") noLoc)
                          (Fix . TypeData . DataVar $ Var ('a' :| "") noLoc)
                          noLoc
                    )
                    noLoc
              )
              ( Fix2 . ExpTerm . TermFun $
                  Fun
                    ( Pat
                        (DataVar $ Var ('x' :| "") noLoc)
                        noLoc
                    )
                    (Fix2 . ExpTerm . TermData . DataVar $ Var ('x' :| "") noLoc)
                    noLoc
              )
              noLoc
        )
        noLoc
  ]

-- (= id (-> x x))
termAst :: TermAst
termAst =
  [ ItemDef $
      Def
        (Var ('i' :| "d") noLoc)
        ( ExpTyped $
            Typed
              ()
              ( Fix . TermFun $
                  Fun
                    ( Pat
                        (DataVar $ Var ('x' :| "") noLoc)
                        noLoc
                    )
                    (Fix . TermData . DataVar $ Var ('x' :| "") noLoc)
                    noLoc
              )
              noLoc
        )
        noLoc
  ]
