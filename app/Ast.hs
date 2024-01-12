{-# LANGUAGE StandaloneDeriving #-}
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
    StrLit (..),
    CharLit (..),
    FloatLit (..),
    IntLit (..),
    Var (..),
    Chars,
    Loc,
    Fix (..),
    OfFix,
    Fix2 (..),
    OfFix2,
  )
where

import Data.List.NonEmpty (NonEmpty (..))

type Ast a = [Item a] -- item*

type ExpAst = Ast (Fix2 Exp) -- {exp exp}

type TypedAst = Ast (Exp (Fix Type) (Fix2 Exp)) -- {type exp}

type TermAst = Ast (Fix Term) -- term

--------------------------------------------------------------------------------

data Item a
  = ItemImport Import
  | ItemExport Export
  | ItemDeclare (Declare a)
  | ItemDef (Def a)
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

data Def a
  = Def Var a Loc -- (= name exp)
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
  | TypeChar CharType
  | TypeFloat FloatType
  | TypeInt IntType
  | TypeFun (FunType a)
  | TypeDo (DoType a)
  | TypeSet (Set a)
  | TypeFor (For a)
  | TypeKind (Kind a)
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
  | DataStr StrLit
  | DataChar CharLit
  | DataFloat FloatLit
  | DataInt IntLit
  | DataVar Var
  deriving (Show)

data Cons a
  = Cons a a Loc -- [data data+]
  | ConsZero Loc
  | ConsOne Loc
  deriving (Show)

data StrLit = StrLit String String Loc -- "foo"
  deriving (Show)

data CharLit = CharLit Char Loc -- 'a
  deriving (Show)

data FloatLit = FloatLit Chars Loc -- 3.14
  deriving (Show)

data IntLit = IntLit Chars Loc -- 42
  deriving (Show)

data Var
  = Var Chars Loc -- x
  | VarNone Loc
  deriving (Show)

--------------------------------------------------------------------------------

type Chars = NonEmpty Char

type Loc = (String, String, String)

--------------------------------------------------------------------------------

newtype Fix f = Fix {unfix :: OfFix f}

type OfFix f = f (Fix f)

deriving instance (Show (OfFix f)) => Show (Fix f)

newtype Fix2 f = Fix2 {unfix2 :: OfFix2 f}

type OfFix2 f = f (Fix2 f) (Fix2 f)

deriving instance (Show (OfFix2 f)) => Show (Fix2 f)

--------------------------------------------------------------------------------

noLoc :: Loc
noLoc = ([], [], [])

-- (= id {(For a (=> a a)) (-> x x)})
expAst :: ExpAst
expAst =
  [ ItemDef $
      Def
        (Var ('i' :| "d") noLoc)
        ( Fix2 . ExpTyped $
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
  ]
