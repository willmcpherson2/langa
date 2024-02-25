{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ast
  ( Ast,
    ExpAst,
    TypedAst,
    TermAst,
    Item (..),
    Global (..),
    Local (..),
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
    NatType (..),
    FunType (..),
    DoType (..),
    Set (..),
    For (..),
    Kind (..),
    Pat (..),
    Data (..),
    Cons (..),
    CharLit (..),
    FloatLit (..),
    IntLit (..),
    NatLit (..),
    NilLit (..),
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
  = ItemGlobal (Global a)
  | ItemLocal (Local a)
  | ItemNone Loc
  deriving (Show)

data Global a
  = Global Var a Loc -- (<- name exp)
  | GlobalZero Loc
  | GlobalOne Loc
  | GlobalMore Loc
  deriving (Show)

data Local a
  = Local Var a Loc -- (= name exp)
  | LocalZero Loc
  | LocalOne Loc
  | LocalMore Loc
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
  = Typed a b Loc -- (: type exp)
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
  | TypeNat NatType
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

newtype NatType = NatType Loc -- Nat
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
  = Set a a Loc -- {type type+}
  | SetZero Loc
  | SetOne Loc
  deriving (Show)

data For a
  = For Var a Loc -- (A name+ type)
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
  | DataChar CharLit
  | DataFloat FloatLit
  | DataInt IntLit
  | DataNat NatLit
  | DataNil NilLit
  | DataVar Var
  deriving (Show)

data Cons a
  = Cons a a Loc -- [data data+]
  | ConsZero Loc
  | ConsOne Loc
  deriving (Show)

data CharLit = CharLit Char Loc -- 'a'
  deriving (Show)

data FloatLit = FloatLit Float Loc -- 3.14
  deriving (Show)

data IntLit = IntLit Int Loc -- -7
  deriving (Show)

data NatLit = NatLit Int Loc -- 42
  deriving (Show)

newtype NilLit = NilLit Loc -- nil
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

-- (= id (: (A a (=> a a)) (-> x x)))
expAst :: ExpAst
expAst =
  [ ItemLocal $
      Local
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

-- (= id (: (A a (=> a a)) (-> x x)))
typedAst :: TypedAst
typedAst =
  [ ItemLocal $
      Local
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
  [ ItemLocal $
      Local
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
