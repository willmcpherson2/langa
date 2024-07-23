module Ast
  ( Ast,
    Item (..),
    Global (..),
    Local (..),
    Typed (..),
    Ann (..),
    For (..),
    Exp (..),
    Set (..),
    FunType (..),
    DoType (..),
    FloatType (..),
    IntType (..),
    NatType (..),
    CharType (..),
    Type (..),
    Infer (..),
    Case (..),
    Fun (..),
    Do (..),
    Let (..),
    App (..),
    Cons (..),
    FloatLit (..),
    IntLit (..),
    NatLit (..),
    CharLit (..),
    NilLit (..),
    Var (..),
    Chars,
    Loc,
  )
where

import Data.List.NonEmpty (NonEmpty (..))

--------------------------------------------------------------------------------

type Ast = [Item]

--------------------------------------------------------------------------------

data Item
  = ItemGlobal Global
  | ItemLocal Local
  | ItemNone Loc
  deriving (Show)

data Global
  = Global Var Typed Loc -- (<- var typed)
  | GlobalZero Loc
  | GlobalOne Loc
  | GlobalMore Loc
  deriving (Show)

data Local
  = Local Var Typed Loc -- (= var typed)
  | LocalZero Loc
  | LocalOne Loc
  | LocalMore Loc
  deriving (Show)

--------------------------------------------------------------------------------

data Typed
  = TypedAnn Ann
  | TypedFor For
  | TypedType Type
  | TypedNone Loc
  deriving (Show)

data Ann
  = Ann Typed Exp Loc -- (: typed exp)
  | AnnZero Loc
  | AnnOne Loc
  deriving (Show)

data For
  = For Var Exp Typed Loc -- (A var exp typed)
  | ForZero Loc
  | ForOne Loc
  | ForTwo Loc
  deriving (Show)

newtype Type = Type Loc -- Type
  deriving (Show)

--------------------------------------------------------------------------------

data Exp
  = TypeSet Set
  | TypeFun FunType
  | TypeDo DoType
  | TypeFloat FloatType
  | TypeInt IntType
  | TypeNat NatType
  | TypeChar CharType
  | TypeInfer Infer
  | TermCase Case
  | TermFun Fun
  | TermDo Do
  | TermLet Let
  | TermApp App
  | ExpCons Cons
  | ExpFloat FloatLit
  | ExpInt IntLit
  | ExpNat NatLit
  | ExpChar CharLit
  | ExpNil NilLit
  | ExpVar Var
  | ExpNone Loc
  deriving (Show)

data Set
  = Set Typed Typed Loc -- (Set typed typed)
  | SetZero Loc
  | SetOne Loc
  deriving (Show)

data FunType
  = FunType Typed Typed Loc -- (=> typed typed)
  | FunTypeZero Loc
  | FunTypeOne Loc
  deriving (Show)

data DoType
  = DoType Typed Loc -- (Do typed)
  | DoTypeZero Loc
  | DoTypeMany Loc
  deriving (Show)

newtype FloatType = FloatType Loc -- Float
  deriving (Show)

newtype IntType = IntType Loc -- Int
  deriving (Show)

newtype NatType = NatType Loc -- Nat
  deriving (Show)

newtype CharType = CharType Loc -- Char
  deriving (Show)

data Infer = Infer Int Loc -- #
  deriving (Show)

data Fun
  = Fun Typed Typed Loc -- (-> typed typed)
  | FunZero Loc
  | FunOne Loc
  deriving (Show)

data Case
  = Case Typed Typed Loc -- (? typed typed)
  | CaseZero Loc
  | CaseOne Loc
  deriving (Show)

data Do
  = Do Typed Typed Typed Loc -- (do typed typed typed)
  | DoZero Loc
  | DoOne Loc
  | DoTwo Loc
  deriving (Show)

data Let
  = Let Typed Typed Typed Loc -- (= typed typed typed)
  | LetZero Loc
  | LetOne Loc
  | LetTwo Loc
  deriving (Show)

data App
  = App Typed Typed Loc -- (typed typed)
  | AppZero Loc
  | AppOne Loc
  deriving (Show)

data Cons
  = Cons Typed Typed Loc -- [typed typed]
  | ConsZero Loc
  | ConsOne Loc
  deriving (Show)

data FloatLit = FloatLit Float Loc -- 3.14
  deriving (Show)

data IntLit = IntLit Int Loc -- -7
  deriving (Show)

data NatLit = NatLit Int Loc -- 42
  deriving (Show)

data CharLit = CharLit Char Loc -- 'a'
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
