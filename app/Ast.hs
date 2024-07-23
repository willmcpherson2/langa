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
    SymLit (..),
    NilLit (..),
    Var (..),
    Chars,
    Loc,
  )
where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))

--------------------------------------------------------------------------------

type Ast = [Item]

--------------------------------------------------------------------------------

data Item
  = ItemGlobal Global
  | ItemLocal Local
  | ItemNone Loc
  deriving (Show, Data)

data Global
  = Global Var Typed Loc -- (<- var typed)
  | GlobalZero Loc
  | GlobalOne Loc
  | GlobalMore Loc
  deriving (Show, Data)

data Local
  = Local Var Typed Loc -- (= var typed)
  | LocalZero Loc
  | LocalOne Loc
  | LocalMore Loc
  deriving (Show, Data)

--------------------------------------------------------------------------------

data Typed
  = TypedAnn Ann
  | TypedFor For
  | TypedType Type
  | TypedNone Loc
  deriving (Show, Data)

data Ann
  = Ann Typed Exp Loc -- (: typed exp)
  | AnnZero Loc
  | AnnOne Loc
  deriving (Show, Data)

data For
  = For Var Exp Typed Loc -- (A var exp typed)
  | ForZero Loc
  | ForOne Loc
  | ForTwo Loc
  deriving (Show, Data)

newtype Type = Type Loc -- Type
  deriving (Show, Data)

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
  | ExpSym SymLit
  | ExpNil NilLit
  | ExpVar Var
  | ExpNone Loc
  deriving (Show, Data)

data Set
  = Set Typed Typed Loc -- (Set typed typed)
  | SetZero Loc
  | SetOne Loc
  deriving (Show, Data)

data FunType
  = FunType Typed Typed Loc -- (=> typed typed)
  | FunTypeZero Loc
  | FunTypeOne Loc
  deriving (Show, Data)

data DoType
  = DoType Typed Loc -- (Do typed)
  | DoTypeZero Loc
  | DoTypeMany Loc
  deriving (Show, Data)

newtype FloatType = FloatType Loc -- Float
  deriving (Show, Data)

newtype IntType = IntType Loc -- Int
  deriving (Show, Data)

newtype NatType = NatType Loc -- Nat
  deriving (Show, Data)

newtype CharType = CharType Loc -- Char
  deriving (Show, Data)

data Infer = Infer Int Loc -- #
  deriving (Show, Data)

data Fun
  = Fun Typed Typed Loc -- (-> typed typed)
  | FunZero Loc
  | FunOne Loc
  deriving (Show, Data)

data Case
  = Case Typed Typed Loc -- (? typed typed)
  | CaseZero Loc
  | CaseOne Loc
  deriving (Show, Data)

data Do
  = Do Typed Typed Typed Loc -- (do typed typed typed)
  | DoZero Loc
  | DoOne Loc
  | DoTwo Loc
  deriving (Show, Data)

data Let
  = Let Typed Typed Typed Loc -- (= typed typed typed)
  | LetZero Loc
  | LetOne Loc
  | LetTwo Loc
  deriving (Show, Data)

data App
  = App Typed Typed Loc -- (typed typed)
  | AppZero Loc
  | AppOne Loc
  deriving (Show, Data)

data Cons
  = Cons Typed Typed Loc -- [typed typed]
  | ConsZero Loc
  | ConsOne Loc
  deriving (Show, Data)

data FloatLit = FloatLit Float Loc -- 3.14
  deriving (Show, Data)

data IntLit = IntLit Int Loc -- -7
  deriving (Show, Data)

data NatLit = NatLit Int Loc -- 42
  deriving (Show, Data)

data CharLit = CharLit Char Loc -- 'a'
  deriving (Show, Data)

data SymLit = SymLit String Loc -- :foo
  deriving (Show, Data)

newtype NilLit = NilLit Loc -- nil
  deriving (Show, Data)

data Var
  = Var Chars Loc -- x
  | VarNone Loc
  deriving (Show, Data)

--------------------------------------------------------------------------------

type Chars = NonEmpty Char

type Loc = (String, String, String)
