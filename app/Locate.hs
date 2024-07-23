module Locate (Locate (..)) where

import Ast
import Tree

class Locate a where
  locate :: a -> Loc

instance Locate Tree where
  locate = \case
    TreeParens _ loc -> loc
    TreeBrackets _ loc -> loc
    TreeBraces _ loc -> loc
    TreeChar (CharLit _ loc) -> loc
    TreeFloat (FloatLit _ loc) -> loc
    TreeInt (IntLit _ loc) -> loc
    TreeNat (NatLit _ loc) -> loc
    TreeVar (Var _ loc) -> loc
    TreeVar (VarNone loc) -> loc

instance Locate Exp where
  locate = \case
    TypeSet (Set _ _ loc) -> loc
    TypeSet (SetZero loc) -> loc
    TypeSet (SetOne loc) -> loc
    TypeFun (FunType _ _ loc) -> loc
    TypeFun (FunTypeZero loc) -> loc
    TypeFun (FunTypeOne loc) -> loc
    TypeDo (DoType _ loc) -> loc
    TypeDo (DoTypeZero loc) -> loc
    TypeDo (DoTypeMany loc) -> loc
    TypeFloat (FloatType loc) -> loc
    TypeInt (IntType loc) -> loc
    TypeNat (NatType loc) -> loc
    TypeChar (CharType loc) -> loc
    TypeInfer (Infer _ loc) -> loc
    TermCase (Case _ _ loc) -> loc
    TermCase (CaseZero loc) -> loc
    TermCase (CaseOne loc) -> loc
    TermFun (Fun _ _ loc) -> loc
    TermFun (FunZero loc) -> loc
    TermFun (FunOne loc) -> loc
    TermDo (Do _ _ _ loc) -> loc
    TermDo (DoZero loc) -> loc
    TermDo (DoOne loc) -> loc
    TermDo (DoTwo loc) -> loc
    TermLet (Let _ _ _ loc) -> loc
    TermLet (LetZero loc) -> loc
    TermLet (LetOne loc) -> loc
    TermLet (LetTwo loc) -> loc
    TermApp (App _ _ loc) -> loc
    TermApp (AppZero loc) -> loc
    TermApp (AppOne loc) -> loc
    ExpCons (Cons _ _ loc) -> loc
    ExpCons (ConsZero loc) -> loc
    ExpCons (ConsOne loc) -> loc
    ExpFloat (FloatLit _ loc) -> loc
    ExpInt (IntLit _ loc) -> loc
    ExpNat (NatLit _ loc) -> loc
    ExpChar (CharLit _ loc) -> loc
    ExpNil (NilLit loc) -> loc
    ExpVar (Var _ loc) -> loc
    ExpVar (VarNone loc) -> loc
    ExpNone loc -> loc
