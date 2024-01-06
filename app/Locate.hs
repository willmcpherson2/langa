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
    TreeStr (StrLit _ _ loc) -> loc
    TreeChar (CharLit _ loc) -> loc
    TreeFloat (FloatLit _ loc) -> loc
    TreeInt (IntLit _ loc) -> loc
    TreeVar (Var _ loc) -> loc
    TreeVar (VarNone loc) -> loc
