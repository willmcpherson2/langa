{-# LANGUAGE UndecidableInstances #-}

module Display (Display (..)) where

import Ast
import Data.List.NonEmpty (toList)
import Tree
import Data.List (intercalate)

class Display a where
  display :: a -> String

report :: Loc -> String -> String
report (l, m, r) msg =
  let (charsBefore, linesBefore) = break (== '\n') l
      lineStart = length $ filter (== '\n') linesBefore
      charStart = length charsBefore
      charEnd =
        if length (lines m) > 1
          then length $ last $ lines m
          else charStart + length m
      lineEnd = lineStart + length (lines m)
   in show (lineStart + 1)
        <> ":"
        <> show (charStart + 1)
        <> "-"
        <> show lineEnd
        <> ":"
        <> show charEnd
        <> "\n"
        <> replicate charStart ' '
        <> "↓"
        <> "\n"
        <> reverse charsBefore
        <> m
        <> takeWhile (/= '\n') r
        <> "\n"
        <> replicate (charEnd - 1) ' '
        <> "↑"
        <> "\n"
        <> msg

instance Display [Tree] where
  display = intercalate "\n" . map display

instance Display Tree where
  display = \case
    TreeParens trees _ -> "(" <> unwords (map display trees) <> ")"
    TreeBrackets trees _ -> "[" <> unwords (map display trees) <> "]"
    TreeBraces trees _ -> "{" <> unwords (map display trees) <> "}"
    TreeChar char -> display char
    TreeFloat float -> display float
    TreeInt int -> display int
    TreeNat nat -> display nat
    TreeNil nil -> display nil
    TreeVar var -> display var

instance (Display a) => Display (Ast a) where
  display items = unlines $ display <$> items

instance (Display a) => Display (Item a) where
  display = \case
    ItemGlobal global -> display global
    ItemLocal local -> display local
    ItemNone loc -> report loc "Expected item"

instance (Display a) => Display (Global a) where
  display = \case
    Global var exp _ -> "(<- " <> display var <> " " <> display exp <> ")"
    GlobalZero loc -> report loc "Expected variable and expression"
    GlobalOne loc -> report loc "Expected variable and expression"
    GlobalMore loc -> report loc "Expected variable and expression"

instance (Display a) => Display (Local a) where
  display = \case
    Local var exp _ -> "(= " <> display var <> " " <> display exp <> ")"
    LocalZero loc -> report loc "Expected variable and expression"
    LocalOne loc -> report loc "Expected variable and expression"
    LocalMore loc -> report loc "Expected variable and expression"

instance (Display a, Display b) => Display (Exp a b) where
  display = \case
    ExpTyped typed -> display typed
    ExpType ty -> display ty
    ExpTerm term -> display term
    ExpNone loc -> report loc "Expected expression"

instance (Display a, Display b) => Display (Typed a b) where
  display = \case
    Typed exp ty _ -> "(: " <> display exp <> " " <> display ty <> ")"
    TypedZero loc -> report loc "Expected two expressions"
    TypedOne loc -> report loc "Expected two expressions"
    TypedMore loc -> report loc "Expected two expressions"

instance (Display a) => Display (Type a) where
  display = \case
    TypeData dat -> display dat
    TypeChar char -> display char
    TypeFloat float -> display float
    TypeInt int -> display int
    TypeNat nat -> display nat
    TypeFun fun -> display fun
    TypeDo d -> display d
    TypeSet set -> display set
    TypeFor for -> display for
    TypeKind kind -> display kind

instance Display CharType where
  display _ = "Char"

instance Display FloatType where
  display _ = "Float"

instance Display IntType where
  display _ = "Int"

instance Display NatType where
  display _ = "Nat"

instance (Display a) => Display (FunType a) where
  display = \case
    FunType a b _ -> "(=> " <> display a <> " " <> display b <> ")"
    FunTypeZero loc -> report loc "Expected two expressions"
    FunTypeOne loc -> report loc "Expected two expressions"

instance (Display a) => Display (DoType a) where
  display = \case
    DoType ty _ -> "(Do " <> display ty <> ")"
    DoTypeZero loc -> report loc "Expected expression"
    DoTypeMany loc -> report loc "Expected expression"

instance (Display a) => Display (Set a) where
  display = \case
    Set a b _ -> "{" <> display a <> " " <> display b <> "}"
    SetZero loc -> report loc "Expected two expressions"
    SetOne loc -> report loc "Expected two expressions"

instance (Display a) => Display (For a) where
  display = \case
    For var ty _ -> "(A " <> display var <> " " <> display ty <> ")"
    ForZero loc -> report loc "Expected variable and expression"
    ForOne loc -> report loc "Expected variable and expression"

instance (Display a) => Display (Kind a) where
  display = \case
    Kind n _ -> "(Type " <> display n <> ")"
    KindZero loc -> report loc "Expected expression"
    KindMany loc -> report loc "Expected expression"

instance (Display b) => Display (Term b) where
  display = \case
    TermData dat -> display dat
    TermFun fun -> display fun
    TermCase cas -> display cas
    TermDo d -> display d
    TermLet le -> display le
    TermApp app -> display app

instance (Display b) => Display (Fun b) where
  display = \case
    Fun pat body _ -> "(-> " <> display pat <> " " <> display body <> ")"
    FunZero loc -> report loc "Expected pattern and expression"
    FunOne loc -> report loc "Expected pattern and expression"

instance (Display b) => Display (Case b) where
  display = \case
    Case a b _ -> "(? " <> display a <> " " <> display b <> ")"
    CaseZero loc -> report loc "Expected two expressions"
    CaseOne loc -> report loc "Expected two expressions"

instance (Display b) => Display (Do b) where
  display = \case
    Do pat arg body _ -> "(do " <> display pat <> " " <> display arg <> " " <> display body <> ")"
    DoZero loc -> report loc "Expected pattern and two expressions"
    DoOne loc -> report loc "Expected pattern and two expressions"
    DoTwo loc -> report loc "Expected pattern and two expressions"

instance (Display b) => Display (Let b) where
  display = \case
    Let pat arg body _ -> "(= " <> display pat <> " " <> display arg <> " " <> display body <> ")"
    LetZero loc -> report loc "Expected pattern and two expressions"
    LetOne loc -> report loc "Expected pattern and two expressions"
    LetTwo loc -> report loc "Expected pattern and two expressions"

instance (Display b) => Display (App b) where
  display = \case
    App a b _ -> "(" <> display a <> " " <> display b <> ")"
    AppZero loc -> report loc "Expected two expressions"
    AppOne loc -> report loc "Expected two expressions"

instance Display Pat where
  display = \case
    Pat dat _ -> display dat
    PatNone loc -> report loc "Expected data"

instance (Display a) => Display (Data a) where
  display = \case
    DataCons cons -> display cons
    DataChar char -> display char
    DataFloat float -> display float
    DataInt int -> display int
    DataNat nat -> display nat
    DataNil nil -> display nil
    DataVar var -> display var

instance (Display a) => Display (Cons a) where
  display = \case
    Cons a b _ -> "[" <> display a <> " " <> display b <> "]"
    ConsZero loc -> report loc "Expected two expressions"
    ConsOne loc -> report loc "Expected two expressions"

instance Display CharLit where
  display (CharLit char _) = "'" <> [char] <> "'"

instance Display FloatLit where
  display (FloatLit float _) = show float

instance Display IntLit where
  display (IntLit int _) = show int

instance Display NatLit where
  display (NatLit nat _) = show nat

instance Display NilLit where
  display (NilLit _) = "nil"

instance Display Var where
  display = \case
    Var chars _ -> display chars
    VarNone loc -> report loc "Expected a variable"

instance Display Chars where
  display = toList

instance (Display (OfFix f)) => Display (Fix f) where
  display = display . unfix

instance (Display (OfFix2 f)) => Display (Fix2 f) where
  display = display . unfix2
