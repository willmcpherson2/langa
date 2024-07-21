module Display (Display (..)) where

import Ast
import Data.List (intercalate)
import Data.List.NonEmpty (toList)
import Tree

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

instance Display Ast where
  display items = unlines $ display <$> items

instance Display Item where
  display = \case
    ItemGlobal global -> display global
    ItemLocal local -> display local
    ItemNone loc -> report loc "Expected item"

instance Display Global where
  display = \case
    Global var typed _ -> "(<- " <> display var <> " " <> display typed <> ")"
    GlobalZero loc -> report loc "Expected variable and expression"
    GlobalOne loc -> report loc "Expected variable and expression"
    GlobalMore loc -> report loc "Expected variable and expression"

instance Display Local where
  display = \case
    Local var typed _ -> "(= " <> display var <> " " <> display typed <> ")"
    LocalZero loc -> report loc "Expected variable and expression"
    LocalOne loc -> report loc "Expected variable and expression"
    LocalMore loc -> report loc "Expected variable and expression"

instance Display Typed where
  display = \case
    TypedAnn ann -> display ann
    TypedFor for -> display for
    TypedNone loc -> report loc "Expected typed expression"

instance Display Ann where
  display = \case
    Ann ty exp _ -> "(: " <> display ty <> " " <> display exp <> ")"
    AnnZero loc -> report loc "Expected type and expression"
    AnnOne loc -> report loc "Expected type and expression"
    AnnMore loc -> report loc "Expected type and expression"

instance Display For where
  display = \case
    For var ty exp _ -> "(A " <> display var <> " " <> display ty <> " " <> display exp <> ")"
    ForZero loc -> report loc "Expected pattern and two expressions"
    ForOne loc -> report loc "Expected pattern and two expressions"
    ForTwo loc -> report loc "Expected pattern and two expressions"

instance Display Exp where
  display = \case
    TypeSet set -> display set
    TypeFun fun -> display fun
    TypeDo d -> display d
    TypeFloat float -> display float
    TypeInt int -> display int
    TypeNat nat -> display nat
    TypeChar char -> display char
    TypeType ty -> display ty
    TermCase cas -> display cas
    TermFun fun -> display fun
    TermDo d -> display d
    TermLet le -> display le
    TermApp app -> display app
    ExpCons cons -> display cons
    ExpFloat float -> display float
    ExpInt int -> display int
    ExpNat nat -> display nat
    ExpChar char -> display char
    ExpNil nil -> display nil
    ExpVar var -> display var
    ExpNone loc -> report loc "Expected expression"

instance Display Set where
  display = \case
    Set a b _ -> "(Set " <> display a <> " " <> display b <> ")"
    SetZero loc -> report loc "Expected two expressions"
    SetOne loc -> report loc "Expected two expressions"

instance Display FunType where
  display = \case
    FunType a b _ -> "(=> " <> display a <> " " <> display b <> ")"
    FunTypeZero loc -> report loc "Expected two expressions"
    FunTypeOne loc -> report loc "Expected two expressions"

instance Display DoType where
  display = \case
    DoType ty _ -> "(Do " <> display ty <> ")"
    DoTypeZero loc -> report loc "Expected expression"
    DoTypeMany loc -> report loc "Expected expression"

instance Display FloatType where
  display _ = "Float"

instance Display IntType where
  display _ = "Int"

instance Display NatType where
  display _ = "Nat"

instance Display CharType where
  display _ = "Char"

instance Display Type where
  display _ = "Type"

instance Display Case where
  display = \case
    Case a b _ -> "(? " <> display a <> " " <> display b <> ")"
    CaseZero loc -> report loc "Expected two expressions"
    CaseOne loc -> report loc "Expected two expressions"

instance Display Fun where
  display = \case
    Fun pat body _ -> "(-> " <> display pat <> " " <> display body <> ")"
    FunZero loc -> report loc "Expected pattern and expression"
    FunOne loc -> report loc "Expected pattern and expression"

instance Display Do where
  display = \case
    Do pat arg body _ -> "(do " <> display pat <> " " <> display arg <> " " <> display body <> ")"
    DoZero loc -> report loc "Expected pattern and two expressions"
    DoOne loc -> report loc "Expected pattern and two expressions"
    DoTwo loc -> report loc "Expected pattern and two expressions"

instance Display Let where
  display = \case
    Let pat arg body _ -> "(= " <> display pat <> " " <> display arg <> " " <> display body <> ")"
    LetZero loc -> report loc "Expected pattern and two expressions"
    LetOne loc -> report loc "Expected pattern and two expressions"
    LetTwo loc -> report loc "Expected pattern and two expressions"

instance Display App where
  display = \case
    App a b _ -> "(" <> display a <> " " <> display b <> ")"
    AppZero loc -> report loc "Expected two expressions"
    AppOne loc -> report loc "Expected two expressions"

instance Display Cons where
  display = \case
    Cons a b _ -> "[" <> display a <> " " <> display b <> "]"
    ConsZero loc -> report loc "Expected two expressions"
    ConsOne loc -> report loc "Expected two expressions"

instance Display FloatLit where
  display (FloatLit float _) = show float

instance Display IntLit where
  display (IntLit int _) = show int

instance Display NatLit where
  display (NatLit nat _) = show nat

instance Display CharLit where
  display (CharLit char _) = "'" <> [char] <> "'"

instance Display NilLit where
  display (NilLit _) = "nil"

instance Display Var where
  display = \case
    Var chars _ -> display chars
    VarNone loc -> report loc "Expected a variable"

instance Display Chars where
  display = toList
