module Check (checkAst) where

import Ast

checkAst :: ExpAst -> TermAst
checkAst = map checkItem

checkItem :: Item (Fix2 Exp) -> Item (Fix Term)
checkItem = \case
  ItemGlobal global -> ItemGlobal $ checkGlobal global
  ItemLocal local -> ItemLocal $ checkLocal local
  ItemNone loc -> ItemNone loc

checkGlobal :: Global (Fix2 Exp) -> Global (Fix Term)
checkGlobal = \case
  Global var exp loc -> Global var (checkExp exp) loc
  GlobalZero loc -> GlobalZero loc
  GlobalOne loc -> GlobalOne loc
  GlobalMore loc -> GlobalMore loc

checkLocal :: Local (Fix2 Exp) -> Local (Fix Term)
checkLocal = \case
  Local var exp loc -> Local var (checkExp exp) loc
  LocalZero loc -> LocalZero loc
  LocalOne loc -> LocalOne loc
  LocalMore loc -> LocalMore loc

checkExp :: Fix2 Exp -> Fix Term
checkExp = \case
  Fix2 (ExpTyped _) -> error "unimplemented"
  Fix2 (ExpType _) -> error "unimplemented"
  Fix2 (ExpTerm term) -> Fix $ checkTerm term
  Fix2 (ExpNone _) -> error "unimplemented"

checkTerm :: Term (Fix2 Exp) -> OfFix Term
checkTerm = \case
  TermData dat -> TermData $ checkData dat
  TermFun fun -> TermFun $ checkFun fun
  TermCase cas -> TermCase $ checkCase cas
  TermDo d -> TermDo $ checkDo d
  TermLet le -> TermLet $ checkLet le
  TermApp app -> TermApp $ checkApp app

checkData :: Data (Fix2 Exp) -> Data (Fix Term)
checkData = \case
  DataCons cons -> DataCons $ checkCons cons
  DataChar char -> DataChar char
  DataFloat float -> DataFloat float
  DataInt int -> DataInt int
  DataNat nat -> DataNat nat
  DataNil nil -> DataNil nil
  DataVar var -> DataVar var

checkFun :: Fun (Fix2 Exp) -> Fun (Fix Term)
checkFun = \case
  Fun pat exp loc -> Fun pat (checkExp exp) loc
  FunZero loc -> FunZero loc
  FunOne loc -> FunOne loc

checkCase :: Case (Fix2 Exp) -> Case (Fix Term)
checkCase = \case
  Case l r loc -> Case (checkExp l) (checkExp r) loc
  CaseZero loc -> CaseZero loc
  CaseOne loc -> CaseOne loc

checkDo :: Do (Fix2 Exp) -> Do (Fix Term)
checkDo = \case
  Do pat l r loc -> Do pat (checkExp l) (checkExp r) loc
  DoZero loc -> DoZero loc
  DoOne loc -> DoOne loc
  DoTwo loc -> DoTwo loc

checkLet :: Let (Fix2 Exp) -> Let (Fix Term)
checkLet = \case
  Let pat l r loc -> Let pat (checkExp l) (checkExp r) loc
  LetZero loc -> LetZero loc
  LetOne loc -> LetOne loc
  LetTwo loc -> LetTwo loc

checkApp :: App (Fix2 Exp) -> App (Fix Term)
checkApp = \case
  App l r loc -> App (checkExp l) (checkExp r) loc
  AppZero loc -> AppZero loc
  AppOne loc -> AppOne loc

checkCons :: Cons (Fix2 Exp) -> Cons (Fix Term)
checkCons = \case
  Cons l r loc -> Cons (checkExp l) (checkExp r) loc
  ConsZero loc -> ConsZero loc
  ConsOne loc -> ConsOne loc
