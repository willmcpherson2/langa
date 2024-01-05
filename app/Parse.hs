{-# LANGUAGE UndecidableInstances #-}

module Parse (parseAst) where

import Ast
import Data.List.NonEmpty (NonEmpty (..))
import Locate (Locate (..))
import Tree

parseAst :: [Tree] -> ExpAst
parseAst = map parseItem

parseItem :: Tree -> Item (Fix2 Exp) (Fix2 Exp)
parseItem = \case
  TreeParens (TreeVar (Var ('<' :| "-") _) : trees) loc ->
    ItemImport $ case trees of
      [] -> ImportZero loc
      name : names -> Import (parseVar name) (map parseVar names) loc
  TreeParens (TreeVar (Var ('-' :| ">") _) : trees) loc ->
    ItemExport $ case trees of
      [] -> ExportZero loc
      _ -> Export (map parseVar trees) loc
  TreeBraces trees loc ->
    ItemDeclare $ case trees of
      [] -> DeclareZero loc
      [_] -> DeclareOne loc
      [name, exp] -> Declare (parseVar name) (Fix2 $ parseExp exp) loc
      _ -> DeclareMore loc
  TreeParens (TreeVar (Var ('=' :| "") _) : trees) loc ->
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
  TreeParens (TreeVar (Var ('-' :| ">") _) : trees) loc ->
    Just . TermFun $ case trees of
      [] -> FunZero loc
      [_] -> FunOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 $ parseExp a
              b : trees -> Fix2 . ExpTerm . TermFun $ Fun (parsePat a) (go b trees) loc
         in Fun (parsePat a) (go b trees) loc
  TreeParens (TreeVar (Var ('?' :| "") _) : trees) loc ->
    Just . TermCase $ case map parseExp trees of
      [] -> CaseZero loc
      [_] -> CaseOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 a
              b : trees -> Fix2 . ExpTerm . TermCase $ Case (Fix2 a) (go b trees) loc
         in Case (Fix2 a) (go b trees) loc
  TreeParens (TreeVar (Var ('d' :| "o") _) : trees) loc ->
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
  TreeParens (TreeVar (Var ('=' :| "") _) : trees) loc ->
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
  TreeVar (Var ('S' :| "tring") loc) -> Just . TypeStr $ StrType loc
  TreeVar (Var ('C' :| "har") loc) -> Just . TypeChar $ CharType loc
  TreeVar (Var ('F' :| "loat") loc) -> Just . TypeFloat $ FloatType loc
  TreeVar (Var ('I' :| "nt") loc) -> Just . TypeInt $ IntType loc
  TreeParens (TreeVar (Var ('=' :| ">") _) : trees) loc ->
    Just . TypeFun $ case map parseExp trees of
      [] -> FunTypeZero loc
      [_] -> FunTypeOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 a
              b : trees -> Fix2 . ExpType . TypeFun $ FunType (Fix2 a) (go b trees) loc
         in FunType (Fix2 a) (go b trees) loc
  TreeVar (Var ('D' :| "o") loc) ->
    Just . TypeDo $ DoType (Fix2 . ExpTerm $ TermData (DataInt $ IntLit ('0' :| "") loc)) loc
  TreeParens (TreeVar (Var ('D' :| "o") _) : trees) loc ->
    Just . TypeDo $ case trees of
      [] -> DoTypeZero loc
      [exp] -> DoType (Fix2 $ parseExp exp) loc
      _ -> DoTypeMany loc
  TreeParens (TreeVar (Var ('S' :| "et") _) : trees) loc ->
    Just . TypeSet $ case map parseExp trees of
      [] -> SetZero loc
      [_] -> SetOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 a
              b : trees -> Fix2 . ExpType . TypeSet $ Set (Fix2 a) (go b trees) loc
         in Set (Fix2 a) (go b trees) loc
  TreeParens (TreeVar (Var ('F' :| "or") _) : trees) loc ->
    Just . TypeFor $ case trees of
      [] -> ForZero loc
      [_] -> ForOne loc
      a : b : trees ->
        let go a = \case
              [] -> Fix2 $ parseExp a
              b : trees -> Fix2 . ExpType . TypeFor $ For (parseVar a) (go b trees) loc
         in For (parseVar a) (go b trees) loc
  TreeVar (Var ('T' :| "ype") loc) ->
    Just . TypeKind $ Kind (Fix2 . ExpTerm $ TermData (DataInt $ IntLit ('0' :| "") loc)) loc
  TreeParens (TreeVar (Var ('T' :| "ype") _) : trees) loc ->
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
  TreeStr str -> Just $ DataStr str
  TreeChar char -> Just $ DataChar char
  TreeFloat float -> Just $ DataFloat float
  TreeInt int -> Just $ DataInt int
  TreeVar var -> Just $ DataVar var
  _ -> Nothing

parseVar :: Tree -> Var
parseVar = \case
  TreeVar var -> var
  tree -> VarNone (locate tree)