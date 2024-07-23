module Parse (parseAst) where

import Ast
import Data.List.NonEmpty (NonEmpty (..))
import Locate (Locate (..))
import Tree

parseAst :: [Tree] -> Ast
parseAst = map parseItem

parseItem :: Tree -> Item
parseItem = \case
  TreeParens (TreeVar (Var ('<' :| "-") _) : trees) loc ->
    ItemGlobal $ case trees of
      [] -> GlobalZero loc
      [_] -> GlobalOne loc
      [name, exp] -> Global (parseVar name) (parseTyped exp) loc
      _ -> GlobalMore loc
  TreeParens (TreeVar (Var ('=' :| "") _) : trees) loc ->
    ItemLocal $ case trees of
      [] -> LocalZero loc
      [_] -> LocalOne loc
      [name, exp] -> Local (parseVar name) (parseTyped exp) loc
      _ -> LocalMore loc
  tree -> ItemNone $ locate tree

parseTyped :: Tree -> Typed
parseTyped = \case
  TreeParens (TreeVar (Var (':' :| "") _) : trees) loc ->
    TypedAnn $ case trees of
      [] -> AnnZero loc
      [_] -> AnnOne loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> go (Ann (TypedAnn a) (parseExp b) loc) trees
         in go (Ann (parseTyped a) (parseExp b) loc) trees
  TreeParens (TreeVar (Var ('A' :| "") _) : trees) loc ->
    TypedFor $ case trees of
      [] -> ForZero loc
      [_] -> ForOne loc
      [_, _] -> ForTwo loc
      a : b : c : trees ->
        let go a = \case
              [] -> parseTyped a
              [_] -> TypedFor $ ForOne loc
              b : c : trees -> TypedFor $ For (parseVar a) (parseExp b) (go c trees) loc
         in For (parseVar a) (parseExp b) (go c trees) loc
  TreeVar (Var ('T' :| "ype") loc) -> TypedType $ Type loc
  tree -> inferred $ parseExp tree

parseExp :: Tree -> Exp
parseExp = \case
  TreeParens (TreeVar (Var ('S' :| "et") _) : trees) loc ->
    TypeSet $ case map parseTyped trees of
      [] -> SetZero loc
      [_] -> SetOne loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> inferred . TypeSet $ Set a (go b trees) loc
         in Set a (go b trees) loc
  TreeParens (TreeVar (Var ('=' :| ">") _) : trees) loc ->
    TypeFun $ case map parseTyped trees of
      [] -> FunTypeZero loc
      [_] -> FunTypeOne loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> inferred . TypeFun $ FunType a (go b trees) loc
         in FunType a (go b trees) loc
  TreeParens (TreeVar (Var ('D' :| "o") _) : trees) loc ->
    TypeDo $ case trees of
      [] -> DoTypeZero loc
      [exp] -> DoType (parseTyped exp) loc
      _ -> DoTypeMany loc
  TreeVar (Var ('D' :| "o") loc) ->
    TypeDo $ DoType (inferred . ExpNil $ NilLit loc) loc
  TreeVar (Var ('F' :| "loat") loc) -> TypeFloat $ FloatType loc
  TreeVar (Var ('I' :| "nt") loc) -> TypeInt $ IntType loc
  TreeVar (Var ('N' :| "at") loc) -> TypeNat $ NatType loc
  TreeVar (Var ('C' :| "har") loc) -> TypeChar $ CharType loc
  TreeVar (Var ('#' :| "") loc) -> TypeInfer $ Infer 0 loc
  TreeParens (TreeVar (Var ('-' :| ">") _) : trees) loc ->
    TermFun $ case map parseTyped trees of
      [] -> FunZero loc
      [_] -> FunOne loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> inferred . TermFun $ Fun a (go b trees) loc
         in Fun a (go b trees) loc
  TreeParens (TreeVar (Var ('?' :| "") _) : trees) loc ->
    TermCase $ case map parseTyped trees of
      [] -> CaseZero loc
      [_] -> CaseOne loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> inferred . TermCase $ Case a (go b trees) loc
         in Case a (go b trees) loc
  TreeParens (TreeVar (Var ('d' :| "o") _) : trees) loc ->
    TermDo $ case trees of
      [] -> DoZero loc
      [_] -> DoOne loc
      [_, _] -> DoTwo loc
      a : b : c : trees ->
        let go a = \case
              [] -> parseTyped a
              [_] -> inferred . TermDo $ DoOne loc
              b : c : trees -> inferred . TermDo $ Do (parseTyped a) (parseTyped b) (go c trees) loc
         in Do (parseTyped a) (parseTyped b) (go c trees) loc
  TreeParens (TreeVar (Var ('=' :| "") _) : trees) loc ->
    TermLet $ case trees of
      [] -> LetZero loc
      [_] -> LetOne loc
      [_, _] -> LetTwo loc
      a : b : c : trees ->
        let go a = \case
              [] -> parseTyped a
              [_] -> inferred . TermLet $ LetOne loc
              b : c : trees -> inferred . TermLet $ Let (parseTyped a) (parseTyped b) (go c trees) loc
         in Let (parseTyped a) (parseTyped b) (go c trees) loc
  TreeParens trees loc ->
    TermApp $ case map parseTyped trees of
      [] -> AppZero loc
      [_] -> AppOne loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> go (App (inferred $ TermApp a) b loc) trees
         in go (App a b loc) trees
  TreeBrackets trees loc ->
    ExpCons $ case map parseTyped trees of
      [] -> ConsZero loc
      [_] -> ConsOne loc
      a : b : trees ->
        let go a = \case
              [] -> a
              b : trees -> inferred . ExpCons $ Cons a (go b trees) loc
         in Cons a (go b trees) loc
  TreeFloat float -> ExpFloat float
  TreeInt int -> ExpInt int
  TreeNat nat -> ExpNat nat
  TreeChar char -> ExpChar char
  TreeVar (Var (':' :| sym) loc) -> ExpSym $ SymLit sym loc
  TreeVar (Var ('n' :| "il") loc) -> ExpNil $ NilLit loc
  TreeVar var -> ExpVar $ parseVar (TreeVar var)
  tree -> ExpNone $ locate tree

parseVar :: Tree -> Var
parseVar = \case
  TreeVar var -> var
  tree -> VarNone (locate tree)

inferred :: Exp -> Typed
inferred exp =
  let loc = locate exp
   in -- (: Type #0 exp)
      TypedAnn $ Ann (TypedAnn $ Ann (TypedType $ Type loc) (TypeInfer $ Infer 0 loc) loc) exp loc
