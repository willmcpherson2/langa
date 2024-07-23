module Check (checkAst) where

import Ast
import Control.Monad.State
import Data.Generics.Uniplate.Data

checkAst :: Ast -> Ast
checkAst = initInfers

initInfers :: Ast -> Ast
initInfers ast = evalState (transformBiM updateInfer ast) 1
  where
    updateInfer :: Infer -> State Int Infer
    updateInfer (Infer _ loc) = do
      n <- get
      put $ n + 1
      pure $ Infer n loc
