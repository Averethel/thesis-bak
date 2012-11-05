{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Unification (unify, checkCompare) where
  import Utils.Errors
  
  import Languages.MiniML.State
  import Languages.MiniML.Syntax
  import Languages.MiniML.PrettyPrint

  import Control.Monad.Error
  import Control.Monad.State
  import Data.List
  import Data.Maybe
  
  canCmp :: TypeExpr -> Bool
  canCmp (TE_Var _)        = True
  canCmp (TE_Arrow _ _)    = False
  canCmp (TE_Tuple tes)    = all canCmp tes
  canCmp (TE_Constr tes _) = all canCmp tes
  
  substType :: String -> TypeExpr -> TypeExpr -> TypeExpr
  substType x te (TE_Var tv)
    | x == tv                   = te
    | otherwise                 = TE_Var tv
  substType x te (TE_Arrow te1 te2) = TE_Arrow (substType x te te1) (substType x te te2)
  substType x te (TE_Tuple tes)     = TE_Tuple $ map (substType x te) tes
  substType x te (TE_Constr tes tc) = TE_Constr (map (substType x te) tes) tc
  
  addConstraints :: (MonadError String m, MonadState InterpreterState m) => [(TypeExpr, TypeExpr)] ->  m ()
  addConstraints []            = return ()
  addConstraints ((t1, t2):ts) = do
    addConstraint t1 t2
    addConstraints ts

  unify :: (MonadError String m, MonadState InterpreterState m) => m Subst
  unify = do
    c <- getConstraint
    case c of
      Nothing     -> getSubst
      Just (a, b) -> do
        f <- getSubst
        case (f a, f b) of
          (TE_Var x, tp) -> do
            composeSubst (substType x tp)
            unify
          (tp, TE_Var x) -> do
            composeSubst (substType x tp)
            unify
          (TE_Arrow t1 t2, TE_Arrow t3 t4) -> do
            addConstraint t1 t3
            addConstraint t2 t4
            unify
          (TE_Tuple ts1, TE_Tuple ts2) ->
            if   length ts1 == length ts2
            then do
              addConstraints $ zip ts1 ts2
              unify
            else throwError $ cannotUnify (TE_Tuple ts1, TE_Tuple ts2)
          (TE_Constr ts1 tc1, TE_Constr ts2 tc2) -> do
            if   length ts1 == length ts2 && tc1 == tc2
            then do
              addConstraints $ zip ts1 ts2
              unify
            else throwError $ cannotUnify (TE_Constr ts1 tc1, TE_Constr ts2 tc2)
          (a, b) -> 
            if   a == b
            then unify
            else throwError $ cannotUnify (a, b)
  
  checkCompare :: (MonadError String m, MonadState InterpreterState m) => m ()
  checkCompare = do
    f  <- getSubst
    cs <- getSimpleConstraints
    if   all (canCmp . f) cs
    then return ()
    else throwError $ cannotCompare $ fromJust $ find (not . canCmp . f) cs
