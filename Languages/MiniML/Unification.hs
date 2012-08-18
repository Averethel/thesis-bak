{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Unification (unify, check_compare) where
  import Languages.MiniML.Errors
  import Languages.MiniML.State
  import Languages.MiniML.Syntax
  import Languages.MiniML.PrettyPrint

  import Control.Monad.Error
  import Control.Monad.State
  import Data.List
  import Data.Maybe
  
  can_cmp :: TypeExpr -> Bool
  can_cmp (TE_Var _)        = True
  can_cmp (TE_Arrow _ _)    = False
  can_cmp (TE_Tuple tes)    = all can_cmp tes
  can_cmp (TE_Constr tes _) = all can_cmp tes
  
  subst_type :: String -> TypeExpr -> TypeExpr -> TypeExpr
  subst_type x te (TE_Var tv)
    | x == tv                   = te
    | otherwise                 = TE_Var tv
  subst_type x te (TE_Arrow te1 te2) = TE_Arrow (subst_type x te te1) (subst_type x te te2)
  subst_type x te (TE_Tuple tes)     = TE_Tuple $ map (subst_type x te) tes
  subst_type x te (TE_Constr tes tc) = TE_Constr (map (subst_type x te) tes) tc
  
  add_constraints :: (MonadError String m, MonadState InterpreterState m) => [(TypeExpr, TypeExpr)] ->  m ()
  add_constraints []            = return ()
  add_constraints ((t1, t2):ts) = do
    add_constraint t1 t2
    add_constraints ts

  unify :: (MonadError String m, MonadState InterpreterState m) => m Subst
  unify = do
    c <- get_constraint
    case c of
      Nothing     -> get_subst
      Just (a, b) -> do
        f <- get_subst
        case (f a, f b) of
          (TE_Var x, tp) -> do
            compose_subst (subst_type x tp)
            unify
          (tp, TE_Var x) -> do
            compose_subst (subst_type x tp)
            unify
          (TE_Arrow t1 t2, TE_Arrow t3 t4) -> do
            add_constraint t1 t3
            add_constraint t2 t4
            unify
          (TE_Tuple ts1, TE_Tuple ts2) ->
            if   length ts1 == length ts2
            then do
              add_constraints $ zip ts1 ts2
              unify
            else throwError $ cannot_unify (TE_Tuple ts1, TE_Tuple ts2)
          (TE_Constr ts1 tc1, TE_Constr ts2 tc2) -> do
            if   length ts1 == length ts2 && tc1 == tc2
            then do
              add_constraints $ zip ts1 ts2
              unify
            else throwError $ cannot_unify (TE_Constr ts1 tc1, TE_Constr ts2 tc2)
          (a, b) -> 
            if   a == b
            then unify
            else throwError $ cannot_unify (a, b)
  
  check_compare :: (MonadError String m, MonadState InterpreterState m) => m ()
  check_compare = do
    f  <- get_subst
    cs <- get_simple_constraints
    if   all (can_cmp . f) cs
    then return ()
    else throwError $ cannot_compare $ fromJust $ find (not . can_cmp . f) cs