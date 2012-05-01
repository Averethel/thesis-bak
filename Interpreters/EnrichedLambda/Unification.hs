{-# LANGUAGE 
  FlexibleContexts
  #-}

module Interpreters.EnrichedLambda.Unification where
  import Interpreters.EnrichedLambda.Errors
  import Interpreters.EnrichedLambda.PrettyPrint
  import Interpreters.EnrichedLambda.State
  import Interpreters.EnrichedLambda.Syntax
  
  import Control.Monad.Error
  import Control.Monad.State
  import Data.List
  import Data.Maybe
  
  can_cmp :: Type -> Bool
  can_cmp (T_Var _)      = True
  can_cmp T_Int          = True
  can_cmp T_Bool         = True
  can_cmp T_Unit         = True
  can_cmp (T_Pair t1 t2) = can_cmp t1 && can_cmp t2
  can_cmp (T_List t)     = can_cmp t
  can_cmp (T_Ref t)      = can_cmp t
  can_cmp _              = False
  
  subst_type :: String -> Type -> Type -> Type
  subst_type v tp (T_Var v')
    | v == v'                     = tp
    | otherwise                   = T_Var v'
  subst_type v tp (T_List t)      = T_List (subst_type v tp t)
  subst_type v tp (T_Ref t)       = T_Ref (subst_type v tp t)
  subst_type v tp (T_Pair t1 t2)  = T_Pair (subst_type v tp t1) (subst_type v tp t2)
  subst_type v tp (T_Arrow t1 t2) = T_Arrow (subst_type v tp t1) (subst_type v tp t2)
  subst_type _ _  t               = t
  
  unify :: (MonadError String m, MonadState InterpreterState m) => m Subst
  unify = do
    c <- get_constraint
    case c of
      Nothing     -> get_subst
      Just (a, b) -> do
        f <- get_subst
        case (f a, f b) of
          (T_Var x, tp) -> do
            compose_subst (subst_type x tp)
            unify
          (tp, T_Var x) -> do
            compose_subst (subst_type x tp)
            unify
          (T_Ref t1, T_Ref t2) -> do
            add_constraint t1 t2
            unify
          (T_List t1, T_List t2) -> do
            add_constraint t1 t2
            unify
          (T_Arrow t1 t2, T_Arrow t3 t4) -> do
            add_constraint t1 t3
            add_constraint t2 t4
            unify
          (T_Pair t1 t2, T_Pair t3 t4) -> do
            add_constraint t1 t3
            add_constraint t2 t4
            unify
          (a, b) -> 
            if   a == b
            then unify
            else throwError $ cannot_unify a b
  
  check_compare :: (MonadError String m, MonadState InterpreterState m) => m ()
  check_compare = do
    f  <- get_subst
    cs <- get_simple_constraints
    if   all (can_cmp . f) cs
    then return ()
    else throwError $ cannot_compare $ fromJust $ find (not . can_cmp . f) cs
