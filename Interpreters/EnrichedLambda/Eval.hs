{-# LANGUAGE
  FlexibleContexts
  #-}

module Interpreters.EnrichedLambda.Eval where
  import Interpreters.EnrichedLambda.Errors
  import Interpreters.EnrichedLambda.Syntax
  import Interpreters.EnrichedLambda.State
  
  import Control.Monad.Error
  import Control.Monad.State
  
  is_value :: Expr -> Bool
  is_value (E_Const _)      = True
  is_value (E_Location _)   = True
  is_value (E_Function v e) = True
  is_value (E_Cons e1 e2)   = is_value e1 && is_value e2
  is_value (E_Pair e1 e2)   = is_value e1 && is_value e2
  is_value _                = False
  
  eval_step_expr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  eval_step_expr (E_Var s) = do
    env <- get_eval_env
    case env s of
      Nothing -> throwError $ unbound_variable s
      Just e  -> return e
  eval_step_expr (E_Not (E_Const C_True)) = return $ E_Const C_False
  eval_step_expr (E_Not (E_Const C_False)) = return $ E_Const C_True
  eval_step_expr (E_Not e)
    | not . is_value $ e = do
      e' <- eval_step_expr e
      return $ E_Not e'
  eval_step_expr (E_Ref e)
    | is_value e = do
      l <- alloc_addr e
      return $ E_Location l
    | otherwise  = do
      e' <- eval_step_expr e
      return $ E_Ref e'
  eval_step_expr (E_Deref (E_Location l)) = lookup_mem l
  eval_step_expr (E_Deref e)
    | not . is_value $ e = do
      e' <- eval_step_expr e
      return $ E_Deref e'
  eval_step_expr (E_Eq e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Eq e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Eq e1' e2
  eval_step_expr (E_Eq (E_Const c1) (E_Const c2))
    | c1 == c2  = return $ E_Const $ C_True
    | otherwise = return $ E_Const $ C_False
  eval_step_expr (E_Eq e1@(E_Location l1) e2@(E_Location l2)) = return $ E_Eq (E_Deref e1) (E_Deref e2)
  eval_step_expr (E_Eq (E_Pair e1 e2) (E_Pair e3 e4)) = return $ E_ITE (E_Eq e1 e3) (E_Eq e2 e4) (E_Const C_False)
  eval_step_expr (E_Eq (E_Cons e1 e2) (E_Cons e3 e4)) = return $ E_ITE (E_Eq e1 e3) (E_Eq e2 e4) (E_Const C_False)
  eval_step_expr (E_Eq (E_Cons _ _) (E_Const C_Nil))  = return $ E_Const $ C_False
  eval_step_expr (E_Eq (E_Const C_Nil) (E_Cons _ _))  = return $ E_Const $ C_False
  eval_step_expr (E_Plus e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Plus e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Plus e1' e2
  eval_step_expr (E_Plus (E_Const (C_Int n1)) (E_Const (C_Int n2))) = return $ E_Const $ C_Int $ n1 + n2
  eval_step_expr (E_Minus e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Minus e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Minus e1' e2
  eval_step_expr (E_Minus (E_Const (C_Int n1)) (E_Const (C_Int n2))) = return $ E_Const $ C_Int $ n1 - n2
  eval_step_expr (E_Div e1 e2)
    | is_value e1 && (not . is_value $ e2 )= do
      e2' <- eval_step_expr e2
      return $ E_Div e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Div e1' e2
  eval_step_expr (E_Div (E_Const (C_Int n1)) (E_Const (C_Int n2)))
    | n2 /= 0   = return $ E_Const $ C_Int $ n1 `div` n2
    | otherwise = throwError $ division_by_0
  eval_step_expr (E_Mult e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Mult e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Mult e1' e2
  eval_step_expr (E_Mult (E_Const (C_Int n1)) (E_Const (C_Int n2))) = return $ E_Const $ C_Int $ n1 * n2
  eval_step_expr (E_Assign e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Assign e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Assign e1' e2
  eval_step_expr (E_Assign (E_Location a) v) = do
    update_mem a v
    return $ E_Const $ C_Unit
  eval_step_expr (E_Head e)
    | not . is_value $ e = do
      e' <- eval_step_expr e
      return $ E_Head e'
  eval_step_expr (E_Head (E_Cons e _)) = return e
  eval_step_expr (E_Tail e)
    | not . is_value $ e = do
      e' <- eval_step_expr e
      return $ E_Tail e'
  eval_step_expr (E_Tail (E_Cons _ e)) = return e
  eval_step_expr (E_Cons e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Cons e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Cons e1' e2
  eval_step_expr (E_ITE e1 e2 e3)
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_ITE e1' e2 e3
  eval_step_expr (E_ITE (E_Const C_True) e2 _) = return e2
  eval_step_expr (E_ITE (E_Const C_False) _ e) = return e
  eval_step_expr (E_Seq e1 e2)
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Seq e1' e2
  eval_step_expr (E_Seq (E_Const C_Unit) e2) = return e2
  eval_step_expr (E_Fst e)
    | not . is_value $ e = do
      e' <- eval_step_expr e
      return $ E_Fst e'
  eval_step_expr (E_Fst (E_Pair e _)) = return e
  eval_step_expr (E_Snd e)
    | not . is_value $ e = do
      e' <- eval_step_expr e
      return $ E_Snd e'
  eval_step_expr (E_Snd (E_Pair _ e)) = return e
  eval_step_expr (E_Pair e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Pair e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Pair e1' e2
  eval_step_expr (E_Let vn e1 e2)
    | is_value e1 = do
      add_to_eval_env vn e1
      return e2
    | otherwise = do
      e1' <- eval_step_expr e1
      return (E_Let vn e1' e2)
  eval_step_expr (E_Letrec vn e1 e2) = do
    add_to_eval_env vn e1
    return e2
  eval_step_expr (E_Apply e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Apply e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Apply e1' e2
  eval_step_expr (E_Apply (E_Function vn e1) e2) = do
    add_to_eval_env vn e2
    return e1
  eval_step_expr E_MatchFailure = throwError match_failure
  
  eval_expr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  eval_expr e
    | is_value e = return e
    | otherwise  = do 
      e' <- eval_step_expr e
      eval_expr e'
