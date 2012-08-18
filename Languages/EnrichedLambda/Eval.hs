{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.EnrichedLambda.Eval (eval_expr, eval_definition, eval_program) where
  import Languages.EnrichedLambda.Errors
  import Languages.EnrichedLambda.Syntax
  import Languages.EnrichedLambda.State
  import Languages.EnrichedLambda.PrettyPrint
  
  import Control.Monad.Error
  import Control.Monad.State
  
  is_value :: Expr -> Bool
  is_value (E_Const _)             = True
  is_value (E_UPrim _)             = True
  is_value (E_BPrim _)             = True
  is_value (E_Location _)          = True
  is_value (E_Function v e)        = True
  is_value (E_Apply (E_BPrim _) v) = is_value v
  is_value (E_Cons e1 e2)          = is_value e1 && is_value e2
  is_value (E_Pair e1 e2)          = is_value e1 && is_value e2
  is_value _                       = False
  
  eval_unary_prim :: (MonadError String m, MonadState InterpreterState m) => UnaryPrim -> Expr -> m Expr
  eval_unary_prim U_Not (E_Const C_True) = return $ E_Const C_False
  eval_unary_prim U_Not (E_Const C_False) = return $ E_Const C_True
  eval_unary_prim U_Ref e = do
    l <- store e
    return $ E_Location l
  eval_unary_prim U_Deref (E_Location l) = load l
  eval_unary_prim U_Fst (E_Pair e _) = return e
  eval_unary_prim U_Snd (E_Pair _ e) = return e
  eval_unary_prim U_Head (E_Cons e _) = return e
  eval_unary_prim U_Tail (E_Cons _ e) = return e
  eval_unary_prim U_Empty (E_Cons _ _) = return $ E_Const C_False
  eval_unary_prim U_Empty (E_Const C_Nil) = return $ E_Const C_True
  
  eval_binary_prim :: (MonadError String m, MonadState InterpreterState m) => BinaryPrim -> Expr -> Expr -> m Expr
  eval_binary_prim B_Eq (E_Const c1) (E_Const c2)
    | c1 == c2  = return $ E_Const $ C_True
    | otherwise = return $ E_Const $ C_False
  eval_binary_prim B_Eq e1@(E_Location l1) e2@(E_Location l2) = return $ E_Apply (E_Apply (E_BPrim B_Eq) (E_Apply (E_UPrim U_Deref) e1)) (E_Apply (E_UPrim U_Deref) e2)
  eval_binary_prim B_Eq (E_Pair e1 e2) (E_Pair e3 e4) = return $ E_ITE (E_Apply (E_Apply (E_BPrim B_Eq) e1) e3) (E_Apply (E_Apply (E_BPrim B_Eq) e2) e4) (E_Const C_False)
  eval_binary_prim B_Eq (E_Cons e1 e2) (E_Cons e3 e4) = return $ E_ITE (E_Apply (E_Apply (E_BPrim B_Eq) e1) e3) (E_Apply (E_Apply (E_BPrim B_Eq) e2) e4) (E_Const C_False)
  eval_binary_prim B_Eq (E_Cons _ _) (E_Const C_Nil)  = return $ E_Const $ C_False
  eval_binary_prim B_Eq (E_Const C_Nil) (E_Cons _ _)  = return $ E_Const $ C_False
  eval_binary_prim B_Plus (E_Const (C_Int n1)) (E_Const (C_Int n2)) = return $ E_Const $ C_Int $ n1 + n2
  eval_binary_prim B_Minus (E_Const (C_Int n1)) (E_Const (C_Int n2)) = return $ E_Const $ C_Int $ n1 - n2
  eval_binary_prim B_Div (E_Const (C_Int n1)) (E_Const (C_Int n2))
    | n2 /= 0   = return $ E_Const $ C_Int $ n1 `div` n2
    | otherwise = throwError $ division_by_0
  eval_binary_prim B_Mult (E_Const (C_Int n1)) (E_Const (C_Int n2)) = return $ E_Const $ C_Int $ n1 * n2
  eval_binary_prim B_Assign (E_Location a) v = do
    store_at a v
    return $ E_Const $ C_Unit
  
  recfun :: (MonadState InterpreterState m) => [(String, Expr)] -> m ()
  recfun []          = return ()
  recfun ((v, e):bs) = do
    extend_eval_env v e
    recfun bs

  eval_step_expr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  eval_step_expr (E_Val s) = do
    env <- get_eval_env
    case env s of
      Nothing -> throwError $ unbound_variable s
      Just e  -> return e
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
  eval_step_expr (E_Pair e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Pair e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Pair e1' e2
  eval_step_expr (E_Let vn e1 e2)
    | is_value e1 = do
      extend_eval_env vn e1
      return e2
    | otherwise = do
      e1' <- eval_step_expr e1
      return (E_Let vn e1' e2)
  eval_step_expr (E_LetRec lrbs e2) = do
    recfun lrbs
    return e2
  eval_step_expr (E_Apply e1 e2)
    | is_value e1 && (not . is_value $ e2) = do
      e2' <- eval_step_expr e2
      return $ E_Apply e1 e2'
    | not . is_value $ e1 = do
      e1' <- eval_step_expr e1
      return $ E_Apply e1' e2
  eval_step_expr (E_Apply (E_Function vn e1) e2) = do
    extend_eval_env vn e2
    return e1
  eval_step_expr (E_Apply (E_UPrim up) e) = eval_unary_prim up e
  eval_step_expr (E_Apply (E_Apply (E_BPrim bp) e1) e2) = eval_binary_prim bp e1 e2
  eval_step_expr E_MatchFailure = throwError match_failure
  
  eval_expr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  eval_expr e
    | is_value e = return e
    | otherwise  = do 
      e' <- eval_step_expr e
      eval_expr e'

  eval_definition :: (MonadError String m, MonadState InterpreterState m) => Definition -> m ()
  eval_definition (D_Let v e)    = do
    e' <- eval_expr e
    extend_eval_env v e'
  eval_definition (D_LetRec lrbs) = recfun lrbs

  eval_instruction :: (MonadError String m, MonadState InterpreterState m) => Instruction -> m ()
  eval_instruction (IDF df) = eval_definition df
  eval_instruction (IEX ex) = do
    e <- eval_expr ex
    extend_eval_env "it" e

  eval_program :: (MonadError String m, MonadState InterpreterState m) => Program -> m Expr
  eval_program []     = do
    env <- get_eval_env
    case env "it" of
      Nothing -> return Null
      Just ex -> return ex
  eval_program (i:is) = do
    eval_instruction i
    eval_program is
