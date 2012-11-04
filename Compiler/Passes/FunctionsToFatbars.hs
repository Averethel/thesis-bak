{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Passes.FunctionsToFatbars (functions_to_fatbars) where 
  import Languages.MiniML.Syntax

  import Control.Monad.State

  type NamerState = Integer

  empty_state :: NamerState
  empty_state = 0

  new_var :: MonadState NamerState m => m String
  new_var = do
    s <- get
    put $ s + 1
    return $ "_arg_" ++ show s

  functions_to_fatbars_binding :: MonadState NamerState m => Binding -> m Binding
  functions_to_fatbars_binding (p, e) = do
    e' <- functions_to_fatbars_expression e
    return (p, e')

  functions_to_fatbars_fun_binding :: MonadState NamerState m => FunBinding -> m Expr
  functions_to_fatbars_fun_binding (p, e, E_Const C_True) = do
    e' <- functions_to_fatbars_expression e
    return $ E_Function [(p, e', E_Const C_True)]
  functions_to_fatbars_fun_binding (p, e, g)              = do
    e' <- functions_to_fatbars_expression e
    g' <- functions_to_fatbars_expression g
    return $ E_Function [(p, E_ITE g' e' E_MatchFailure, E_Const C_True)]

  functions_to_fatbars_let_rec_binding :: MonadState NamerState m => LetRecBinding -> m LetRecBinding
  functions_to_fatbars_let_rec_binding (v, e) = do
    e' <- functions_to_fatbars_expression e
    return (v, e')

  functions_to_fatbars_expression :: MonadState NamerState m => Expr -> m Expr
  functions_to_fatbars_expression (E_Apply e1 e2)  = do
    e1' <- functions_to_fatbars_expression e1
    e2' <- functions_to_fatbars_expression e2
    return $ E_Apply e1' e2'
  functions_to_fatbars_expression (E_Cons e1 e2)   = do
    e1' <- functions_to_fatbars_expression e1
    e2' <- functions_to_fatbars_expression e2
    return $ E_Cons e1' e2'
  functions_to_fatbars_expression (E_Tuple es)     = do
    es' <- mapM functions_to_fatbars_expression es
    return $ E_Tuple es'
  functions_to_fatbars_expression (E_And e1 e2)    = do
    e1' <- functions_to_fatbars_expression e1
    e2' <- functions_to_fatbars_expression e2
    return $ E_And e1' e2'
  functions_to_fatbars_expression (E_Or e1 e2)     = do
    e1' <- functions_to_fatbars_expression e1
    e2' <- functions_to_fatbars_expression e2
    return $ E_Or e1' e2'
  functions_to_fatbars_expression (E_ITE e1 e2 e3) = do
    e1' <- functions_to_fatbars_expression e1
    e2' <- functions_to_fatbars_expression e2
    e3' <- functions_to_fatbars_expression e3
    return $ E_ITE e1' e2' e3'
  functions_to_fatbars_expression (E_Seq e1 e2)    = do
    e1' <- functions_to_fatbars_expression e1
    e2' <- functions_to_fatbars_expression e2
    return $ E_Seq e1' e2' 
  functions_to_fatbars_expression (E_Function fbs)  = do
    fbs' <- mapM functions_to_fatbars_fun_binding fbs
    v    <- new_var
    let fbs'' = map (\x -> E_Apply x $ E_Val v) fbs'
    return $ E_Function 
             [(P_Val v, foldr E_FatBar E_MatchFailure fbs'', E_Const C_True)]
  functions_to_fatbars_expression (E_Let lbs e)     = do
    lbs' <- mapM functions_to_fatbars_binding lbs
    e'   <- functions_to_fatbars_expression e
    return $ E_Let lbs' e'
  functions_to_fatbars_expression (E_LetRec lrbs e) = do  
    lrbs' <- mapM functions_to_fatbars_let_rec_binding lrbs
    e'    <- functions_to_fatbars_expression e
    return $ E_LetRec lrbs' e'
  functions_to_fatbars_expression e                 =
    return e

  functions_to_fatbars_definition :: MonadState NamerState m => Definition -> m Definition
  functions_to_fatbars_definition (D_Let lbs)     = do
    lbs' <- mapM functions_to_fatbars_binding lbs
    return $ D_Let lbs'
  functions_to_fatbars_definition (D_LetRec lrbs) = do
    lrbs' <- mapM functions_to_fatbars_let_rec_binding lrbs
    return $ D_LetRec lrbs'

  functions_to_fatbars_instruction :: MonadState NamerState m => Instruction -> m Instruction
  functions_to_fatbars_instruction (IDF df) = do
    df' <- functions_to_fatbars_definition df
    return $ IDF df'
  functions_to_fatbars_instruction (IEX ex) = do
    ex' <- functions_to_fatbars_expression ex
    return $ IEX ex'

  functions_to_fatbars :: Program -> Program
  functions_to_fatbars p = 
    fst $ runState (mapM functions_to_fatbars_instruction p) empty_state
