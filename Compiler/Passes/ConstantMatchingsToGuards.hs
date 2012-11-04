{-# LANGUAGE 
  FlexibleContexts
  #-}

module Compiler.Passes.ConstantMatchingsToGuards (program_to_guarded_patterns) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  -- Assumptions:
  -- 1. Wildcards were translated to variables
  -- 2. Tuples were folded to pairs

  type NamerState = Integer

  empty_state :: NamerState
  empty_state = 0

  new_var :: MonadState NamerState m => m String
  new_var = do
    s <- get
    put $ s + 1
    return $ "_pat_" ++ show s

  pattern_to_guarded_patterns :: MonadState NamerState m => Pattern -> m (Pattern, Expr)
  pattern_to_guarded_patterns (P_Const (C_Int n)) = do
    v <- new_var
    return (P_Val v, 
            E_Apply (E_Apply (E_BPrim B_Eq) $ E_Val v) $ E_Const $ C_Int n)
  pattern_to_guarded_patterns (P_Tuple [a, b])    = do
    (a', e1) <- pattern_to_guarded_patterns a
    (b', e2) <- pattern_to_guarded_patterns b
    let p' = P_Tuple [a', b']
    return $ case (e1, e2) of
      (E_Const C_True, E_Const C_True) -> (p', E_Const C_True)
      (_,              E_Const C_True) -> (p', e1)
      (E_Const C_True, _)              -> (p', e2)
      (_,              _)              -> (p', E_And e1 e2)
  pattern_to_guarded_patterns (P_Cons p1 p2)      = do
    (p1', e1) <- pattern_to_guarded_patterns p1
    (p2', e2) <- pattern_to_guarded_patterns p2
    let p' = P_Cons p1' p2'
    return $ case (e1, e2) of
      (E_Const C_True, E_Const C_True) -> (p', E_Const C_True)
      (_,              E_Const C_True) -> (p', e1)
      (E_Const C_True, _)              -> (p', e2)
      (_,              _)              -> (p', E_And e1 e2)
  pattern_to_guarded_patterns p                   =
    return (p, E_Const C_True)

  fun_binding_to_guarded_patterns :: MonadState NamerState m => FunBinding -> m FunBinding
  fun_binding_to_guarded_patterns (p, e, g) = do
    (p', g') <- pattern_to_guarded_patterns p
    return $ case (g, g') of
      (E_Const C_True, E_Const C_True) -> (p', e, E_Const C_True)
      (_,              E_Const C_True) -> (p', e, g)
      (E_Const C_True, _)              -> (p', e, g')
      (_,              _)              -> (p', e, E_And g g')

  letrec_binding_to_guarded_patterns :: MonadState NamerState m => LetRecBinding -> m LetRecBinding
  letrec_binding_to_guarded_patterns (v, e) = do
    e' <- expression_to_guarded_patterns e
    return (v, e')

  binding_to_guarded_patterns :: MonadState NamerState m => Binding -> m Binding
  binding_to_guarded_patterns (p, e) = do
    e' <- expression_to_guarded_patterns e
    return (p, e')

  expression_to_guarded_patterns :: MonadState NamerState m => Expr -> m Expr
  expression_to_guarded_patterns (E_Apply e1 e2)   = do
    e1' <- expression_to_guarded_patterns e1
    e2' <- expression_to_guarded_patterns e2
    return $ E_Apply e1' e2'
  expression_to_guarded_patterns (E_Cons e1 e2)    = do
    e1' <- expression_to_guarded_patterns e1
    e2' <- expression_to_guarded_patterns e2
    return $ E_Cons e1' e2'
  expression_to_guarded_patterns (E_Tuple es)      = do
    es' <- mapM expression_to_guarded_patterns es
    return $ E_Tuple es'
  expression_to_guarded_patterns (E_And e1 e2)     = do
    e1' <- expression_to_guarded_patterns e1
    e2' <- expression_to_guarded_patterns e2
    return $ E_And e1' e2'
  expression_to_guarded_patterns (E_Or e1 e2)      = do
    e1' <- expression_to_guarded_patterns e1
    e2' <- expression_to_guarded_patterns e2
    return $ E_Or e1' e2'
  expression_to_guarded_patterns (E_ITE e1 e2 e3)  = do
    e1' <- expression_to_guarded_patterns e1
    e2' <- expression_to_guarded_patterns e2
    e3' <- expression_to_guarded_patterns e3
    return $ E_ITE e1' e2' e3'
  expression_to_guarded_patterns (E_Seq e1 e2)     = do
    e1' <- expression_to_guarded_patterns e1
    e2' <- expression_to_guarded_patterns e2
    return $ E_Seq e1' e2'
  expression_to_guarded_patterns (E_Function fbs)  = do
    fbs' <- mapM fun_binding_to_guarded_patterns fbs
    return $ E_Function fbs'
  expression_to_guarded_patterns (E_Let bs e)      = do
    bs' <- mapM binding_to_guarded_patterns bs
    e'  <- expression_to_guarded_patterns e
    return $ E_Let bs' e'
  expression_to_guarded_patterns (E_LetRec lrbs e) = do
    lrbs' <- mapM letrec_binding_to_guarded_patterns lrbs
    e'    <- expression_to_guarded_patterns e
    return $ E_LetRec lrbs' e'
  expression_to_guarded_patterns e                 =
    return e

  definition_to_guarded_patterns :: MonadState NamerState m => Definition -> m Definition
  definition_to_guarded_patterns (D_Let bs)      = do
    bs' <- mapM binding_to_guarded_patterns bs
    return $ D_Let bs'
  definition_to_guarded_patterns (D_LetRec lrbs) = do
    lrbs' <- mapM letrec_binding_to_guarded_patterns lrbs
    return $ D_LetRec lrbs'

  instruction_to_guarded_patterns :: MonadState NamerState m => Instruction -> m Instruction
  instruction_to_guarded_patterns (IDF df) = do
    df' <- definition_to_guarded_patterns df
    return $ IDF df'
  instruction_to_guarded_patterns (IEX ex) = do
    ex' <- expression_to_guarded_patterns ex
    return $ IEX ex'

  program_to_guarded_patterns :: Program -> Program
  program_to_guarded_patterns p = 
    fst $ runState (mapM instruction_to_guarded_patterns p) empty_state
