{-# LANGUAGE 
  FlexibleContexts
  #-}

module Compiler.Passes.ConstantMatchingsToGuards (programToGuardedPatterns) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  -- Assumptions:
  -- 1. Wildcards were translated to variables
  -- 2. Tuples were folded to pairs

  type NamerState = Integer

  emptyState :: NamerState
  emptyState = 0

  newVar :: MonadState NamerState m => m String
  newVar = do
    s <- get
    put $ s + 1
    return $ "Pat_" ++ show s

  patternToGuardedPatterns :: MonadState NamerState m => Pattern -> m (Pattern, Expr)
  patternToGuardedPatterns (P_Const (C_Int n)) = do
    v <- newVar
    return (P_Val v, 
            E_Apply (E_Apply (E_BPrim B_Eq) $ E_Val v) $ E_Const $ C_Int n)
  patternToGuardedPatterns (P_Tuple [a, b])    = do
    (a', e1) <- patternToGuardedPatterns a
    (b', e2) <- patternToGuardedPatterns b
    let p' = P_Tuple [a', b']
    return $ case (e1, e2) of
      (E_Const C_True, E_Const C_True) -> (p', E_Const C_True)
      (_,              E_Const C_True) -> (p', e1)
      (E_Const C_True, _)              -> (p', e2)
      (_,              _)              -> (p', E_And e1 e2)
  patternToGuardedPatterns (P_Cons p1 p2)      = do
    (p1', e1) <- patternToGuardedPatterns p1
    (p2', e2) <- patternToGuardedPatterns p2
    let p' = P_Cons p1' p2'
    return $ case (e1, e2) of
      (E_Const C_True, E_Const C_True) -> (p', E_Const C_True)
      (_,              E_Const C_True) -> (p', e1)
      (E_Const C_True, _)              -> (p', e2)
      (_,              _)              -> (p', E_And e1 e2)
  patternToGuardedPatterns p                   =
    return (p, E_Const C_True)

  funBindingToGuardedPatterns :: MonadState NamerState m => FunBinding -> m FunBinding
  funBindingToGuardedPatterns (p, e, g) = do
    (p', g') <- patternToGuardedPatterns p
    return $ case (g, g') of
      (E_Const C_True, E_Const C_True) -> (p', e, E_Const C_True)
      (_,              E_Const C_True) -> (p', e, g)
      (E_Const C_True, _)              -> (p', e, g')
      (_,              _)              -> (p', e, E_And g g')

  letrecBindingToGuardedPatterns :: MonadState NamerState m => LetRecBinding -> m LetRecBinding
  letrecBindingToGuardedPatterns (v, e) = do
    e' <- expressionToGuardedPatterns e
    return (v, e')

  bindingToGuardedPatterns :: MonadState NamerState m => Binding -> m Binding
  bindingToGuardedPatterns (p, e) = do
    e' <- expressionToGuardedPatterns e
    return (p, e')

  expressionToGuardedPatterns :: MonadState NamerState m => Expr -> m Expr
  expressionToGuardedPatterns (E_Apply e1 e2)   = do
    e1' <- expressionToGuardedPatterns e1
    e2' <- expressionToGuardedPatterns e2
    return $ E_Apply e1' e2'
  expressionToGuardedPatterns (E_Cons e1 e2)    = do
    e1' <- expressionToGuardedPatterns e1
    e2' <- expressionToGuardedPatterns e2
    return $ E_Cons e1' e2'
  expressionToGuardedPatterns (E_Tuple es)      = do
    es' <- mapM expressionToGuardedPatterns es
    return $ E_Tuple es'
  expressionToGuardedPatterns (E_And e1 e2)     = do
    e1' <- expressionToGuardedPatterns e1
    e2' <- expressionToGuardedPatterns e2
    return $ E_And e1' e2'
  expressionToGuardedPatterns (E_Or e1 e2)      = do
    e1' <- expressionToGuardedPatterns e1
    e2' <- expressionToGuardedPatterns e2
    return $ E_Or e1' e2'
  expressionToGuardedPatterns (E_ITE e1 e2 e3)  = do
    e1' <- expressionToGuardedPatterns e1
    e2' <- expressionToGuardedPatterns e2
    e3' <- expressionToGuardedPatterns e3
    return $ E_ITE e1' e2' e3'
  expressionToGuardedPatterns (E_Seq e1 e2)     = do
    e1' <- expressionToGuardedPatterns e1
    e2' <- expressionToGuardedPatterns e2
    return $ E_Seq e1' e2'
  expressionToGuardedPatterns (E_Function fbs)  = do
    fbs' <- mapM funBindingToGuardedPatterns fbs
    return $ E_Function fbs'
  expressionToGuardedPatterns (E_Let bs e)      = do
    bs' <- mapM bindingToGuardedPatterns bs
    e'  <- expressionToGuardedPatterns e
    return $ E_Let bs' e'
  expressionToGuardedPatterns (E_LetRec lrbs e) = do
    lrbs' <- mapM letrecBindingToGuardedPatterns lrbs
    e'    <- expressionToGuardedPatterns e
    return $ E_LetRec lrbs' e'
  expressionToGuardedPatterns e                 =
    return e

  definitionToGuardedPatterns :: MonadState NamerState m => Definition -> m Definition
  definitionToGuardedPatterns (D_Let bs)      = do
    bs' <- mapM bindingToGuardedPatterns bs
    return $ D_Let bs'
  definitionToGuardedPatterns (D_LetRec lrbs) = do
    lrbs' <- mapM letrecBindingToGuardedPatterns lrbs
    return $ D_LetRec lrbs'

  instructionToGuardedPatterns :: MonadState NamerState m => Instruction -> m Instruction
  instructionToGuardedPatterns (IDF df) = do
    df' <- definitionToGuardedPatterns df
    return $ IDF df'
  instructionToGuardedPatterns (IEX ex) = do
    ex' <- expressionToGuardedPatterns ex
    return $ IEX ex'

  programToGuardedPatterns :: Program -> Program
  programToGuardedPatterns p = 
    fst $ runState (mapM instructionToGuardedPatterns p) emptyState
