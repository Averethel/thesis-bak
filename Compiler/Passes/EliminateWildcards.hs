{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Passes.EliminateWildcards (eliminate_wildcards) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  type NamerState = Integer

  empty_state :: NamerState
  empty_state = 0

  new_pat_var :: MonadState NamerState m => m String
  new_pat_var = do
    s <- get
    put $ s + 1
    return $ "_wild_" ++ show s

  eliminate_wildcards_pattern :: MonadState NamerState m => Pattern -> m Pattern
  eliminate_wildcards_pattern P_Wildcard     = do
    v <- new_pat_var
    return $ P_Val v
  eliminate_wildcards_pattern (P_Cons p1 p2) = do
    p1' <- eliminate_wildcards_pattern p1
    p2' <- eliminate_wildcards_pattern p2
    return $ P_Cons p1' p2'
  eliminate_wildcards_pattern (P_Tuple ps)   = do
    ps' <- mapM eliminate_wildcards_pattern ps
    return $ P_Tuple ps'
  eliminate_wildcards_pattern p              =
    return p

  eliminate_wildcards_let_bindings :: MonadState NamerState m => [Binding] -> m [Binding]
  eliminate_wildcards_let_bindings = mapM (\(p, e) -> do {
    p' <- eliminate_wildcards_pattern p;
    e' <- eliminate_wildcards_expr e;
    return (p', e')
    })

  eliminate_wildcards_bindings :: MonadState NamerState m => [FunBinding] -> m [FunBinding]
  eliminate_wildcards_bindings = mapM (\(p, e, g) -> do {
    p' <- eliminate_wildcards_pattern p;
    e' <- eliminate_wildcards_expr e;
    g' <- eliminate_wildcards_expr g;
    return (p', e', g')
    })

  eliminate_wildcards_letrec_bindings :: MonadState NamerState m => [LetRecBinding] -> m [LetRecBinding]
  eliminate_wildcards_letrec_bindings = mapM (\(n, e) -> do{
    e' <- eliminate_wildcards_expr e;
    return (n, e')
    })

  eliminate_wildcards_expr :: MonadState NamerState m => Expr -> m Expr
  eliminate_wildcards_expr (E_Apply e1 e2)    = do
    e1' <- eliminate_wildcards_expr e1
    e2' <- eliminate_wildcards_expr e2
    return $ E_Apply e1' e2'
  eliminate_wildcards_expr (E_Cons e1 e2)     = do
    e1' <- eliminate_wildcards_expr e1
    e2' <- eliminate_wildcards_expr e2
    return $ E_Cons e1' e2'
  eliminate_wildcards_expr (E_Tuple es)       = do
    es' <- mapM eliminate_wildcards_expr es
    return $ E_Tuple es'
  eliminate_wildcards_expr (E_And e1 e2)      = do
    e1' <- eliminate_wildcards_expr e1
    e2' <- eliminate_wildcards_expr e2
    return $ E_And e1' e2'
  eliminate_wildcards_expr (E_Or e1 e2)       = do
    e1' <- eliminate_wildcards_expr e1
    e2' <- eliminate_wildcards_expr e2
    return $ E_Or e1' e2'
  eliminate_wildcards_expr (E_ITE e1 e2 e3)   = do
    e1' <- eliminate_wildcards_expr e1
    e2' <- eliminate_wildcards_expr e2
    e3' <- eliminate_wildcards_expr e3
    return $ E_ITE e1' e2' e3'
  eliminate_wildcards_expr (E_Seq e1 e2)      = do
    e1' <- eliminate_wildcards_expr e1
    e2' <- eliminate_wildcards_expr e2
    return $ E_Seq e1' e2'
  eliminate_wildcards_expr (E_Function bs)    = do
    bs' <- eliminate_wildcards_bindings bs
    return $ E_Function bs'
  eliminate_wildcards_expr (E_Let bs e1)      = do
    bs' <- eliminate_wildcards_let_bindings bs
    e1' <- eliminate_wildcards_expr e1
    return $ E_Let bs' e1'
  eliminate_wildcards_expr (E_LetRec lrbs e1) = do
    bs' <- eliminate_wildcards_letrec_bindings lrbs
    e1' <- eliminate_wildcards_expr e1
    return $ E_LetRec bs' e1'
  eliminate_wildcards_expr e                  =
    return e

  eliminate_wildcards_definition :: MonadState NamerState m => Definition -> m Definition
  eliminate_wildcards_definition (D_Let bs)    = do
    bs' <- eliminate_wildcards_let_bindings bs
    return $ D_Let bs'
  eliminate_wildcards_definition (D_LetRec bs) = do
    bs' <- eliminate_wildcards_letrec_bindings bs
    return $ D_LetRec bs'

  eliminate_wildcards_instruction :: MonadState NamerState m => Instruction -> m Instruction
  eliminate_wildcards_instruction (IEX e) = do
    e' <- eliminate_wildcards_expr e
    return $ IEX e'
  eliminate_wildcards_instruction (IDF d) = do
    d' <- eliminate_wildcards_definition d
    return $ IDF d'

  eliminate_wildcards_program :: MonadState NamerState m => Program -> m Program
  eliminate_wildcards_program = mapM eliminate_wildcards_instruction

  eliminate_wildcards :: Program -> Program
  eliminate_wildcards p = fst $ runState (eliminate_wildcards_program p) empty_state
