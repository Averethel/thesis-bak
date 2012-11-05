{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Passes.EliminateWildcards (eliminateWildcards) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  type NamerState = Integer

  emptyState :: NamerState
  emptyState = 0

  newPatVar :: MonadState NamerState m => m String
  newPatVar = do
    s <- get
    put $ s + 1
    return $ "Wild_" ++ show s

  eliminateWildcardsPattern :: MonadState NamerState m => Pattern -> m Pattern
  eliminateWildcardsPattern P_Wildcard     = do
    v <- newPatVar
    return $ P_Val v
  eliminateWildcardsPattern (P_Cons p1 p2) = do
    p1' <- eliminateWildcardsPattern p1
    p2' <- eliminateWildcardsPattern p2
    return $ P_Cons p1' p2'
  eliminateWildcardsPattern (P_Tuple ps)   = do
    ps' <- mapM eliminateWildcardsPattern ps
    return $ P_Tuple ps'
  eliminateWildcardsPattern p              =
    return p

  eliminateWildcardsLetBindings :: MonadState NamerState m => [Binding] -> m [Binding]
  eliminateWildcardsLetBindings = mapM (\(p, e) -> do {
    p' <- eliminateWildcardsPattern p;
    e' <- eliminateWildcardsExpr e;
    return (p', e')
    })

  eliminateWildcardsBindings :: MonadState NamerState m => [FunBinding] -> m [FunBinding]
  eliminateWildcardsBindings = mapM (\(p, e, g) -> do {
    p' <- eliminateWildcardsPattern p;
    e' <- eliminateWildcardsExpr e;
    g' <- eliminateWildcardsExpr g;
    return (p', e', g')
    })

  eliminateWildcardsLetrecBindings :: MonadState NamerState m => [LetRecBinding] -> m [LetRecBinding]
  eliminateWildcardsLetrecBindings = mapM (\(n, e) -> do{
    e' <- eliminateWildcardsExpr e;
    return (n, e')
    })

  eliminateWildcardsExpr :: MonadState NamerState m => Expr -> m Expr
  eliminateWildcardsExpr (E_Apply e1 e2)    = do
    e1' <- eliminateWildcardsExpr e1
    e2' <- eliminateWildcardsExpr e2
    return $ E_Apply e1' e2'
  eliminateWildcardsExpr (E_Cons e1 e2)     = do
    e1' <- eliminateWildcardsExpr e1
    e2' <- eliminateWildcardsExpr e2
    return $ E_Cons e1' e2'
  eliminateWildcardsExpr (E_Tuple es)       = do
    es' <- mapM eliminateWildcardsExpr es
    return $ E_Tuple es'
  eliminateWildcardsExpr (E_And e1 e2)      = do
    e1' <- eliminateWildcardsExpr e1
    e2' <- eliminateWildcardsExpr e2
    return $ E_And e1' e2'
  eliminateWildcardsExpr (E_Or e1 e2)       = do
    e1' <- eliminateWildcardsExpr e1
    e2' <- eliminateWildcardsExpr e2
    return $ E_Or e1' e2'
  eliminateWildcardsExpr (E_ITE e1 e2 e3)   = do
    e1' <- eliminateWildcardsExpr e1
    e2' <- eliminateWildcardsExpr e2
    e3' <- eliminateWildcardsExpr e3
    return $ E_ITE e1' e2' e3'
  eliminateWildcardsExpr (E_Seq e1 e2)      = do
    e1' <- eliminateWildcardsExpr e1
    e2' <- eliminateWildcardsExpr e2
    return $ E_Seq e1' e2'
  eliminateWildcardsExpr (E_Function bs)    = do
    bs' <- eliminateWildcardsBindings bs
    return $ E_Function bs'
  eliminateWildcardsExpr (E_Let bs e1)      = do
    bs' <- eliminateWildcardsLetBindings bs
    e1' <- eliminateWildcardsExpr e1
    return $ E_Let bs' e1'
  eliminateWildcardsExpr (E_LetRec lrbs e1) = do
    bs' <- eliminateWildcardsLetrecBindings lrbs
    e1' <- eliminateWildcardsExpr e1
    return $ E_LetRec bs' e1'
  eliminateWildcardsExpr e                  =
    return e

  eliminateWildcardsDefinition :: MonadState NamerState m => Definition -> m Definition
  eliminateWildcardsDefinition (D_Let bs)    = do
    bs' <- eliminateWildcardsLetBindings bs
    return $ D_Let bs'
  eliminateWildcardsDefinition (D_LetRec bs) = do
    bs' <- eliminateWildcardsLetrecBindings bs
    return $ D_LetRec bs'

  eliminateWildcardsInstruction :: MonadState NamerState m => Instruction -> m Instruction
  eliminateWildcardsInstruction (IEX e) = do
    e' <- eliminateWildcardsExpr e
    return $ IEX e'
  eliminateWildcardsInstruction (IDF d) = do
    d' <- eliminateWildcardsDefinition d
    return $ IDF d'

  eliminateWildcardsProgram :: MonadState NamerState m => Program -> m Program
  eliminateWildcardsProgram = mapM eliminateWildcardsInstruction

  eliminateWildcards :: Program -> Program
  eliminateWildcards p = fst $ runState (eliminateWildcardsProgram p) emptyState
