{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Passes.FunctionsToFatbars (functionsToFatbars) where 
  import Languages.MiniML.Syntax

  import Control.Monad.State

  type NamerState = Integer

  emptyState :: NamerState
  emptyState = 0

  newVar :: MonadState NamerState m => m String
  newVar = do
    s <- get
    put $ s + 1
    return $ "Arg_" ++ show s

  functionsToFatbarsBinding :: MonadState NamerState m => Binding -> m Binding
  functionsToFatbarsBinding (p, e) = do
    e' <- functionsToFatbarsExpression e
    return (p, e')

  functionsToFatbarsFunBinding :: MonadState NamerState m => FunBinding -> m Expr
  functionsToFatbarsFunBinding (p, e, E_Const C_True) = do
    e' <- functionsToFatbarsExpression e
    return $ E_Function [(p, e', E_Const C_True)]
  functionsToFatbarsFunBinding (p, e, g)              = do
    e' <- functionsToFatbarsExpression e
    g' <- functionsToFatbarsExpression g
    return $ E_Function [(p, E_ITE g' e' E_MatchFailure, E_Const C_True)]

  functionsToFatbarsLetRecBinding :: MonadState NamerState m => LetRecBinding -> m LetRecBinding
  functionsToFatbarsLetRecBinding (v, e) = do
    e' <- functionsToFatbarsExpression e
    return (v, e')

  functionsToFatbarsExpression :: MonadState NamerState m => Expr -> m Expr
  functionsToFatbarsExpression (E_Apply e1 e2)  = do
    e1' <- functionsToFatbarsExpression e1
    e2' <- functionsToFatbarsExpression e2
    return $ E_Apply e1' e2'
  functionsToFatbarsExpression (E_Cons e1 e2)   = do
    e1' <- functionsToFatbarsExpression e1
    e2' <- functionsToFatbarsExpression e2
    return $ E_Cons e1' e2'
  functionsToFatbarsExpression (E_Tuple es)     = do
    es' <- mapM functionsToFatbarsExpression es
    return $ E_Tuple es'
  functionsToFatbarsExpression (E_And e1 e2)    = do
    e1' <- functionsToFatbarsExpression e1
    e2' <- functionsToFatbarsExpression e2
    return $ E_And e1' e2'
  functionsToFatbarsExpression (E_Or e1 e2)     = do
    e1' <- functionsToFatbarsExpression e1
    e2' <- functionsToFatbarsExpression e2
    return $ E_Or e1' e2'
  functionsToFatbarsExpression (E_ITE e1 e2 e3) = do
    e1' <- functionsToFatbarsExpression e1
    e2' <- functionsToFatbarsExpression e2
    e3' <- functionsToFatbarsExpression e3
    return $ E_ITE e1' e2' e3'
  functionsToFatbarsExpression (E_Seq e1 e2)    = do
    e1' <- functionsToFatbarsExpression e1
    e2' <- functionsToFatbarsExpression e2
    return $ E_Seq e1' e2' 
  functionsToFatbarsExpression (E_Function fbs)  = do
    fbs' <- mapM functionsToFatbarsFunBinding fbs
    v    <- newVar
    let fbs'' = map (\x -> E_Apply x $ E_Val v) fbs'
    return $ E_Function 
             [(P_Val v, foldr E_FatBar E_MatchFailure fbs'', E_Const C_True)]
  functionsToFatbarsExpression (E_Let lbs e)     = do
    lbs' <- mapM functionsToFatbarsBinding lbs
    e'   <- functionsToFatbarsExpression e
    return $ E_Let lbs' e'
  functionsToFatbarsExpression (E_LetRec lrbs e) = do  
    lrbs' <- mapM functionsToFatbarsLetRecBinding lrbs
    e'    <- functionsToFatbarsExpression e
    return $ E_LetRec lrbs' e'
  functionsToFatbarsExpression e                 =
    return e

  functionsToFatbarsDefinition :: MonadState NamerState m => Definition -> m Definition
  functionsToFatbarsDefinition (D_Let lbs)     = do
    lbs' <- mapM functionsToFatbarsBinding lbs
    return $ D_Let lbs'
  functionsToFatbarsDefinition (D_LetRec lrbs) = do
    lrbs' <- mapM functionsToFatbarsLetRecBinding lrbs
    return $ D_LetRec lrbs'

  functionsToFatbarsInstruction :: MonadState NamerState m => Instruction -> m Instruction
  functionsToFatbarsInstruction (IDF df) = do
    df' <- functionsToFatbarsDefinition df
    return $ IDF df'
  functionsToFatbarsInstruction (IEX ex) = do
    ex' <- functionsToFatbarsExpression ex
    return $ IEX ex'

  functionsToFatbars :: Program -> Program
  functionsToFatbars p = 
    fst $ runState (mapM functionsToFatbarsInstruction p) emptyState
