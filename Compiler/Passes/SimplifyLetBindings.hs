{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Passes.SimplifyLetBindings (letBindingsToDeclarations) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  type NamerState = Integer

  emptyState :: NamerState
  emptyState = 0

  newVar :: MonadState NamerState m => m String
  newVar = do
    s <- get
    put $ s + 1
    return $ "Let_" ++ show s

  isVariable :: Pattern -> Bool
  isVariable (P_Val _) = True
  isVariable _         = False

  patternToBindings :: Pattern -> Expr -> [Binding]
  patternToBindings (P_Val v)        e =
    [(P_Val v, e)]
  patternToBindings (P_Const _)      _ =
    []
  patternToBindings (P_Tuple [a, b]) e =
    let 
      b1 = patternToBindings a (E_Apply (E_UPrim U_Fst) e)
      b2 = patternToBindings b (E_Apply (E_UPrim U_Snd) e)
    in 
      b1 ++ b2
  patternToBindings (P_Cons p1 p2)   e =
    let 
      b1 = patternToBindings p1 (E_Apply (E_UPrim U_Head) e)
      b2 = patternToBindings p2 (E_Apply (E_UPrim U_Tail) e)
    in
      b1 ++ b2

  bindingToDeclarations :: MonadState NamerState m => Binding -> m [Binding]
  bindingToDeclarations (p, e)
    | isVariable p = return [(p, e)]
    | otherwise     = do
      v <- newVar
      let bs = patternToBindings p (E_Val v)
      return $ (P_Val v, e):bs

  letBindingsToDeclarationsFunBinding :: MonadState NamerState m => FunBinding -> m FunBinding
  letBindingsToDeclarationsFunBinding (p, e, g) = do
    e' <- letBindingsToDeclarationsExpr e
    g' <- letBindingsToDeclarationsExpr g
    return (p, e', g')

  letBindingsToDeclarationsLetrecBinding :: MonadState NamerState m => LetRecBinding -> m LetRecBinding
  letBindingsToDeclarationsLetrecBinding (v, e) = do
    e' <- letBindingsToDeclarationsExpr e
    return (v, e')

  letBindingsToDeclarationsExpr :: MonadState NamerState m => Expr -> m Expr
  letBindingsToDeclarationsExpr (E_Apply e1 e2)   = do
    e1' <- letBindingsToDeclarationsExpr e1
    e2' <- letBindingsToDeclarationsExpr e2
    return $ E_Apply e1' e2'
  letBindingsToDeclarationsExpr (E_Cons e1 e2)    = do
    e1' <- letBindingsToDeclarationsExpr e1
    e2' <- letBindingsToDeclarationsExpr e2
    return $ E_Cons e1' e2'
  letBindingsToDeclarationsExpr (E_Tuple es)      = do
    es' <- mapM letBindingsToDeclarationsExpr es
    return $ E_Tuple es'
  letBindingsToDeclarationsExpr (E_And e1 e2)     = do
    e1' <- letBindingsToDeclarationsExpr e1
    e2' <- letBindingsToDeclarationsExpr e2
    return $ E_And e1' e2'
  letBindingsToDeclarationsExpr (E_Or e1 e2)      = do
    e1' <- letBindingsToDeclarationsExpr e1
    e2' <- letBindingsToDeclarationsExpr e2
    return $ E_Or e1' e2'
  letBindingsToDeclarationsExpr (E_ITE e1 e2 e3)  = do
    e1' <- letBindingsToDeclarationsExpr e1
    e2' <- letBindingsToDeclarationsExpr e2
    e3' <- letBindingsToDeclarationsExpr e3
    return $ E_ITE e1' e2' e3'
  letBindingsToDeclarationsExpr (E_Seq e1 e2)     = do
    e1' <- letBindingsToDeclarationsExpr e1
    e2' <- letBindingsToDeclarationsExpr e2
    return $ E_Seq e1' e2'
  letBindingsToDeclarationsExpr (E_Function fbs)  = do
    fbs' <- mapM letBindingsToDeclarationsFunBinding fbs
    return $ E_Function fbs'
  letBindingsToDeclarationsExpr (E_Let bs e)      = do
    bs' <- mapM bindingToDeclarations bs
    e'  <- letBindingsToDeclarationsExpr e
    return $ E_Let (concat bs') e'
  letBindingsToDeclarationsExpr (E_LetRec lrbs e) = do
    lrbs' <- mapM letBindingsToDeclarationsLetrecBinding lrbs
    e'    <- letBindingsToDeclarationsExpr e
    return $ E_LetRec lrbs' e
  letBindingsToDeclarationsExpr e                 =
    return e

  letBindingsToDeclarationsDefinition :: MonadState NamerState m => Definition -> m Definition
  letBindingsToDeclarationsDefinition (D_Let bs)      = do
    bs' <- mapM bindingToDeclarations bs
    return $ D_Let $ concat bs'
  letBindingsToDeclarationsDefinition (D_LetRec lrbs) = do
    lrbs' <- mapM letBindingsToDeclarationsLetrecBinding lrbs
    return $ D_LetRec lrbs'

  letBindingsToDeclarationsInsruction :: MonadState NamerState m => Instruction -> m Instruction
  letBindingsToDeclarationsInsruction (IDF df) = do
    df' <- letBindingsToDeclarationsDefinition df
    return $ IDF df'
  letBindingsToDeclarationsInsruction (IEX ex) = do
    ex' <- letBindingsToDeclarationsExpr ex
    return $ IEX ex'

  letBindingsToDeclarations :: Program -> Program
  letBindingsToDeclarations p = 
    fst $ runState (mapM letBindingsToDeclarationsInsruction p) emptyState
