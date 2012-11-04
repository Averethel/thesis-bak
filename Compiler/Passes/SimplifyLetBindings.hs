{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Passes.SimplifyLetBindings (let_bindings_to_declarations) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  type NamerState = Integer

  empty_state :: NamerState
  empty_state = 0

  new_var :: MonadState NamerState m => m String
  new_var = do
    s <- get
    put $ s + 1
    return $ "_let_" ++ show s

  is_variable :: Pattern -> Bool
  is_variable (P_Val _) = True
  is_variable _         = False

  pattern_to_bindings :: Pattern -> Expr -> [Binding]
  pattern_to_bindings (P_Val v)        e =
    [(P_Val v, e)]
  pattern_to_bindings (P_Const _)      _ =
    []
  pattern_to_bindings (P_Tuple [a, b]) e =
    let 
      b1 = pattern_to_bindings a (E_Apply (E_UPrim U_Fst) e)
      b2 = pattern_to_bindings b (E_Apply (E_UPrim U_Snd) e)
    in 
      b1 ++ b2
  pattern_to_bindings (P_Cons p1 p2)   e =
    let 
      b1 = pattern_to_bindings p1 (E_Apply (E_UPrim U_Head) e)
      b2 = pattern_to_bindings p2 (E_Apply (E_UPrim U_Tail) e)
    in
      b1 ++ b2

  binding_to_declarations :: MonadState NamerState m => Binding -> m [Binding]
  binding_to_declarations (p, e)
    | is_variable p = return [(p, e)]
    | otherwise     = do
      v <- new_var
      let bs = pattern_to_bindings p (E_Val v)
      return $ (P_Val v, e):bs

  let_bindings_to_declarations_fun_binding :: MonadState NamerState m => FunBinding -> m FunBinding
  let_bindings_to_declarations_fun_binding (p, e, g) = do
    e' <- let_bindings_to_declarations_expr e
    g' <- let_bindings_to_declarations_expr g
    return (p, e', g')

  let_bindings_to_declarations_letrec_binding :: MonadState NamerState m => LetRecBinding -> m LetRecBinding
  let_bindings_to_declarations_letrec_binding (v, fbs) = do
    fbs' <- mapM let_bindings_to_declarations_fun_binding fbs
    return (v, fbs')

  let_bindings_to_declarations_expr :: MonadState NamerState m => Expr -> m Expr
  let_bindings_to_declarations_expr (E_Apply e1 e2)   = do
    e1' <- let_bindings_to_declarations_expr e1
    e2' <- let_bindings_to_declarations_expr e2
    return $ E_Apply e1' e2'
  let_bindings_to_declarations_expr (E_Cons e1 e2)    = do
    e1' <- let_bindings_to_declarations_expr e1
    e2' <- let_bindings_to_declarations_expr e2
    return $ E_Cons e1' e2'
  let_bindings_to_declarations_expr (E_Tuple es)      = do
    es' <- mapM let_bindings_to_declarations_expr es
    return $ E_Tuple es'
  let_bindings_to_declarations_expr (E_And e1 e2)     = do
    e1' <- let_bindings_to_declarations_expr e1
    e2' <- let_bindings_to_declarations_expr e2
    return $ E_And e1' e2'
  let_bindings_to_declarations_expr (E_Or e1 e2)      = do
    e1' <- let_bindings_to_declarations_expr e1
    e2' <- let_bindings_to_declarations_expr e2
    return $ E_Or e1' e2'
  let_bindings_to_declarations_expr (E_ITE e1 e2 e3)  = do
    e1' <- let_bindings_to_declarations_expr e1
    e2' <- let_bindings_to_declarations_expr e2
    e3' <- let_bindings_to_declarations_expr e3
    return $ E_ITE e1' e2' e3'
  let_bindings_to_declarations_expr (E_Seq e1 e2)     = do
    e1' <- let_bindings_to_declarations_expr e1
    e2' <- let_bindings_to_declarations_expr e2
    return $ E_Seq e1' e2'
  let_bindings_to_declarations_expr (E_Function fbs)  = do
    fbs' <- mapM let_bindings_to_declarations_fun_binding fbs
    return $ E_Function fbs'
  let_bindings_to_declarations_expr (E_Let bs e)      = do
    bs' <- mapM binding_to_declarations bs
    e'  <- let_bindings_to_declarations_expr e
    return $ E_Let (concat bs') e'
  let_bindings_to_declarations_expr (E_LetRec lrbs e) = do
    lrbs' <- mapM let_bindings_to_declarations_letrec_binding lrbs
    e'    <- let_bindings_to_declarations_expr e
    return $ E_LetRec lrbs' e
  let_bindings_to_declarations_expr e                 =
    return e

  let_bindings_to_declarations_definition :: MonadState NamerState m => Definition -> m Definition
  let_bindings_to_declarations_definition (D_Let bs)      = do
    bs' <- mapM binding_to_declarations bs
    return $ D_Let $ concat bs'
  let_bindings_to_declarations_definition (D_LetRec lrbs) = do
    lrbs' <- mapM let_bindings_to_declarations_letrec_binding lrbs
    return $ D_LetRec lrbs'

  let_bindings_to_declarations_insruction :: MonadState NamerState m => Instruction -> m Instruction
  let_bindings_to_declarations_insruction (IDF df) = do
    df' <- let_bindings_to_declarations_definition df
    return $ IDF df'
  let_bindings_to_declarations_insruction (IEX ex) = do
    ex' <- let_bindings_to_declarations_expr ex
    return $ IEX ex'

  let_bindings_to_declarations :: Program -> Program
  let_bindings_to_declarations p = 
    fst $ runState (mapM let_bindings_to_declarations_insruction p) empty_state
