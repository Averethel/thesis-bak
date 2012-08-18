{-# LANGUAGE 
  FlexibleContexts
  #-}

module Languages.EnrichedLambda.Typing (type_of_expression, type_of_program) where
  import Languages.EnrichedLambda.Errors
  import Languages.EnrichedLambda.PrettyPrint
  import Languages.EnrichedLambda.State
  import Languages.EnrichedLambda.Syntax
  
  import Control.Monad.Error
  import Control.Monad.State
  import Data.Maybe
  
  type_of_constant :: (MonadState InterpreterState m) => Constant -> m Type
  type_of_constant (C_Int _) = return T_Int
  type_of_constant C_True    = return T_Bool
  type_of_constant C_False   = return T_Bool
  type_of_constant C_Unit    = return T_Unit
  type_of_constant C_Nil     = do
    tv <- fresh_type_var
    return $ T_List tv
  
  type_of_unary_prim :: (MonadState InterpreterState m) => UnaryPrim -> m Type
  type_of_unary_prim U_Not = return $ T_Arrow T_Bool T_Bool
  type_of_unary_prim U_Ref = do
    tv <- fresh_type_var
    return $ T_Arrow tv (T_Ref tv)
  type_of_unary_prim U_Deref = do
    tv <- fresh_type_var
    return $ T_Arrow (T_Ref tv) tv
  type_of_unary_prim U_Fst = do
    tv1 <- fresh_type_var
    tv2 <- fresh_type_var
    return $ T_Arrow (T_Pair tv1 tv2) tv1
  type_of_unary_prim U_Snd = do
    tv1 <- fresh_type_var
    tv2 <- fresh_type_var
    return $ T_Arrow (T_Pair tv1 tv2) tv2
  type_of_unary_prim U_Head = do
    tv <- fresh_type_var
    return $ T_Arrow (T_List tv) tv
  type_of_unary_prim U_Tail = do
    tv <- fresh_type_var
    return $ T_Arrow (T_List tv) (T_List tv)
  type_of_unary_prim U_Empty = do
    tv <- fresh_type_var
    return $ T_Arrow (T_List tv) T_Bool
  
  type_of_binary_prim :: (MonadState InterpreterState m) => BinaryPrim -> m Type
  type_of_binary_prim B_Eq = do
    tv <- fresh_type_var
    add_simple_constraint tv
    return $ T_Arrow tv $ T_Arrow tv T_Bool
  type_of_binary_prim B_Plus = return $ T_Arrow T_Int $ T_Arrow T_Int T_Int
  type_of_binary_prim B_Minus = return $ T_Arrow T_Int $ T_Arrow T_Int T_Int
  type_of_binary_prim B_Mult = return $ T_Arrow T_Int $ T_Arrow T_Int T_Int
  type_of_binary_prim B_Div = return $ T_Arrow T_Int $ T_Arrow T_Int T_Int
  type_of_binary_prim B_Assign = do
    tv <- fresh_type_var
    return $ T_Arrow (T_Ref tv) $ T_Arrow tv T_Unit

  recfun :: (MonadError String m, MonadState InterpreterState m) => [(String, Expr)] -> m ()
  recfun lrbs = recfun' lrbs [] where
    recfun' []          []           = return ()
    recfun' []          ((tv, e):cs) = do
      t <- type_of_expression e
      add_constraint tv t
      recfun' [] cs
    recfun' ((v, e):bs) cs           = do
      tv <- fresh_type_var
      extend_typing_env v tv
      recfun' bs ((tv, e):cs)

  type_of_expression :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Type
  type_of_expression (E_Val v) = do
    env <- get_typing_env
    case env v of
      Nothing -> throwError $ unbound_variable v
      Just t  -> return t
  type_of_expression (E_UPrim up) = type_of_unary_prim up
  type_of_expression (E_BPrim bp) = type_of_binary_prim bp
  type_of_expression (E_Const c) = type_of_constant c
  type_of_expression (E_Cons e1 e2) = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    add_constraint t2 (T_List t1)
    return t2
  type_of_expression (E_ITE e1 e2 e3) = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    t3 <- type_of_expression e3
    add_constraint t1 T_Bool
    add_constraint t2 t3
    return t2
  type_of_expression (E_Seq e1 e2) = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    add_constraint t1 T_Unit -- maybe warinng only?
    return t2
  type_of_expression (E_Pair e1 e2) = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    return $ T_Pair t1 t2
  type_of_expression (E_Let v e1 e2) = do
    env <- get_typing_env
    t1 <- type_of_expression e1
    extend_typing_env v t1
    t2 <- type_of_expression e2
    reset_typing_env env
    return t2
  type_of_expression (E_LetRec lrbs e2) = do
    env <- get_typing_env
    recfun lrbs
    t2 <- type_of_expression e2
    reset_typing_env env
    return t2
  type_of_expression (E_Apply e1 e2) = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    tv <- fresh_type_var
    add_constraint t1 (T_Arrow t2 tv)
    return tv
  type_of_expression (E_Function v e) = do
    env <- get_typing_env
    tv <- fresh_type_var
    extend_typing_env v tv
    t <- type_of_expression e
    reset_typing_env env
    return (T_Arrow tv t)
  type_of_expression E_MatchFailure = do
    tv <- fresh_type_var
    return tv

  type_of_definition :: (MonadError String m, MonadState InterpreterState m) => Definition -> m ()
  type_of_definition (D_Let v e)     = do
    t <- type_of_expression e
    extend_typing_env v t
  type_of_definition (D_LetRec lrbs) = recfun lrbs

  type_of_instruction :: (MonadError String m, MonadState InterpreterState m) => Instruction -> m ()
  type_of_instruction (IDF df) = type_of_definition df
  type_of_instruction (IEX ex) = do
    t <- type_of_expression ex
    extend_typing_env "it" t

  type_of_program :: (MonadError String m, MonadState InterpreterState m) => Program -> m ()
  type_of_program []     = return ()
  type_of_program (i:is) = do
    type_of_instruction i
    type_of_program is
