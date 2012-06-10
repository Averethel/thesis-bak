{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.EnrichedLambdaLowLevelTypes.Typing where
  import Languages.EnrichedLambdaLowLevelTypes.Errors
  import Languages.EnrichedLambdaLowLevelTypes.PrettyPrint
  import Languages.EnrichedLambdaLowLevelTypes.State
  import Languages.EnrichedLambdaLowLevelTypes.Syntax
  
  import Control.Monad.Error
  import Control.Monad.State
  
  deref :: Type -> Type
  deref (T_Ref t) = t
  deref t         = t
  
  type_of_struct :: (MonadError String m, MonadState InterpreterState m) => Struct -> m Type
  type_of_struct (S_Ref e)                = do
    t <- type_of_expression e
    return $ T_Ref t
  type_of_struct (S_Int _)                = return T_Int
  type_of_struct (S_Str Tg_True _ [])     = return T_Bool
  type_of_struct (S_Str Tg_False _ [])    = return T_Bool
  type_of_struct (S_Str Tg_Unit _ [])     = return T_Unit
  type_of_struct (S_Str Tg_Pair _ [a, b]) = do
    t1 <- type_of_struct a
    t2 <- type_of_struct b
    return $ T_Pair t1 t2
  type_of_struct (S_Str Tg_Nil _ _)       = do
    t <- fresh_type_var
    return $ T_List t
  type_of_struct (S_Str Tg_Cons _ [a, b]) = do
    t1 <- type_of_struct a
    t2 <- type_of_struct b
    add_constraint (deref t2) $ T_List t1
    return $ T_List t1
  type_of_struct (S_Ptr a) = do
    s <- get
    let e = (dynamic_memory s) `at` a
    t <- type_of_expression e
    return $ T_Ref t
  type_of_struct (S_StaticPtr a) = do
    s <- get
    let e = (static_memory s) `at` a
    t <- type_of_expression e
    return $ T_Ref t
  
  type_of_prim :: (MonadError String m, MonadState InterpreterState m) => Prim -> m Type
  type_of_prim P_AllocPair = do
    a <- fresh_type_var
    b <- fresh_type_var
    return $ T_Arrow a $ T_Arrow b $ T_Pair a b
  type_of_prim P_AllocList = do
    a <- fresh_type_var
    return $ T_Arrow a $ T_Arrow (T_List a) $ T_List a
  type_of_prim P_Not       = return $ T_Arrow T_Bool T_Bool
  type_of_prim P_Ref       = do
    a <- fresh_type_var
    return $ T_Arrow a $ T_Ref a
  type_of_prim P_Deref     = do
    a <- fresh_type_var
    return $ T_Arrow (T_Ref a) a
  type_of_prim P_Fst       = do
    a <- fresh_type_var
    b <- fresh_type_var
    return $ T_Arrow (T_Pair a b) a
  type_of_prim P_Snd       = do
    a <- fresh_type_var
    b <- fresh_type_var
    return $ T_Arrow (T_Pair a b) b
  type_of_prim P_Head      = do
    a <- fresh_type_var
    return $ T_Arrow (T_List a) a
  type_of_prim P_Tail      = do
    a <- fresh_type_var
    return $ T_Arrow (T_List a) $ T_List a
  type_of_prim P_Empty     = do
    a <- fresh_type_var
    return $ T_Arrow (T_List a) T_Bool
  type_of_prim P_Eq        = do
    a <- fresh_type_var
    add_simple_constraint a
    return $ T_Arrow a $ T_Arrow a $ T_Bool
  type_of_prim P_Plus      = return $ T_Arrow T_Int $ T_Arrow T_Int $ T_Int
  type_of_prim P_Minus     = return $ T_Arrow T_Int $ T_Arrow T_Int $ T_Int
  type_of_prim P_Mult      = return $ T_Arrow T_Int $ T_Arrow T_Int $ T_Int
  type_of_prim P_Div       = return $ T_Arrow T_Int $ T_Arrow T_Int $ T_Int
  type_of_prim P_Assign    = do
    a <- fresh_type_var
    return $ T_Arrow (T_Ref a) $ T_Arrow a $ T_Unit
  
  type_of_expression :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Type
  type_of_expression (E_Prim p)         = type_of_prim p
  type_of_expression (E_Struct s)       = type_of_struct s
  type_of_expression (E_Val v)          = do
    env <- get_typing_env
    case env v of
      Nothing -> throwError $ unbound_variable v
      Just t  -> return t
  type_of_expression (E_ITE e1 e2 e3)   = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    t3 <- type_of_expression e3
    add_constraint T_Bool t1
    add_constraint t2 t3
    return t2
  type_of_expression (E_Seq e1 e2)      = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    add_constraint T_Unit t1
    return t2
  type_of_expression (E_Let v e1 e2)    = do
    env <- get_typing_env
    t1 <- type_of_expression e1
    extend_typing_env v t1
    t2 <- type_of_expression e2
    reset_typing_env env
    return t2
  type_of_expression (E_Letrec v e1 e2) = do
    env <- get_typing_env
    tv <- fresh_type_var
    extend_typing_env v tv
    t1 <- type_of_expression e1
    add_constraint t1 tv
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
    return $ T_Arrow tv t
  type_of_expression E_MatchFailure = do
    tv <- fresh_type_var
    return tv
