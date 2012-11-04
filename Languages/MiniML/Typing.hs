{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Typing (type_of_definition, type_of_expression, type_of_program) where
  import Utils.Errors

  import Languages.MiniML.Kinding
  import Languages.MiniML.State
  import Languages.MiniML.Syntax
  import Languages.MiniML.PrettyPrint

  import Control.Monad.Error
  import Control.Monad.State

  names_distinct :: [ValueName] -> Bool
  names_distinct []     = True
  names_distinct (x:xs) = names_distinct' x xs xs where
    names_distinct' x []     []     = True
    names_distinct' x []     (y:ys) = names_distinct' y ys ys
    names_distinct' x (z:zs) ys
      | x /= z                      = names_distinct' x zs ys
      | otherwise                   = False

  get_names :: Pattern -> [ValueName]
  get_names (P_Val x)      = [x]
  get_names (P_Tuple es)   = concatMap get_names es
  get_names (P_Cons p1 p2) = get_names p1 ++ get_names p2
  get_names _              = []

  type_of_constant :: (MonadState InterpreterState m, MonadError String m) => Constant -> m TypeExpr
  type_of_constant (C_Int n) = 
    return $ TE_Constr [] Int
  type_of_constant C_False   =
    return $ TE_Constr [] Bool
  type_of_constant C_True    = 
    return $ TE_Constr [] Bool
  type_of_constant C_Nil     = do
    v <- fresh_type_var
    return $ TE_Constr [v] List
  type_of_constant C_Unit    =
    return $ TE_Constr [] Unit
  
  type_of_unary_primitive :: (MonadState InterpreterState m, MonadError String m) => UnaryPrim -> m TypeExpr
  type_of_unary_primitive U_Not     = 
    return $ TE_Arrow (TE_Constr [] Bool) (TE_Constr [] Bool)
  type_of_unary_primitive U_Ref     = do
    v <- fresh_type_var
    return $ TE_Arrow v (TE_Constr [v] Ref)
  type_of_unary_primitive U_Deref   = do
    v <- fresh_type_var
    return $ TE_Arrow (TE_Constr [v] Ref) v
  type_of_unary_primitive U_I_Minus =
    return $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  type_of_unary_primitive U_Fst     = do
    v1 <- fresh_type_var
    v2 <- fresh_type_var
    return $ TE_Arrow (TE_Tuple [v1, v2]) v1
  type_of_unary_primitive U_Snd     = do
    v1 <- fresh_type_var
    v2 <- fresh_type_var
    return $ TE_Arrow (TE_Tuple [v1, v2]) v2
  type_of_unary_primitive U_Empty   = do
    v <- fresh_type_var
    return $ TE_Arrow (TE_Constr [v] List) (TE_Constr [] Bool)
  type_of_unary_primitive U_Head    = do
    v <- fresh_type_var
    return $ TE_Arrow (TE_Constr [v] List) v
  type_of_unary_primitive U_Tail    = do
    v <- fresh_type_var
    return $ TE_Arrow (TE_Constr [v] List) $ TE_Constr [v] List

  type_of_binary_primitive :: (MonadState InterpreterState m, MonadError String m) => BinaryPrim -> m TypeExpr
  type_of_binary_primitive B_Eq      = do
    v <- fresh_type_var
    add_simple_constraint v
    return $ TE_Arrow v $ TE_Arrow v (TE_Constr [] Bool)
  type_of_binary_primitive B_I_Plus  = 
    return $ TE_Arrow (TE_Constr [] Int) $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  type_of_binary_primitive B_I_Minus =
    return $ TE_Arrow (TE_Constr [] Int) $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  type_of_binary_primitive B_I_Mult  =
    return $ TE_Arrow (TE_Constr [] Int) $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  type_of_binary_primitive B_I_Div   =
    return $ TE_Arrow (TE_Constr [] Int) $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  type_of_binary_primitive B_Assign  = do
    v <- fresh_type_var
    return $ TE_Arrow (TE_Constr [v] Ref) $ TE_Arrow v (TE_Constr [] Unit)

  type_and_bindings_of_pattern  :: (MonadState InterpreterState m, MonadError String m) => Pattern -> m TypeExpr
  type_and_bindings_of_pattern p
    | names_distinct $ get_names p = type_and_bindings_of_pattern' p
    | otherwise                    = throwError $ non_distinct_names p
    where
      type_and_bindings_of_pattern' :: (MonadState InterpreterState m, MonadError String m) => Pattern -> m TypeExpr
      type_and_bindings_of_pattern' (P_Val x)      = do
        v <- fresh_type_var
        extend_typing_env x v
        return $ v
      type_and_bindings_of_pattern' P_Wildcard     =
        fresh_type_var
      type_and_bindings_of_pattern' (P_Const c)    =
        type_of_constant c
      type_and_bindings_of_pattern' (P_Tuple es)   = do
        ts <- mapM type_and_bindings_of_pattern es
        return $ TE_Tuple ts
      type_and_bindings_of_pattern' (P_Cons p1 p2) = do
        t1 <- type_and_bindings_of_pattern' p1
        t2 <- type_and_bindings_of_pattern' p2
        add_constraint t2 $ TE_Constr [t1] List
        return t2

  recfun :: (MonadError String m, MonadState InterpreterState m) => [LetRecBinding] -> m ()
  recfun lrbs = recfun' (map fst lrbs) lrbs [] [] where
    recfun' :: (MonadError String m, MonadState InterpreterState m) => [ValueName] -> [LetRecBinding] -> [TypeExpr] -> [TypeExpr] -> m ()
    recfun' []     []             ts1 ts2 = add_bindings_constraints $ zip ts1 ts2
    recfun' []     ((n, e):lrbs) ts1 ts2 = do
      tp <- type_of_expression e
      recfun' [] lrbs ts1 (tp:ts2)
    recfun' (n:ns) lrbs           ts1 ts2 = do
      t <- fresh_type_var
      extend_typing_env n t
      recfun' ns lrbs (t:ts1) ts2
    add_bindings_constraints :: (MonadError String m, MonadState InterpreterState m) => [(TypeExpr, TypeExpr)] -> m ()
    add_bindings_constraints []            = return ()
    add_bindings_constraints ((t1, t2):ts) = do
      add_constraint t1 t2
      add_bindings_constraints ts

  type_of_function :: (MonadError String m, MonadState InterpreterState m) => [FunBinding] -> m TypeExpr
  type_of_function bs = type_of_function' bs [] where
    type_of_function' :: (MonadError String m, MonadState InterpreterState m) => [FunBinding] -> [TypeExpr] -> m TypeExpr
    type_of_function' []          acc = add_function_constraints acc
    type_of_function' ((p, e, g):es) acc = do
      env <- get_typing_env
      t1  <- type_and_bindings_of_pattern p
      t2  <- type_of_expression e
      tg  <- type_of_expression g
      add_constraint tg (TE_Constr [] Bool)
      reset_typing_env env
      type_of_function' es ((TE_Arrow t1 t2):acc)
    add_function_constraints :: (MonadError String m, MonadState InterpreterState m) => [TypeExpr] -> m TypeExpr
    add_function_constraints [t]        = return t
    add_function_constraints (t1:t2:ts) = do 
      add_constraint t1 t2
      add_function_constraints (t2:ts)

  type_of_case :: (MonadError String m, MonadState InterpreterState m) => [Binding] -> m TypeExpr
  type_of_case bs = type_of_case' bs [] where
    type_of_case' :: (MonadError String m, MonadState InterpreterState m) => [Binding] -> [TypeExpr] -> m TypeExpr
    type_of_case' []          acc = add_case_constraints acc
    type_of_case' ((p, e):es) acc = do
      env <- get_typing_env
      t1  <- type_and_bindings_of_pattern p
      t2  <- type_of_expression e
      reset_typing_env env
      type_of_case' es ((TE_Arrow t1 t2):acc)
    add_case_constraints :: (MonadError String m, MonadState InterpreterState m) => [TypeExpr] -> m TypeExpr
    add_case_constraints [t]        = return t
    add_case_constraints (t1:t2:ts) = do 
      add_constraint t1 t2
      add_case_constraints (t2:ts)


  type_of_bindings :: (MonadError String m, MonadState InterpreterState m) => [Binding] -> m ()
  type_of_bindings []          =
    return ()
  type_of_bindings ((p, e):bs) = do
    tp  <- type_and_bindings_of_pattern p
    te  <- type_of_expression e
    add_constraint tp te
    type_of_bindings bs

  type_of_expression :: (MonadError String m, MonadState InterpreterState m) => Expr -> m TypeExpr
  type_of_expression (E_UPrim up)       =
    type_of_unary_primitive up
  type_of_expression (E_BPrim bp)       =
    type_of_binary_primitive bp
  type_of_expression (E_Val v)          = do
    env <- get_typing_env
    case env v of 
      Nothing -> throwError $ unbound_variable v
      Just t  -> return t
  type_of_expression (E_Const c)        =
    type_of_constant c
  type_of_expression (E_Apply e1 e2)    = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    tv <- fresh_type_var
    add_constraint t1 (TE_Arrow t2 tv)
    return tv
  type_of_expression (E_Cons e1 e2)     = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    add_constraint t2 (TE_Constr [t1] List)
    return t2
  type_of_expression (E_Tuple es)       = do
    ts <- mapM type_of_expression es
    return $ TE_Tuple ts
  type_of_expression (E_And e1 e2)      = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    add_constraint t1 $ TE_Constr [] Bool
    add_constraint t2 $ TE_Constr [] Bool
    return $ TE_Constr [] Bool
  type_of_expression (E_Or e1 e2)       = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    add_constraint t1 $ TE_Constr [] Bool
    add_constraint t2 $ TE_Constr [] Bool
    return $ TE_Constr [] Bool
  type_of_expression (E_ITE e1 e2 e3)   = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    t3 <- type_of_expression e3
    add_constraint t1 $ TE_Constr [] Bool
    add_constraint t2 t3
    return t3
  type_of_expression (E_Case e1 bs)     = do
    t1                 <- type_of_expression e1
    (TE_Arrow t1' t2') <- type_of_case bs
    add_constraint t1' t1
    return t2'
  type_of_expression (E_Seq e1 e2)      = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    add_constraint t1 $ TE_Constr [] Unit
    return t2
  type_of_expression (E_Function bs)    = do
    type_of_function bs
  type_of_expression (E_Let bs e2)      = do
    env <- get_typing_env
    type_of_bindings bs
    t2  <- type_of_expression e2
    reset_typing_env env
    return t2
  type_of_expression (E_LetRec lrbs e2) = do
    env <- get_typing_env
    recfun lrbs
    t2 <- type_of_expression e2
    reset_typing_env env
    return t2
  type_of_expression E_MatchFailure     = do
    tv <- fresh_type_var
    return tv
  type_of_expression (E_FatBar e1 e2)   = do
    t1 <- type_of_expression e1
    t2 <- type_of_expression e2
    add_constraint t1 t2
    return t1

  type_of_definition :: (MonadError String m, MonadState InterpreterState m) => Definition -> m ()
  type_of_definition (D_Let bs)      = type_of_bindings bs
  type_of_definition (D_LetRec lrbs) = recfun lrbs

  type_of_instruction :: (MonadError String m, MonadState InterpreterState m) => Instruction -> m ()
  type_of_instruction (IDF df) = type_of_definition df
  type_of_instruction (IEX ex) = do
    t <- type_of_expression ex
    extend_typing_env "it" t

  type_of_program :: (MonadState InterpreterState m, MonadError String m) => Program -> m ()
  type_of_program []     = return ()
  type_of_program (i:is) = do
    type_of_instruction i
    type_of_program is
