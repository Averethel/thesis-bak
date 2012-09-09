{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Eval (eval_program, eval_expr, eval_definition) where
  import Languages.MiniML.Errors
  import Languages.MiniML.State
  import Languages.MiniML.Syntax

  import Control.Monad.Error
  import Control.Monad.State

  is_value :: Expr -> Bool
  is_value (E_UPrim _)             = True
  is_value (E_BPrim _)             = True
  is_value (E_Location _)          = True
  is_value (E_Const _)             = True
  is_value (E_Function _)          = True
  is_value (E_Apply (E_BPrim _) e) = is_value e
  is_value (E_Cons e1 e2)          = is_value e1 && is_value e2
  is_value (E_Tuple es)            = all is_value es
  is_value _                       = False

  recfun :: (MonadState InterpreterState m) => [LetRecBinding] -> m ()
  recfun []          = return ()
  recfun ((v, b):bs) = do
    extend_eval_env v $ E_Function b
    recfun bs

  matches :: Expr -> Pattern -> Bool
  e              `matches` (P_Val x)      = True
  e              `matches` P_Wildcard     = True
  (E_Const c1)   `matches` (P_Const c2)   = c1 == c2
  (E_Tuple es)   `matches` (P_Tuple ps)
    | length es == length ps              = and $ zipWith matches es ps
  (E_Cons e1 e2) `matches` (P_Cons p1 p2) = e1 `matches` p1 && e2 `matches` p2
  _              `matches` _              = False

  matches_of_pattern :: (MonadError String m, MonadState InterpreterState m) => Pattern -> Expr -> m ()
  matches_of_pattern (P_Val x)        e                =
    extend_eval_env x e
  matches_of_pattern P_Wildcard       _                =
    return ()
  matches_of_pattern (P_Const _)      (E_Const _)      =
    return ()
  matches_of_pattern (P_Tuple [])     (E_Tuple [])     =
    return ()
  matches_of_pattern (P_Tuple (p:ps)) (E_Tuple (e:es)) = do
    matches_of_pattern p e
    matches_of_pattern (P_Tuple ps) $ E_Tuple es
  matches_of_pattern (P_Cons p1 p2)   (E_Cons e1 e2)   = do
    matches_of_pattern p1 e1
    matches_of_pattern p2 e2

  eval_unary_primitive :: (MonadError String m, MonadState InterpreterState m) => UnaryPrim -> Expr -> m Expr
  eval_unary_primitive U_Not     (E_Const C_False)   =
    return $ E_Const C_True
  eval_unary_primitive U_Not     (E_Const C_True)    =
    return $ E_Const C_False
  eval_unary_primitive U_Ref     e                   = do
    addr <- store e
    return $ E_Location addr
  eval_unary_primitive U_Deref   (E_Location addr)   =
    load addr
  eval_unary_primitive U_I_Minus (E_Const (C_Int n)) =
    return $ E_Const $ C_Int $ 0 - n

  eval_binary_primitive :: (MonadError String m, MonadState InterpreterState m) => BinaryPrim -> Expr -> Expr -> m Expr 
  eval_binary_primitive B_Eq      (E_Const c1)        (E_Const c2)
    | c1 == c2                                                            = 
      return $ E_Const C_True
    | otherwise                                                           =
      return $ E_Const C_False
  eval_binary_primitive B_Eq      e1@(E_Location _)   e2@(E_Location _)   =
    return $ E_Apply (E_Apply (E_BPrim B_Eq) $ E_Apply (E_UPrim U_Deref) e1) $ E_Apply (E_UPrim U_Deref) e2
  eval_binary_primitive B_Eq      (E_Tuple [])        (E_Tuple [])        =
    return $ E_Const C_True
  eval_binary_primitive B_Eq      (E_Tuple [])        (E_Tuple _)         =
    return $ E_Const C_False
  eval_binary_primitive B_Eq      (E_Tuple _)         (E_Tuple [])        =
    return $ E_Const C_False
  eval_binary_primitive B_Eq      (E_Tuple (e1:es1))  (E_Tuple (e2:es2))  =
    return $ E_And (E_Apply (E_Apply (E_BPrim B_Eq) e1) e2) $ E_Apply (E_Apply (E_BPrim B_Eq) $ E_Tuple es1) $ E_Tuple es2
  eval_binary_primitive B_Eq      (E_Const C_Nil)     (E_Cons _ _)        =
    return $ E_Const C_False
  eval_binary_primitive B_Eq      (E_Cons _ _)        (E_Const C_Nil)     =
    return $ E_Const C_False
  eval_binary_primitive B_Eq      (E_Cons e1 e2)      (E_Cons e3 e4)      =
    return $ E_And (E_Apply (E_Apply (E_BPrim B_Eq) e1) e3) $ E_Apply (E_Apply (E_BPrim B_Eq) e2) e4
  eval_binary_primitive B_I_Plus  (E_Const (C_Int n)) (E_Const (C_Int m)) =
    return $ E_Const $ C_Int $ n + m
  eval_binary_primitive B_I_Minus (E_Const (C_Int n)) (E_Const (C_Int m)) =
    return $ E_Const $ C_Int $ n - m
  eval_binary_primitive B_I_Mult  (E_Const (C_Int n)) (E_Const (C_Int m)) =
    return $ E_Const $ C_Int $ n * m
  eval_binary_primitive B_I_Div   _                   (E_Const (C_Int 0)) =
    throwError division_by_0
  eval_binary_primitive B_I_Div   (E_Const (C_Int n)) (E_Const (C_Int m)) =
    return $ E_Const $ C_Int $ n `div` m
  eval_binary_primitive B_Assign  (E_Location addr)   e                   = do
    store_at addr e
    return $ E_Const C_Unit

  eval_function :: (MonadError String m, MonadState InterpreterState m) => [Binding] -> Expr -> m Expr
  eval_function []           _  =
    throwError match_failure
  eval_function ((p, e2):bs) e1
    | e1 `matches` p            = do
      matches_of_pattern p e1
      return e2
    | otherwise                 =
      eval_function bs e1

  eval_step_tuple :: (MonadError String m, MonadState InterpreterState m) => [Expr] -> [Expr] -> m Expr
  eval_step_tuple []     acc = 
    return $ E_Tuple $ reverse acc
  eval_step_tuple (e:es) acc
    | is_value e             =
      eval_step_tuple es (e:acc)
    | not . is_value $ e     = do
      e' <- eval_step_expr e
      return $ E_Tuple $ (reverse acc) ++ e' : es

  eval_step_expr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  eval_step_expr (E_Val vn)                             = do
    env <- get_eval_env
    case env vn of
      Nothing -> throwError $ unbound_variable vn
      Just e  -> return e
  eval_step_expr (E_Cons e1 e2)
    | is_value e1 && (not . is_value $ e2)              = do
      e2' <- eval_step_expr e2
      return $ E_Cons e1 e2'
    | not . is_value $ e1                               = do
      e1' <- eval_step_expr e1
      return $ E_Cons e1' e2
  eval_step_expr (E_And e1 e2)
    | not . is_value $ e1                               = do
      e1' <- eval_step_expr e1
      return $ E_And e1' e2
  eval_step_expr (E_And (E_Const C_False) _)            =
    return $ E_Const C_False
  eval_step_expr (E_And (E_Const C_True) e2)            =
    return e2
  eval_step_expr (E_Or e1 e2)
    | not . is_value $ e1                               = do
      e1' <- eval_step_expr e1
      return $ E_Or e1' e2
  eval_step_expr (E_Or (E_Const C_True) _)              =
    return $ E_Const C_True
  eval_step_expr (E_Or (E_Const C_False) e2)            =
    return e2
  eval_step_expr (E_ITE e1 e2 e3)
    | not . is_value $ e1                               = do
      e1' <- eval_step_expr e1
      return $ E_ITE e1' e2 e3
  eval_step_expr (E_ITE (E_Const C_True) e2 _)          =
    return e2
  eval_step_expr (E_ITE (E_Const C_False) _ e3)         =
    return e3
  eval_step_expr (E_Case e1 bs)                         =
    eval_function bs e1
  eval_step_expr (E_Seq e1 e2)
    | not . is_value $ e1                               = do
      e1' <- eval_step_expr e1
      return $ E_Seq e1' e2
  eval_step_expr (E_Seq (E_Const C_Unit) e2)            = 
    return e2
  eval_step_expr (E_Apply e1 e2)
    | is_value e1 && (not . is_value $ e2)              = do
      e2' <- eval_step_expr e2
      return $ E_Apply e1 e2'
    | not . is_value $ e1                               = do
      e1' <- eval_step_expr e1
      return $ E_Apply e1' e2 
  eval_step_expr (E_Apply (E_UPrim up) e1)              = 
    eval_unary_primitive up e1
  eval_step_expr (E_Apply (E_Apply (E_BPrim bp) e1) e2) = 
    eval_binary_primitive bp e1 e2
  eval_step_expr (E_Apply (E_Function pm) e2)           = 
    eval_function pm e2
  eval_step_expr (E_Tuple es)                           =
    eval_step_tuple es []
  eval_step_expr (E_Let (p, e1) e2)
    | not . is_value $ e1                               = do
      e1' <- eval_step_expr e1
      return $ E_Let (p, e1') e2
    | is_value e1                                       = do
      matches_of_pattern p e1
      return e2
  eval_step_expr (E_LetRec lrbs e)                      = do
    recfun lrbs
    return e

  eval_expr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  eval_expr e
    | is_value e = return e
    | otherwise  = do 
      e' <- eval_step_expr e
      eval_expr e'

  eval_definition :: (MonadError String m, MonadState InterpreterState m) => Definition -> m ()
  eval_definition (D_Let (p, e))    = do
    e' <- eval_expr e
    matches_of_pattern p e'
  eval_definition (D_LetRec lrbs)   = 
    recfun lrbs

  eval_instruction :: (MonadError String m, MonadState InterpreterState m) => Instruction -> m ()
  eval_instruction (IDF df) = 
    eval_definition df
  eval_instruction (IEX ex) = do
    e <- eval_expr ex
    extend_eval_env "it" e

  eval_program :: (MonadError String m, MonadState InterpreterState m) => Program -> m Expr
  eval_program []     = do
    env <- get_eval_env
    case env "it" of
      Nothing -> return Null
      Just ex -> return ex
  eval_program (i:is) = do
    eval_instruction i
    eval_program is
