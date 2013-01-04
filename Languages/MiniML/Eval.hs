{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Eval where
  import Languages.MiniML.Instances
  import Languages.MiniML.Syntax

  import Utils.Classes.Clojure
  import Utils.EvalEnv
  import Utils.Errors
  import Utils.Memory

  import Control.Monad.Error

  evalConstant :: MonadError String m => Constant -> m Value
  evalConstant (C_Int n) = return $ V_Int n
  evalConstant C_False   = return $ V_Bool False
  evalConstant C_True    = return $ V_Bool True
  evalConstant C_Nil     = return $ V_List []
  evalConstant C_Unit    = return $ V_Unit

  matches :: Value -> Pattern -> Bool
  matches _              (P_Val _)           = True
  matches _              P_Wildcard          = True
  matches (V_Int m)      (P_Const (C_Int n)) = n == m
  matches _              (P_Const (C_Int n)) = False
  matches (V_Bool False) (P_Const C_False)   = True
  matches _              (P_Const C_False)   = False
  matches (V_Bool True)  (P_Const C_True)    = True
  matches _              (P_Const C_True)    = False
  matches (V_List ls)    (P_Const C_Nil)     = null ls
  matches _              (P_Const C_Nil)     = False
  matches V_Unit         (P_Const C_Unit)    = True
  matches _              (P_Const C_Unit)    = False
  matches (V_Tuple vs)   (P_Tuple ps)        = length ps == length vs && and (zipWith matches vs ps)
  matches _              (P_Tuple ps)        = False
  matches (V_List (h:t)) (P_Cons p1 p2)      = matches h p1 && matches (V_List t) p2
  matches _              (P_Cons p1 p2)      = False

  matchesOfPattern :: MonadError String m => Pattern -> Value -> m [(Name, Value)]
  matchesOfPattern (P_Val x)           v              =
    return [(x, v)]
  matchesOfPattern P_Wildcard          _              =
    return []
  matchesOfPattern (P_Const C_True)    (V_Bool True)  =
    return []
  matchesOfPattern (P_Const C_False)   (V_Bool False) =
    return []
  matchesOfPattern (P_Const C_Nil)     (V_List [])    =
    return []
  matchesOfPattern (P_Const C_Unit)    V_Unit         =
    return []
  matchesOfPattern (P_Const (C_Int n)) (V_Int m)
    | n == m                                          =
      return []
  matchesOfPattern (P_Cons p1 p2)      (V_List (h:t)) = do
    hdm <- matchesOfPattern p1 h
    tlm <- matchesOfPattern p2 $ V_List t
    return $ hdm ++ tlm
  matchesOfPattern (P_Tuple ps)        (V_Tuple vs)
    | length ps == length vs                          = do
      bs <- zipWithM matchesOfPattern ps vs
      return $ concat bs

  extendEnv :: MonadError String m => Memory Value -> Env Value Expr -> [Binding] -> m (Env Value Expr, Memory Value)
  extendEnv mem env []          = return (env, mem)
  extendEnv mem env ((p, e):bs) = do
    (v, mem') <- evalExpression mem env e
    pbs       <- matchesOfPattern p v
    extendEnv mem' (env `extendMany` pbs) bs

  findMatchingClauseAndEnv :: MonadError String m => Env Value Expr -> Value -> [Binding] -> m (Expr, Env Value Expr)
  findMatchingClauseAndEnv env v []          =
    return (E_MatchFailure, env)
  findMatchingClauseAndEnv env v ((p, e):bs)
    | v `matches` p                          = do
      pbs <- matchesOfPattern p v
      return (e, env `extendMany` pbs)
    | otherwise                              =
      findMatchingClauseAndEnv env v bs

  findMatchingFunClauseAndEnv :: MonadError String m => Memory Value -> Env Value Expr -> Value -> [FunBinding] -> m (Expr, Memory Value, Env Value Expr)
  findMatchingFunClauseAndEnv mem env v []             =
    return (E_MatchFailure, mem, env)
  findMatchingFunClauseAndEnv mem env v ((p, e, g):bs)
    | v `matches` p                                    = do
      pbs <- matchesOfPattern p v
      let env' = env `extendMany` pbs
      (V_Bool b, mem') <- evalExpression mem env' g
      case b of
        True  -> return (e, mem', env')
        False -> findMatchingFunClauseAndEnv mem env v bs
    | otherwise                                        =
      findMatchingFunClauseAndEnv mem env v bs

  applyBinaryPrim :: MonadError String m => Memory Value -> BinaryPrim -> Value -> Value -> m (Value, Memory Value)
  applyBinaryPrim mem B_Eq      (V_Pointer a) (V_Pointer b) = do
    (v1, mem1) <- applyUnaryPrim mem U_Deref (V_Pointer a)
    (v2, mem2) <- applyUnaryPrim mem1 U_Deref (V_Pointer b)
    applyBinaryPrim mem2 B_Eq v1 v2
  applyBinaryPrim mem B_Eq      v1            v2            =
    return (V_Bool $ v1 == v2, mem)
  applyBinaryPrim mem B_I_Plus  (V_Int n)     (V_Int m)     =
    return (V_Int $ n + m, mem)
  applyBinaryPrim mem B_I_Minus (V_Int n)     (V_Int m)     =
    return (V_Int $ n - m, mem)
  applyBinaryPrim mem B_I_Mult  (V_Int n)     (V_Int m)     =
    return (V_Int $ n * m, mem)
  applyBinaryPrim mem B_I_Div   (V_Int n)     (V_Int 0)     =
    throwError divisionBy0
  applyBinaryPrim mem B_I_Div   (V_Int n)     (V_Int m)     =
    return (V_Int $ n `div` m, mem)
  applyBinaryPrim mem B_Assign  (V_Pointer a) v             =
    return (V_Unit, update mem a v)

  applyUnaryPrim :: MonadError String m => Memory Value -> UnaryPrim -> Value -> m (Value, Memory Value)
  applyUnaryPrim mem U_Not             (V_Bool b)         =
    return (V_Bool $ not b, mem)
  applyUnaryPrim mem U_Ref             v                  = do
    n <- getFreeAddr mem
    return (V_Pointer n, update (clearFreeAddr mem) n v)
  applyUnaryPrim mem U_Deref           (V_Pointer n)      =
    return (mem `at` n, mem)
  applyUnaryPrim mem U_I_Minus         (V_Int n)          =
    return (V_Int $ -1, mem)
  applyUnaryPrim mem U_Fst             (V_Tuple [v1, v2]) =
    return (v1, mem)
  applyUnaryPrim mem U_Snd             (V_Tuple [v1, v2]) =
    return (v2, mem)
  applyUnaryPrim mem U_Empty           (V_List ls)        =
    return (V_Bool $ null ls, mem)
  applyUnaryPrim mem U_Head            (V_List [])        =
    throwError headOfNil
  applyUnaryPrim mem U_Head            (V_List ls)        =
    return (head ls, mem)
  applyUnaryPrim mem U_Tail            (V_List [])        =
    throwError tailOfNil
  applyUnaryPrim mem U_Tail            (V_List ls)        =
    return (V_List $ tail ls, mem)
  applyUnaryPrim mem (U_PartBin bp v1) v2                 =
    applyBinaryPrim mem bp v1 v2

  performApplication :: MonadError String m => Memory Value -> Env Value Expr -> Value -> Value -> m (Value, Memory Value)
  performApplication mem _   (V_UPrim up)   v           =
    applyUnaryPrim mem up v
  performApplication mem env (V_BPrim bp)   v           =
    return (V_Clo env [(P_Val "_clo_", E_Apply (E_UPrim $ U_PartBin bp v) $ E_Val "_clo_", E_Const C_True)], mem)
  performApplication mem _   (V_Clo env bs) v           = do
    (e', mem', env') <- findMatchingFunClauseAndEnv mem env v bs
    evalExpression mem' env' e'
  performApplication _   _   (V_Error s)    _           =
    throwError s
  performApplication _   _   _              (V_Error s) =
    throwError s

  evalTuple :: MonadError String m => Memory Value -> Env Value Expr -> [Expr] -> m (Value, Memory Value)
  evalTuple mem env es = evalTuple' mem env es [] where
    evalTuple' :: MonadError String m => Memory Value -> Env Value Expr -> [Expr] -> [Value] -> m (Value, Memory Value)
    evalTuple' mem env []     vs =
      return (V_Tuple $ reverse vs, mem)
    evalTuple' mem env (e:es) vs = do
      (v, mem') <- evalExpression mem env e
      evalTuple' mem' env es $ v:vs

  evalExpression :: MonadError String m => Memory Value -> Env Value Expr -> Expr -> m (Value, Memory Value)
  evalExpression mem env (E_UPrim up)     =
    return (V_UPrim up, mem)
  evalExpression mem env (E_BPrim bp)     =
    return (V_BPrim bp, mem)
  evalExpression mem env (E_Val x)        = do
    v <- x `get` env
    return (v, mem)
  evalExpression mem env (E_Const c)      = do
    v <- evalConstant c
    return (v, mem)
  evalExpression mem env (E_Apply e1 e2)  = do
    (v2, mem1) <- evalExpression mem env e2
    (v1, mem2) <- evalExpression mem1 env e1
    performApplication mem2 env v1 v2
  evalExpression mem env (E_Cons e1 e2)   = do
    (v1, mem1)        <- evalExpression mem env e1
    (V_List vs, mem2) <- evalExpression mem env e2
    return (V_List $ v1:vs, mem2)
  evalExpression mem env (E_Tuple es)     =
    evalTuple mem env es
  evalExpression mem env (E_And e1 e2)    = do
    (V_Bool b1, mem1) <- evalExpression mem env e1
    (V_Bool b2, mem2) <- evalExpression mem1 env e2
    return (V_Bool $ b1 && b2, mem2)
  evalExpression mem env (E_Or e1 e2)     = do
    (V_Bool b1, mem1) <- evalExpression mem env e1
    (V_Bool b2, mem2) <- evalExpression mem1 env e2
    return (V_Bool $ b1 || b2, mem2)
  evalExpression mem env (E_ITE e1 e2 e3) = do
    (V_Bool b, mem1) <- evalExpression mem env e1
    case b of
      True  -> evalExpression mem1 env e2
      False -> evalExpression mem1 env e3
  evalExpression mem env (E_Case e bs)    = do
    (v, mem')  <- evalExpression mem env e
    (e', env') <- findMatchingClauseAndEnv env v bs
    evalExpression mem' env' e'
  evalExpression mem env (E_Seq e1 e2)    = do
    (V_Unit, mem1) <- evalExpression mem env e1
    evalExpression mem1 env e2
  evalExpression mem env (E_Function bs)  = do
    return (V_Clo env bs, mem)
  evalExpression mem env (E_Let bs e)     = do
    (env', mem') <- extendEnv mem env bs
    evalExpression mem' env' e
  evalExpression mem env (E_LetRec rbs e) = do
    evalExpression mem (env `extendRec` rbs) e
  evalExpression mem env E_MatchFailure   =
    return (V_Error matchFailure, mem)
  evalExpression mem env (E_FatBar e1 e2) = do
    (v1, mem1) <- evalExpression mem env e1
    case v1 of
      V_Error _ -> evalExpression mem1 env e2
      _         -> return (v1, mem1)

  evalDefinition :: MonadError String m => Memory Value -> Env Value Expr -> Definition -> m (Env Value Expr, Memory Value)
  evalDefinition mem env (D_Let bs)    = do
    extendEnv mem env bs
  evalDefinition mem env (D_LetRec bs) =
    return (env `extendRec` bs, mem)

  evalInstruction :: MonadError String m => Memory Value -> Env Value Expr -> Instruction -> m (Maybe Value, Env Value Expr, Memory Value)
  evalInstruction mem env (IDF df) = do
    (env', mem') <- evalDefinition mem env df
    return (Nothing, env', mem')
  evalInstruction mem env (IEX ex) = do
    (v, mem') <- evalExpression mem env ex
    return (Just v, (env `extend` ("it", v)), mem')

  evalProgram :: MonadError String m => Memory Value -> Env Value Expr -> Program -> m (Maybe Value, Env Value Expr, Memory Value)
  evalProgram mem env []     = do
    v <- do {
      a <- "it" `get` env;
      return . Just $ a
    } `catchError` (\_ -> return Nothing)
    return (v, env, mem)
  evalProgram mem env (i:is) = do
    (_, env', mem') <- evalInstruction mem env i
    evalProgram mem' env' is
