{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.EnrichedLambda.Eval where
  import Languages.EnrichedLambda.Instances
  import Languages.EnrichedLambda.Syntax

  import Utils.Classes.Clojure
  import Utils.EvalEnv
  import Utils.Errors
  import Utils.Memory

  import Control.Monad.Error

  extendEnv :: MonadError String m => Memory Value -> Env Value Expr -> [Binding] -> m (Env Value Expr, Memory Value)
  extendEnv mem env []          = return (env, mem)
  extendEnv mem env ((x, e):bs) = do
    (v, mem') <- evalExpression mem env e
    extendEnv mem' (env `extend` (x, v)) bs

  matches :: Value -> (TypeTag, ConstrTag) -> Bool
  (V_Cell tp cs vs) `matches` (tt, ct) = tp == tt && cs == ct
  _                 `matches` _        = False

  clauseExtend :: Env Value Expr -> (Value, [String]) -> Env Value Expr
  clauseExtend env (V_Cell tp cs vs, ns)
    | tp == boolTag && cs == trueTag  && vs == [] && ns == [] = env
    | tp == boolTag && cs == falseTag && vs == [] && ns == [] = env
    | tp == unitTag && cs == unitTagC && vs == [] && ns == [] = env
    | tp == listTag && cs == nilTag   && vs == [] && ns == [] = env
    | tp == listTag && cs == consTag                          =
      case (vs, ns) of
        ([v1, v2], [a, b]) -> env `extend` (b, v2) `extend` (a, v1)
    | tp == pairTag && cs == pairTagC                         =
      case (vs, ns) of
        ([v1, v2], [a, b]) -> env `extend` (b, v2) `extend` (a, v1)

  findMatchingClauseAndEnv :: MonadError String m => Env Value Expr -> Value -> [Clause] -> m (Expr, Env Value Expr)
  findMatchingClauseAndEnv env v []                   =
    return (E_MatchFailure, env)
  findMatchingClauseAndEnv env v ((tp, ct, ns, e):cs)
    | v `matches` (tp, ct)                            = do
      return (e, env `clauseExtend` (v, ns))
    | otherwise                                       =
      findMatchingClauseAndEnv env v cs

  applyUnaryPrim :: MonadError String m => Memory Value -> UnaryPrim -> Value -> m (Value, Memory Value)
  applyUnaryPrim mem U_Not             (V_Cell tp cs vs)
    | tp == boolTag && cs == trueTag  && vs == []        =
      return (V_Cell boolTag falseTag [], mem)
    | tp == boolTag && cs == falseTag && vs == []        =
      return (V_Cell boolTag trueTag [], mem)
  applyUnaryPrim mem U_Ref             v                 = do
    n <- getFreeAddr mem
    return (V_Pointer n, update (clearFreeAddr mem) n v)
  applyUnaryPrim mem U_Deref           (V_Pointer n)     =
    return (mem `at` n, mem)
  applyUnaryPrim mem U_Head            (V_Cell tp cs vs)
    | tp == listTag && cs == nilTag  && vs == []         =
      throwError headOfNil
    | tp == listTag && cs == consTag                     =
      case vs of
        [a, _] -> return (a, mem)
  applyUnaryPrim mem U_Tail            (V_Cell tp cs vs)
    | tp == listTag && cs == nilTag  && vs == []         =
      throwError tailOfNil
    | tp == listTag && cs == consTag                     =
      case vs of
        [_, b] -> return (b, mem)
  applyUnaryPrim mem U_Empty           (V_Cell tp cs vs)
    | tp == listTag && cs == nilTag  && vs == []         =
      return (V_Cell boolTag trueTag [], mem)
    | tp == listTag && cs == consTag                     =
      case vs of
        [_, _] -> return (V_Cell boolTag falseTag [], mem)
  applyUnaryPrim mem U_Fst             (V_Cell tp cs vs)
    | tp == pairTag && cs == pairTagC                    =
      case vs of
        [a, _] -> return (a, mem)
  applyUnaryPrim mem U_Snd             (V_Cell tp cs vs)
    | tp == pairTag && cs == pairTagC                    =
      case vs of
        [_, b] -> return (b, mem)
  applyUnaryPrim mem (U_PartBin bp v1) v2                =
    applyBinaryPrim mem bp v1 v2

  applyBinaryPrim :: MonadError String m => Memory Value -> BinaryPrim -> Value -> Value -> m (Value, Memory Value)
  applyBinaryPrim mem B_Eq     (V_Cell t1 c1 v1) (V_Cell t2 c2 v2)
    | t1 == t2 && c1 == c2 && v1 == v2                                 =
      return (V_Cell boolTag trueTag [], mem)
    | otherwise                                                        =
      return (V_Cell boolTag falseTag [], mem)
  applyBinaryPrim mem B_Eq     (V_Int a)         (V_Int b)
    | a == b                                                           =
      return (V_Cell boolTag trueTag [], mem)
    | otherwise                                                        =
      return (V_Cell boolTag falseTag [], mem)
  applyBinaryPrim mem B_Eq     (V_Pointer a)     (V_Pointer b)         = do
    (v1, mem1) <- applyUnaryPrim mem U_Deref (V_Pointer a)
    (v2, mem2) <- applyUnaryPrim mem U_Deref (V_Pointer b)
    applyBinaryPrim mem2 B_Eq v1 v2
  applyBinaryPrim mem B_Plus   (V_Int n)         (V_Int m)             =
    return (V_Int $ n + m, mem)
  applyBinaryPrim mem B_Minus  (V_Int n)         (V_Int m)             =
    return (V_Int $ n - m, mem)
  applyBinaryPrim mem B_Mult   (V_Int n)         (V_Int m)             =
    return (V_Int $ n * m, mem)
  applyBinaryPrim mem B_Div    (V_Int n)         (V_Int 0)             =
    throwError divisionBy0
  applyBinaryPrim mem B_Div    (V_Int n)         (V_Int m)             =
    return (V_Int $ n `div` m, mem)
  applyBinaryPrim mem B_Assign (V_Pointer a)     v                     =
    return (V_Cell unitTag unitTagC [], update mem a v)
  applyBinaryPrim mem B_And    (V_Cell t1 c1 v1) (V_Cell t2 c2 v2)
    | t1 == boolTag && t2 == boolTag &&
      c1 == trueTag && v1 == [] && v2 == []                            =
        return (V_Cell t2 c2 v2, mem)
    | t1 == boolTag && t2 == boolTag &&
      c1 == falseTag && v1 == [] && v2 == []                           =
        return (V_Cell boolTag falseTag [], mem)
  applyBinaryPrim mem B_Or     (V_Cell t1 c1 v1) (V_Cell t2 c2 v2)
    | t1 == boolTag && t2 == boolTag &&
      c1 == trueTag && v1 == [] && v2 == []                            =
        return (V_Cell boolTag trueTag [], mem)
    | t1 == boolTag && t2 == boolTag &&
      c1 == falseTag && v1 == [] && v2 == []                           =
        return (V_Cell t2 c2 v2, mem)
  applyBinaryPrim mem B_Cons   v                 v2@(V_Cell t' c' v')
    | t' == listTag                                                    =
      return (V_Cell listTag consTag [v, v2], mem)
  applyBinaryPrim mem B_Pair   v1                v2                    =
    return (V_Cell pairTag pairTagC [v1, v2], mem)

  performApplication :: MonadError String m => Memory Value -> Env Value Expr -> Value -> Value -> m (Value, Memory Value)
  performApplication mem env (V_UPrim up) v         =
    applyUnaryPrim mem up v
  performApplication mem env (V_BPrim bp) v         =
    return (V_Clo env "_clo_" $ E_Apply (E_UPrim $ U_PartBin bp v) $ E_Val "_clo_", mem)
  performApplication mem _   (V_Clo env x e) v      =
    evalExpression mem (env `extend` (x, v)) e
  performApplication mem _   (V_Error s) _          =
    throwError s
  performApplication mem _   _          (V_Error s) =
    throwError s

  evalExpression :: MonadError String m => Memory Value -> Env Value Expr -> Expr -> m (Value, Memory Value)
  evalExpression mem env (E_UPrim up)       =
    return (V_UPrim up, mem)
  evalExpression mem env (E_BPrim bp)       =
    return (V_BPrim bp, mem)
  evalExpression mem env (E_Val x)          = do
    v <- x `get` env
    return (v, mem)
  evalExpression mem env (E_Num n)          =
    return (V_Int n, mem)
  evalExpression mem env (E_Constr tp cs a)
    | tp == boolTag && cs == falseTag &&
      a  == 0                               =
        return (V_Cell boolTag falseTag [], mem)
    | tp == boolTag && cs == trueTag &&
      a  == 0                               =
        return (V_Cell boolTag trueTag [], mem)
    | tp == unitTag && cs == unitTagC &&
      a  == 0                               =
        return (V_Cell unitTag unitTagC [], mem)
    | tp == listTag && cs == nilTag &&
      a  == 0                               =
        return (V_Cell listTag nilTag [], mem)
    | tp == listTag && cs == consTag &&
      a  == 2                               =
        return (V_BPrim B_Cons, mem)
    | tp == pairTag && cs == pairTagC &&
      a  == 2                               =
        return (V_BPrim B_Pair, mem)
  evalExpression mem env (E_Seq e1 e2)      = do
    (V_Cell tp cs [], mem1) <- evalExpression mem env e1
    case tp == unitTag && cs == unitTagC of
      True -> evalExpression mem1 env e2
  evalExpression mem env (E_Apply e1 e2)    = do
    (v1, mem1) <- evalExpression mem env e1
    (v2, mem2) <- evalExpression mem1 env e2
    performApplication mem2 env v1 v2
  evalExpression mem env (E_Rescue e1 e2)   = do
    (v1, mem1) <- evalExpression mem env e1
    case v1 of
      V_Error s -> evalExpression mem1 env e2
      _         -> return (v1, mem1)
  evalExpression mem env (E_Let lbs e)      = do
    (env', mem') <- extendEnv mem env lbs
    evalExpression mem' env' e
  evalExpression mem env (E_LetRec lrbs e)  = do
    evalExpression mem (env `extendRec` lrbs) e
  evalExpression mem env (E_Case e cs)      = do
    (v1, mem') <- evalExpression mem env e
    (e', env') <- findMatchingClauseAndEnv env v1 cs
    evalExpression mem' env' e'
  evalExpression mem env (E_Function x e)   =
    return (V_Clo env x e, mem)
  evalExpression mem env E_MatchFailure     =
    return (V_Error matchFailure, mem)

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

  evalProgram :: MonadError String m => Memory Value -> Env Value Expr -> Program -> m (Value, Env Value Expr, Memory Value)
  evalProgram mem env ([],     e) = do
    (v, mem') <- evalExpression mem env e
    return (v, (env `extend` ("it", v)), mem')
  evalProgram mem env ((d:ds), e) = do
    (env', mem') <- evalDefinition mem env d
    evalProgram mem' env' (ds, e)

