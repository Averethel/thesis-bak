{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.EnrichedLambda.Typing where
  import Languages.EnrichedLambda.Instances
  import Languages.EnrichedLambda.Syntax

  import qualified Utils.Classes.Type as TC
  import Utils.State
  import qualified Utils.TypingEnv as TE
  import Utils.Unification

  import Control.Monad.Error
  import Control.Monad.State

  boolType :: Type
  boolType = T_Defined 0 []

  unitType :: Type
  unitType = T_Defined 1 []

  listType :: Type -> Type
  listType t = T_Defined 2 [t]

  pairType :: Type -> Type -> Type
  pairType a b = T_Defined 3 [a, b]

  tagToType :: MonadState (InterpreterState Type) m => TypeTag -> m Type
  tagToType n
    | n == boolTag = return boolType
    | n == unitTag = return unitType
    | n == listTag = do
      a <- newVar
      return $ listType $ T_Var a
    | n == pairTag = do
      a <- newVar
      b <- newVar
      return $ pairType (T_Var a) $ T_Var b

  constrTagToVars :: MonadState (InterpreterState Type) m => TypeTag -> ConstrTag -> m [Type]
  constrTagToVars n c
    | n == boolTag                 = return []
    | n == unitTag                 = return []
    | n == listTag && c == nilTag  = return []
    | n == listTag && c == consTag = do
      a <- newVar
      b <- newVar
      return [T_Var a, T_Var b]
    | n == pairTag                 = do
      a <- newVar
      b <- newVar
      return [T_Var a, T_Var b]

  typeOfUnaryPrim :: (MonadState (InterpreterState Type) m, MonadError String m) => UnaryPrim -> m Type
  typeOfUnaryPrim U_Not   =
    return $ T_Arrow boolType boolType
  typeOfUnaryPrim U_Ref   = do
    a <- newVar
    let v = T_Var a
    return $ T_Arrow v $ T_Ref v
  typeOfUnaryPrim U_Deref = do
    a <- newVar
    let v = T_Var a
    return $ T_Arrow (T_Ref v) v
  typeOfUnaryPrim U_Head  = do
    a <- newVar
    let v = T_Var a
    return $ T_Arrow (listType v) v
  typeOfUnaryPrim U_Tail  = do
    a <- newVar
    let v = T_Var a
    return $ T_Arrow (listType v) $ listType v
  typeOfUnaryPrim U_Empty = do
    a <- newVar
    let v = T_Var a
    return $ T_Arrow (listType v) boolType
  typeOfUnaryPrim U_Fst   = do
    a <- newVar
    b <- newVar
    let v1 = T_Var a
    let v2 = T_Var b
    return $ T_Arrow (pairType v1 v2) v1
  typeOfUnaryPrim U_Snd   = do
    a <- newVar
    b <- newVar
    let v1 = T_Var a
    let v2 = T_Var b
    return $ T_Arrow (pairType v1 v2) v2

  typeOfBinaryPrim :: (MonadState (InterpreterState Type) m, MonadError String m) => BinaryPrim -> m Type
  typeOfBinaryPrim B_Eq     = do
    a <- newVar
    let v = T_Var a
    addSimpleConstraint v
    return $ T_Arrow v $ T_Arrow v boolType
  typeOfBinaryPrim B_Plus   =
    return $ T_Arrow T_Int $ T_Arrow T_Int T_Int
  typeOfBinaryPrim B_Minus  =
    return $ T_Arrow T_Int $ T_Arrow T_Int T_Int
  typeOfBinaryPrim B_Mult   =
    return $ T_Arrow T_Int $ T_Arrow T_Int T_Int
  typeOfBinaryPrim B_Div    =
    return $ T_Arrow T_Int $ T_Arrow T_Int T_Int
  typeOfBinaryPrim B_Assign = do
    a <- newVar
    let v = T_Var a
    return $ T_Arrow (T_Ref v) $ T_Arrow v unitType
  typeOfBinaryPrim B_And    =
    return $ T_Arrow boolType $ T_Arrow boolType boolType
  typeOfBinaryPrim B_Or     =
    return $ T_Arrow boolType $ T_Arrow boolType boolType

  extendEnv :: (MonadState (InterpreterState Type) m, MonadError String m) => TE.Env Type -> [Binding] -> m (TE.Env Type)
  extendEnv env []          = return env
  extendEnv env ((n, e):bs) = do
    t <- typeOfExpr env e
    extendEnv (env `TE.extend` (n, t)) bs

  recursiveExtend :: (MonadState (InterpreterState Type) m, MonadError String m) => TE.Env Type -> [Binding] -> m (TE.Env Type)
  recursiveExtend env bs = recursiveExtend' env (map fst bs) bs where
    recursiveExtend' :: (MonadState (InterpreterState Type) m, MonadError String m) => TE.Env Type -> [String] -> [Binding] -> m (TE.Env Type)
    recursiveExtend' env []     []          = return env
    recursiveExtend' env []     ((n, e):bs) = do
      t1 <- typeOfExpr env e
      t2 <- n `TE.get` env
      addConstraint t1 t2
      recursiveExtend' env [] bs
    recursiveExtend' env (n:ns) bs          = do
      a <- newVar
      let v = T_Var a
      recursiveExtend' (env `TE.extend` (n, v)) ns bs

  typeOfClause :: (MonadState (InterpreterState Type) m, MonadError String m) => TE.Env Type -> Clause -> m Type
  typeOfClause env (tp, cs, vs, e) = do
    t1  <- tagToType tp
    tvs <- constrTagToVars tp cs
    t2  <- typeOfExpr ((zip vs tvs) ++ env) e
    return $ T_Arrow t1 t2

  typeOfCase :: (MonadState (InterpreterState Type) m, MonadError String m) => TE.Env Type -> [Clause] -> m Type
  typeOfCase env cs = do
    a  <- newVar
    let v = T_Var a
    ts <- mapM (typeOfClause env) cs
    addConstraints $  map (\x -> (v, x)) ts
    return v

  typeOfExpr :: (MonadState (InterpreterState Type) m, MonadError String m) => TE.Env Type -> Expr -> m Type
  typeOfExpr env (E_UPrim up)     =
    typeOfUnaryPrim up
  typeOfExpr env (E_BPrim bp)     =
    typeOfBinaryPrim bp
  typeOfExpr env (E_Val n)        =
    n `TE.get` env
  typeOfExpr env (E_Num _)        =
    return $ T_Int
  typeOfExpr env (E_Constr 0 0 0) =
    return $ boolType
  typeOfExpr env (E_Constr 0 1 0) =
    return $ boolType
  typeOfExpr env (E_Constr 1 0 0) =
    return $ unitType
  typeOfExpr env (E_Constr 2 0 0) = do
    v <- newVar
    return $ listType $ T_Var v
  typeOfExpr env (E_Constr 2 0 2) = do
    a <- newVar
    let v = T_Var a
    return $ T_Arrow v $ T_Arrow (listType v) $ listType v
  typeOfExpr env (E_Constr 3 0 2) = do
    a <- newVar
    b <- newVar
    let v1 = T_Var a
    let v2 = T_Var b
    return $ T_Arrow v1 $ T_Arrow v2 $ pairType v1 v2
  typeOfExpr env (E_Seq e1 e2)    = do
    t1 <- typeOfExpr env e1
    addConstraint t1 unitType
    typeOfExpr env e2
  typeOfExpr env (E_Apply e1 e2)  = do
    t1 <- typeOfExpr env e1
    t2 <- typeOfExpr env e2
    a <- newVar
    let v = T_Var a
    addConstraint t1 $ T_Arrow t2 v
    return v
  typeOfExpr env (E_Rescue e1 e2) = do
    t1 <- typeOfExpr env e1
    t2 <- typeOfExpr env e2
    addConstraint t1 t2
    return t1
  typeOfExpr env (E_Let bs e)     = do
    env' <- extendEnv env bs
    typeOfExpr env' e
  typeOfExpr env (E_LetRec bs e)  = do
    env' <- recursiveExtend env bs
    typeOfExpr env' e
  typeOfExpr env (E_Case e cs)    = do
    t               <- typeOfExpr env e
    (T_Arrow t1 t2) <- typeOfCase env cs
    addConstraint t t1
    return t2
  typeOfExpr env (E_Function n e) = do
    a <- newVar
    let v = T_Var a
    t <- typeOfExpr (env `TE.extend` (n, v)) e
    return $ T_Arrow v t
  typeOfExpr env E_MatchFailure   = do
    a <- newVar
    return $ T_Var a

  typeOfDefinition :: (MonadState (InterpreterState Type) m, MonadError String m) => TE.Env Type -> Definition -> m (TE.Env Type)
  typeOfDefinition env (D_Let bs)    = extendEnv env bs
  typeOfDefinition env (D_LetRec bs) = recursiveExtend env bs

  typeOfInstr :: (MonadState (InterpreterState Type) m, MonadError String m) => TE.Env Type -> Instruction -> m (Maybe Type, TE.Env Type)
  typeOfInstr env (IDF df) = do
    env' <- typeOfDefinition env df
    return (Nothing, env')
  typeOfInstr env (IEX ex) = do
    t <- typeOfExpr env ex
    s <- unify
    return $ (Just $ t `TC.applySubst` s, map (\(n, t) -> (n, t `TC.applySubst` s)) env)

  typeOfProg :: (MonadState (InterpreterState Type) m, MonadError String m) => TE.Env Type -> Program -> m (Type, TE.Env Type)
  typeOfProg env ([], e)     = do
    t <- typeOfExpr env e
    s <- unify
    return $ (t `TC.applySubst` s, map (\(n, t) -> (n, t `TC.applySubst` s)) env)
  typeOfProg env ((d:ds), e) = do
    env' <- typeOfDefinition env d
    typeOfProg env' (ds, e)

  typeOfExpression :: TE.Env Type -> Expr -> Either String Type
  typeOfExpression env e = fst $ runState (runErrorT (do { t <- typeOfExpr env e; s <- unify; return $ t `TC.applySubst` s})) emptyState

  typeOfInstruction :: TE.Env Type -> Instruction -> Either String (Maybe Type, TE.Env Type)
  typeOfInstruction env instr = fst $ runState (runErrorT (typeOfInstr env instr)) emptyState

  typeOfProgram :: TE.Env Type -> Program -> Either String (Type, TE.Env Type)
  typeOfProgram env p = fst $ runState (runErrorT (typeOfProg env p)) emptyState
