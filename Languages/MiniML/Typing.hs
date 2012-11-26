{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Typing where
  import Languages.MiniML.Instances
  import Languages.MiniML.Syntax

  import qualified Utils.Classes.Type as TC
  import Utils.Errors
  import Utils.State
  import qualified Utils.TypingEnv as TE
  import Utils.Unification

  import Control.Monad.Error
  import Control.Monad.State

  intType :: TypeExpr
  intType = TE_Constr [] Int

  boolType :: TypeExpr
  boolType = TE_Constr [] Bool

  unitType :: TypeExpr
  unitType = TE_Constr [] Unit

  listType :: TypeExpr -> TypeExpr
  listType a = TE_Constr [a] List

  refType :: TypeExpr -> TypeExpr
  refType a = TE_Constr [a] Ref

  namesDistinct :: [Name] -> Bool
  namesDistinct []     = True
  namesDistinct (x:xs) = namesDistinct' x xs xs where
    namesDistinct' x []     []     = True
    namesDistinct' x []     (y:ys) = namesDistinct' y ys ys
    namesDistinct' x (z:zs) ys
      | x /= z                      = namesDistinct' x zs ys
      | otherwise                   = False

  getNames :: Pattern -> [Name]
  getNames (P_Val x)      = [x]
  getNames (P_Tuple es)   = concatMap getNames es
  getNames (P_Cons p1 p2) = getNames p1 ++ getNames p2
  getNames _              = []

  typeOfConstant :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => Constant -> m TypeExpr
  typeOfConstant (C_Int _) =
    return intType
  typeOfConstant C_False   =
    return boolType
  typeOfConstant C_True    =
    return boolType
  typeOfConstant C_Nil     = do
    a <- newVar
    return $ listType $ TE_Var a
  typeOfConstant C_Unit    =
    return unitType

  typeOfUnaryPrim :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => UnaryPrim -> m TypeExpr
  typeOfUnaryPrim U_Not     =
    return $ TE_Arrow boolType boolType
  typeOfUnaryPrim U_Ref     = do
    a <- newVar
    let v = TE_Var a
    return $ TE_Arrow v $ refType v
  typeOfUnaryPrim U_Deref   = do
    a <- newVar
    let v = TE_Var a
    return $ TE_Arrow (refType v) v
  typeOfUnaryPrim U_I_Minus =
    return $ TE_Arrow intType intType
  typeOfUnaryPrim U_Fst     = do
    a <- newVar
    b <- newVar
    let v1 = TE_Var a
    let v2 = TE_Var b
    return $ TE_Arrow (TE_Tuple [v1, v2]) v1
  typeOfUnaryPrim U_Snd     = do
    a <- newVar
    b <- newVar
    let v1 = TE_Var a
    let v2 = TE_Var b
    return $ TE_Arrow (TE_Tuple [v1, v2]) v2
  typeOfUnaryPrim U_Empty   = do
    a <- newVar
    let v = TE_Var a
    return $ TE_Arrow (listType v) boolType
  typeOfUnaryPrim U_Head    = do
    a <- newVar
    let v = TE_Var a
    return $ TE_Arrow (listType v) v
  typeOfUnaryPrim U_Tail    = do
    a <- newVar
    let v = TE_Var a
    return $ TE_Arrow (listType v) $ listType v

  typeOfBinaryPrim :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => BinaryPrim -> m TypeExpr
  typeOfBinaryPrim B_Eq      = do
    a <- newVar
    let v = TE_Var a
    addSimpleConstraint v
    return $ TE_Arrow v $ TE_Arrow v boolType
  typeOfBinaryPrim B_I_Plus  =
    return $ TE_Arrow intType $ TE_Arrow intType intType
  typeOfBinaryPrim B_I_Minus =
    return $ TE_Arrow intType $ TE_Arrow intType intType
  typeOfBinaryPrim B_I_Mult  =
    return $ TE_Arrow intType $ TE_Arrow intType intType
  typeOfBinaryPrim B_I_Div   =
    return $ TE_Arrow intType $ TE_Arrow intType intType
  typeOfBinaryPrim B_Assign  = do
    a <- newVar
    let v = TE_Var a
    return $ TE_Arrow (refType v) $ TE_Arrow v unitType

  typeAndBindingsOfPattern :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => Pattern -> m (TypeExpr, TE.Env TypeExpr)
  typeAndBindingsOfPattern p
    | namesDistinct $ getNames p = typeAndBindingsOfPattern' p
    | otherwise                  = throwError $ nonDistinctNames p
    where
      typeAndBindingsOfPattern' :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => Pattern -> m (TypeExpr, TE.Env TypeExpr)
      typeAndBindingsOfPattern' (P_Val x)      = do
        a <- newVar
        let t = TE_Var a
        return (t, [(x, t)])
      typeAndBindingsOfPattern' P_Wildcard     = do
        a <- newVar
        return (TE_Var a, [])
      typeAndBindingsOfPattern' (P_Const c)    = do
        t <- typeOfConstant c
        return (t, [])
      typeAndBindingsOfPattern' (P_Tuple ps)   = do
        tsbs <- mapM typeAndBindingsOfPattern ps
        return (TE_Tuple $ map fst tsbs, concatMap snd tsbs)
      typeAndBindingsOfPattern' (P_Cons p1 p2) = do
        (t1, b1) <- typeAndBindingsOfPattern p1
        (t2, b2) <- typeAndBindingsOfPattern p2
        addConstraint t2 $ listType t1
        return (t2, b1 ++ b2)

  addFunctionConstraints :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => [TypeExpr] -> m TypeExpr
  addFunctionConstraints [t]        =
    return t
  addFunctionConstraints (t1:t2:ts) = do
    addConstraint t1 t2
    addFunctionConstraints (t2:ts)

  typeOfFunction :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> [FunBinding] -> m TypeExpr
  typeOfFunction env bs = typeOfFunction' env bs [] where
    typeOfFunction' :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> [FunBinding] -> [TypeExpr] -> m TypeExpr
    typeOfFunction' env [] acc             = addFunctionConstraints acc
    typeOfFunction' env ((p, e, g):bs) acc = do
      (tp, bsp) <- typeAndBindingsOfPattern p
      te        <- typeOfExpr (bsp ++ env) e
      tg        <- typeOfExpr (bsp ++ env) g
      addConstraint tg boolType
      typeOfFunction' env bs ((TE_Arrow tp te):acc)

  typeOfCase :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> [Binding] -> m TypeExpr
  typeOfCase env bs = typeOfCase' env bs [] where
    typeOfCase' :: (MonadError String m, MonadState (InterpreterState TypeExpr) m) => TE.Env TypeExpr -> [Binding] -> [TypeExpr] -> m TypeExpr
    typeOfCase' env []          acc = addFunctionConstraints acc
    typeOfCase' env ((p, e):bs) acc = do
      (tp, bsp) <- typeAndBindingsOfPattern p
      te        <- typeOfExpr (bsp ++ env) e
      typeOfCase' env bs ((TE_Arrow tp te):acc)

  extendEnv :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> [Binding] -> m (TE.Env TypeExpr)
  extendEnv env []          = return env
  extendEnv env ((p, e):bs) = do
    te         <- typeOfExpr env e
    (tp, env') <- typeAndBindingsOfPattern p
    addConstraint tp te
    s          <- unify
    extendEnv (map (\(n, t) -> (n, t `TC.applySubst` s)) env' ++ env) bs

  recursiveExtend :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> [LetRecBinding] -> m (TE.Env TypeExpr)
  recursiveExtend env bs = recursiveExtend' env (map fst bs) bs where
    recursiveExtend' :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> [Name] -> [LetRecBinding] -> m (TE.Env TypeExpr)
    recursiveExtend' env []     []          = return env
    recursiveExtend' env []     ((n, e):bs) = do
      t1 <- typeOfExpr env e
      t2 <- n `TE.get` env
      addConstraint t1 t2
      s  <- unify
      recursiveExtend' (env `TE.extend` (n, t1 `TC.applySubst` s)) [] bs
    recursiveExtend' env (n:ns) bs          = do
      a <- newVar
      let v = TE_Var a
      recursiveExtend' (env `TE.extend` (n, v)) ns bs

  typeOfExpr :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> Expr -> m TypeExpr
  typeOfExpr env (E_UPrim up)     =
    typeOfUnaryPrim up
  typeOfExpr env (E_BPrim bp)     =
    typeOfBinaryPrim bp
  typeOfExpr env (E_Val n)        =
    n `TE.get` env
  typeOfExpr env (E_Const c)      =
    typeOfConstant c
  typeOfExpr env (E_Apply e1 e2)  = do
    t1 <- typeOfExpr env e1
    t2 <- typeOfExpr env e2
    a  <- newVar
    let v = TE_Var a
    addConstraint t1 $ TE_Arrow t2 v
    return v
  typeOfExpr env (E_Cons e1 e2)   = do
    t1 <- typeOfExpr env e1
    t2 <- typeOfExpr env e2
    addConstraint t2 $ listType t1
    return t2
  typeOfExpr env (E_Tuple es)     = do
    ts <- mapM (typeOfExpr env) es
    return $ TE_Tuple ts
  typeOfExpr env (E_And e1 e2)    = do
    t1 <- typeOfExpr env e1
    t2 <- typeOfExpr env e2
    addConstraint t1 boolType
    addConstraint t2 boolType
    return boolType
  typeOfExpr env (E_Or e1 e2)     = do
    t1 <- typeOfExpr env e1
    t2 <- typeOfExpr env e2
    addConstraint t1 boolType
    addConstraint t2 boolType
    return boolType
  typeOfExpr env (E_ITE e1 e2 e3) = do
    t1 <- typeOfExpr env e1
    t2 <- typeOfExpr env e2
    t3 <- typeOfExpr env e3
    addConstraint t1 boolType
    addConstraint t2 t3
    return t2
  typeOfExpr env (E_Seq e1 e2)    = do
    t1 <- typeOfExpr env e1
    t2 <- typeOfExpr env e2
    addConstraint t1 unitType
    return t2
  typeOfExpr env (E_Function bs)  =
    typeOfFunction env bs
  typeOfExpr env (E_Case e bs)    = do
    t1                <- typeOfExpr env e
    (TE_Arrow t1' t2) <- typeOfCase env bs
    addConstraint t1 t1'
    return t2
  typeOfExpr env (E_Let bs e)     = do
    env' <- extendEnv env bs
    typeOfExpr env' e
  typeOfExpr env (E_LetRec rbs e) = do
    env' <- recursiveExtend env rbs
    typeOfExpr env' e
  typeOfExpr env E_MatchFailure   = do
    a <- newVar
    return $ TE_Var a
  typeOfExpr env (E_FatBar e1 e2) = do
    t1 <- typeOfExpr env e1
    t2 <- typeOfExpr env e2
    addConstraint t1 t2
    return t2

  typeOfDefinition :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> Definition -> m (TE.Env TypeExpr)
  typeOfDefinition env (D_Let bs)    = extendEnv env bs
  typeOfDefinition env (D_LetRec bs) = recursiveExtend env bs

  typeOfInstr :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> Instruction -> m (Maybe TypeExpr, TE.Env TypeExpr, Integer)
  typeOfInstr env (IDF df) = do
    env' <- typeOfDefinition env df
    st   <- get
    return (Nothing, env', variableCounter st + 1)
  typeOfInstr env (IEX ex) = do
    t  <- typeOfExpr env ex
    s  <- unify
    let tp = t `TC.applySubst` s
    st <- get
    return $ (Just tp, env `TE.extend` ("it", tp), variableCounter st + 1)

  typeOfProg :: (MonadState (InterpreterState TypeExpr) m, MonadError String m) => TE.Env TypeExpr -> Program -> m (Maybe TypeExpr, TE.Env TypeExpr, Integer)
  typeOfProg env []     = do
    st <- get
    tp <- do {
      a <- "it" `TE.get` env;
      return . Just $ a
    } `catchError` (\_ -> return Nothing)
    return (tp, env, variableCounter st + 1)
  typeOfProg env (i:is) = do
    (_, env', _) <- typeOfInstr env i
    typeOfProg env' is

  typeOfExpression :: Integer -> TE.Env TypeExpr -> Expr -> Either String TypeExpr
  typeOfExpression n env e = fst $ runState (runErrorT (do { t <- typeOfExpr env e; s <- unify; return $ t `TC.applySubst` s})) emptyState {variableCounter = n}

  typeOfInstruction :: Integer -> TE.Env TypeExpr -> Instruction -> Either String (Maybe TypeExpr, TE.Env TypeExpr, Integer)
  typeOfInstruction n env instr = fst $ runState (runErrorT (typeOfInstr env instr)) emptyState {variableCounter = n}

  typeOfProgram :: Integer -> TE.Env TypeExpr -> Program -> Either String (Maybe TypeExpr, TE.Env TypeExpr, Integer)
  typeOfProgram n env p = fst $ runState (runErrorT (typeOfProg env p)) emptyState {variableCounter = n}
