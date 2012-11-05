{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Typing (typeOfDefinition, typeOfExpression, typeOfProgram) where
  import Utils.Errors

  import Languages.MiniML.Kinding
  import Languages.MiniML.State
  import Languages.MiniML.Syntax
  import Languages.MiniML.PrettyPrint

  import Control.Monad.Error
  import Control.Monad.State

  namesDistinct :: [ValueName] -> Bool
  namesDistinct []     = True
  namesDistinct (x:xs) = namesDistinct' x xs xs where
    namesDistinct' x []     []     = True
    namesDistinct' x []     (y:ys) = namesDistinct' y ys ys
    namesDistinct' x (z:zs) ys
      | x /= z                      = namesDistinct' x zs ys
      | otherwise                   = False

  getNames :: Pattern -> [ValueName]
  getNames (P_Val x)      = [x]
  getNames (P_Tuple es)   = concatMap getNames es
  getNames (P_Cons p1 p2) = getNames p1 ++ getNames p2
  getNames _              = []

  typeOfConstant :: (MonadState InterpreterState m, MonadError String m) => Constant -> m TypeExpr
  typeOfConstant (C_Int n) = 
    return $ TE_Constr [] Int
  typeOfConstant C_False   =
    return $ TE_Constr [] Bool
  typeOfConstant C_True    = 
    return $ TE_Constr [] Bool
  typeOfConstant C_Nil     = do
    v <- freshTypeVar
    return $ TE_Constr [v] List
  typeOfConstant C_Unit    =
    return $ TE_Constr [] Unit
  
  typeOfUnaryPrimitive :: (MonadState InterpreterState m, MonadError String m) => UnaryPrim -> m TypeExpr
  typeOfUnaryPrimitive U_Not     = 
    return $ TE_Arrow (TE_Constr [] Bool) (TE_Constr [] Bool)
  typeOfUnaryPrimitive U_Ref     = do
    v <- freshTypeVar
    return $ TE_Arrow v (TE_Constr [v] Ref)
  typeOfUnaryPrimitive U_Deref   = do
    v <- freshTypeVar
    return $ TE_Arrow (TE_Constr [v] Ref) v
  typeOfUnaryPrimitive U_I_Minus =
    return $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  typeOfUnaryPrimitive U_Fst     = do
    v1 <- freshTypeVar
    v2 <- freshTypeVar
    return $ TE_Arrow (TE_Tuple [v1, v2]) v1
  typeOfUnaryPrimitive U_Snd     = do
    v1 <- freshTypeVar
    v2 <- freshTypeVar
    return $ TE_Arrow (TE_Tuple [v1, v2]) v2
  typeOfUnaryPrimitive U_Empty   = do
    v <- freshTypeVar
    return $ TE_Arrow (TE_Constr [v] List) (TE_Constr [] Bool)
  typeOfUnaryPrimitive U_Head    = do
    v <- freshTypeVar
    return $ TE_Arrow (TE_Constr [v] List) v
  typeOfUnaryPrimitive U_Tail    = do
    v <- freshTypeVar
    return $ TE_Arrow (TE_Constr [v] List) $ TE_Constr [v] List

  typeOfBinaryPrimitive :: (MonadState InterpreterState m, MonadError String m) => BinaryPrim -> m TypeExpr
  typeOfBinaryPrimitive B_Eq      = do
    v <- freshTypeVar
    addSimpleConstraint v
    return $ TE_Arrow v $ TE_Arrow v (TE_Constr [] Bool)
  typeOfBinaryPrimitive B_I_Plus  = 
    return $ TE_Arrow (TE_Constr [] Int) $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  typeOfBinaryPrimitive B_I_Minus =
    return $ TE_Arrow (TE_Constr [] Int) $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  typeOfBinaryPrimitive B_I_Mult  =
    return $ TE_Arrow (TE_Constr [] Int) $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  typeOfBinaryPrimitive B_I_Div   =
    return $ TE_Arrow (TE_Constr [] Int) $ TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)
  typeOfBinaryPrimitive B_Assign  = do
    v <- freshTypeVar
    return $ TE_Arrow (TE_Constr [v] Ref) $ TE_Arrow v (TE_Constr [] Unit)

  typeAndBindingsOfPattern  :: (MonadState InterpreterState m, MonadError String m) => Pattern -> m TypeExpr
  typeAndBindingsOfPattern p
    | namesDistinct $ getNames p = typeAndBindingsOfPattern' p
    | otherwise                    = throwError $ nonDistinctNames p
    where
      typeAndBindingsOfPattern' :: (MonadState InterpreterState m, MonadError String m) => Pattern -> m TypeExpr
      typeAndBindingsOfPattern' (P_Val x)      = do
        v <- freshTypeVar
        extendTypingEnv x v
        return $ v
      typeAndBindingsOfPattern' P_Wildcard     =
        freshTypeVar
      typeAndBindingsOfPattern' (P_Const c)    =
        typeOfConstant c
      typeAndBindingsOfPattern' (P_Tuple es)   = do
        ts <- mapM typeAndBindingsOfPattern es
        return $ TE_Tuple ts
      typeAndBindingsOfPattern' (P_Cons p1 p2) = do
        t1 <- typeAndBindingsOfPattern' p1
        t2 <- typeAndBindingsOfPattern' p2
        addConstraint t2 $ TE_Constr [t1] List
        return t2

  recfun :: (MonadError String m, MonadState InterpreterState m) => [LetRecBinding] -> m ()
  recfun lrbs = recfun' (map fst lrbs) lrbs [] [] where
    recfun' :: (MonadError String m, MonadState InterpreterState m) => [ValueName] -> [LetRecBinding] -> [TypeExpr] -> [TypeExpr] -> m ()
    recfun' []     []             ts1 ts2 = addBindingsConstraints $ zip ts1 ts2
    recfun' []     ((n, e):lrbs) ts1 ts2 = do
      tp <- typeOfExpression e
      recfun' [] lrbs ts1 (tp:ts2)
    recfun' (n:ns) lrbs           ts1 ts2 = do
      t <- freshTypeVar
      extendTypingEnv n t
      recfun' ns lrbs (t:ts1) ts2
    addBindingsConstraints :: (MonadError String m, MonadState InterpreterState m) => [(TypeExpr, TypeExpr)] -> m ()
    addBindingsConstraints []            = return ()
    addBindingsConstraints ((t1, t2):ts) = do
      addConstraint t1 t2
      addBindingsConstraints ts

  typeOfFunction :: (MonadError String m, MonadState InterpreterState m) => [FunBinding] -> m TypeExpr
  typeOfFunction bs = typeOfFunction' bs [] where
    typeOfFunction' :: (MonadError String m, MonadState InterpreterState m) => [FunBinding] -> [TypeExpr] -> m TypeExpr
    typeOfFunction' []          acc = addFunctionConstraints acc
    typeOfFunction' ((p, e, g):es) acc = do
      env <- getTypingEnv
      t1  <- typeAndBindingsOfPattern p
      t2  <- typeOfExpression e
      tg  <- typeOfExpression g
      addConstraint tg (TE_Constr [] Bool)
      resetTypingEnv env
      typeOfFunction' es ((TE_Arrow t1 t2):acc)
    addFunctionConstraints :: (MonadError String m, MonadState InterpreterState m) => [TypeExpr] -> m TypeExpr
    addFunctionConstraints [t]        = return t
    addFunctionConstraints (t1:t2:ts) = do 
      addConstraint t1 t2
      addFunctionConstraints (t2:ts)

  typeOfCase :: (MonadError String m, MonadState InterpreterState m) => [Binding] -> m TypeExpr
  typeOfCase bs = typeOfCase' bs [] where
    typeOfCase' :: (MonadError String m, MonadState InterpreterState m) => [Binding] -> [TypeExpr] -> m TypeExpr
    typeOfCase' []          acc = addCaseConstraints acc
    typeOfCase' ((p, e):es) acc = do
      env <- getTypingEnv
      t1  <- typeAndBindingsOfPattern p
      t2  <- typeOfExpression e
      resetTypingEnv env
      typeOfCase' es ((TE_Arrow t1 t2):acc)
    addCaseConstraints :: (MonadError String m, MonadState InterpreterState m) => [TypeExpr] -> m TypeExpr
    addCaseConstraints [t]        = return t
    addCaseConstraints (t1:t2:ts) = do 
      addConstraint t1 t2
      addCaseConstraints (t2:ts)


  typeOfBindings :: (MonadError String m, MonadState InterpreterState m) => [Binding] -> m ()
  typeOfBindings []          =
    return ()
  typeOfBindings ((p, e):bs) = do
    tp  <- typeAndBindingsOfPattern p
    te  <- typeOfExpression e
    addConstraint tp te
    typeOfBindings bs

  typeOfExpression :: (MonadError String m, MonadState InterpreterState m) => Expr -> m TypeExpr
  typeOfExpression (E_UPrim up)       =
    typeOfUnaryPrimitive up
  typeOfExpression (E_BPrim bp)       =
    typeOfBinaryPrimitive bp
  typeOfExpression (E_Val v)          = do
    env <- getTypingEnv
    case env v of 
      Nothing -> throwError $ unboundVariable v
      Just t  -> return t
  typeOfExpression (E_Const c)        =
    typeOfConstant c
  typeOfExpression (E_Apply e1 e2)    = do
    t1 <- typeOfExpression e1
    t2 <- typeOfExpression e2
    tv <- freshTypeVar
    addConstraint t1 (TE_Arrow t2 tv)
    return tv
  typeOfExpression (E_Cons e1 e2)     = do
    t1 <- typeOfExpression e1
    t2 <- typeOfExpression e2
    addConstraint t2 (TE_Constr [t1] List)
    return t2
  typeOfExpression (E_Tuple es)       = do
    ts <- mapM typeOfExpression es
    return $ TE_Tuple ts
  typeOfExpression (E_And e1 e2)      = do
    t1 <- typeOfExpression e1
    t2 <- typeOfExpression e2
    addConstraint t1 $ TE_Constr [] Bool
    addConstraint t2 $ TE_Constr [] Bool
    return $ TE_Constr [] Bool
  typeOfExpression (E_Or e1 e2)       = do
    t1 <- typeOfExpression e1
    t2 <- typeOfExpression e2
    addConstraint t1 $ TE_Constr [] Bool
    addConstraint t2 $ TE_Constr [] Bool
    return $ TE_Constr [] Bool
  typeOfExpression (E_ITE e1 e2 e3)   = do
    t1 <- typeOfExpression e1
    t2 <- typeOfExpression e2
    t3 <- typeOfExpression e3
    addConstraint t1 $ TE_Constr [] Bool
    addConstraint t2 t3
    return t3
  typeOfExpression (E_Case e1 bs)     = do
    t1                 <- typeOfExpression e1
    (TE_Arrow t1' t2') <- typeOfCase bs
    addConstraint t1' t1
    return t2'
  typeOfExpression (E_Seq e1 e2)      = do
    t1 <- typeOfExpression e1
    t2 <- typeOfExpression e2
    addConstraint t1 $ TE_Constr [] Unit
    return t2
  typeOfExpression (E_Function bs)    = do
    typeOfFunction bs
  typeOfExpression (E_Let bs e2)      = do
    env <- getTypingEnv
    typeOfBindings bs
    t2  <- typeOfExpression e2
    resetTypingEnv env
    return t2
  typeOfExpression (E_LetRec lrbs e2) = do
    env <- getTypingEnv
    recfun lrbs
    t2 <- typeOfExpression e2
    resetTypingEnv env
    return t2
  typeOfExpression E_MatchFailure     = do
    tv <- freshTypeVar
    return tv
  typeOfExpression (E_FatBar e1 e2)   = do
    t1 <- typeOfExpression e1
    t2 <- typeOfExpression e2
    addConstraint t1 t2
    return t1

  typeOfDefinition :: (MonadError String m, MonadState InterpreterState m) => Definition -> m ()
  typeOfDefinition (D_Let bs)      = typeOfBindings bs
  typeOfDefinition (D_LetRec lrbs) = recfun lrbs

  typeOfInstruction :: (MonadError String m, MonadState InterpreterState m) => Instruction -> m ()
  typeOfInstruction (IDF df) = typeOfDefinition df
  typeOfInstruction (IEX ex) = do
    t <- typeOfExpression ex
    extendTypingEnv "it" t

  typeOfProgram :: (MonadState InterpreterState m, MonadError String m) => Program -> m ()
  typeOfProgram []     = return ()
  typeOfProgram (i:is) = do
    typeOfInstruction i
    typeOfProgram is
