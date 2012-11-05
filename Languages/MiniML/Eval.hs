{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Eval (evalProgram, evalExpr, evalDefinition) where
  import Utils.Errors

  import Languages.MiniML.State
  import Languages.MiniML.Syntax

  import Control.Monad.Error
  import Control.Monad.State

  isValue :: Expr -> Bool
  isValue (E_UPrim _)             = True
  isValue (E_BPrim _)             = True
  isValue (E_Location _)          = True
  isValue (E_Const _)             = True
  isValue (E_Function _)          = True
  isValue (E_Apply (E_BPrim _) e) = isValue e
  isValue (E_Cons e1 e2)          = isValue e1 && isValue e2
  isValue (E_Tuple es)            = all isValue es
  isValue _                       = False

  recfun :: (MonadState InterpreterState m) => [LetRecBinding] -> m ()
  recfun []          = return ()
  recfun ((v, e):bs) = do
    extendEvalEnv v e
    recfun bs

  matches :: Expr -> Pattern -> Bool
  e              `matches` (P_Val x)      = True
  e              `matches` P_Wildcard     = True
  (E_Const c1)   `matches` (P_Const c2)   = c1 == c2
  (E_Tuple es)   `matches` (P_Tuple ps)
    | length es == length ps              = and $ zipWith matches es ps
  (E_Cons e1 e2) `matches` (P_Cons p1 p2) = e1 `matches` p1 && e2 `matches` p2
  _              `matches` _              = False

  matchesOfPattern :: (MonadError String m, MonadState InterpreterState m) => Pattern -> Expr -> m ()
  matchesOfPattern (P_Val x)        e                =
    extendEvalEnv x e
  matchesOfPattern P_Wildcard       _                =
    return ()
  matchesOfPattern (P_Const _)      (E_Const _)      =
    return ()
  matchesOfPattern (P_Tuple [])     (E_Tuple [])     =
    return ()
  matchesOfPattern (P_Tuple (p:ps)) (E_Tuple (e:es)) = do
    matchesOfPattern p e
    matchesOfPattern (P_Tuple ps) $ E_Tuple es
  matchesOfPattern (P_Cons p1 p2)   (E_Cons e1 e2)   = do
    matchesOfPattern p1 e1
    matchesOfPattern p2 e2

  evalUnaryPrimitive :: (MonadError String m, MonadState InterpreterState m) => UnaryPrim -> Expr -> m Expr
  evalUnaryPrimitive U_Not     (E_Const C_False)   =
    return $ E_Const C_True
  evalUnaryPrimitive U_Not     (E_Const C_True)    =
    return $ E_Const C_False
  evalUnaryPrimitive U_Ref     e                   = do
    addr <- store e
    return $ E_Location addr
  evalUnaryPrimitive U_Deref   (E_Location addr)   =
    load addr
  evalUnaryPrimitive U_I_Minus (E_Const (C_Int n)) =
    return $ E_Const $ C_Int $ 0 - n
  evalUnaryPrimitive U_Fst     (E_Tuple [v1, _])   =
    return v1
  evalUnaryPrimitive U_Snd     (E_Tuple [_,  v2])  =
    return v2
  evalUnaryPrimitive U_Empty   (E_Const C_Nil)     =
    return $ E_Const C_True
  evalUnaryPrimitive U_Empty   (E_Cons _ _)        =
    return $ E_Const C_False
  evalUnaryPrimitive U_Head    (E_Const C_Nil)     =
    throwError $ headOfNil
  evalUnaryPrimitive U_Head    (E_Cons v1 _)       =
    return v1
  evalUnaryPrimitive U_Tail    (E_Const C_Nil)     =
    throwError $ tailOfNil
  evalUnaryPrimitive U_Tail    (E_Cons _ v2)       =
    return v2

  evalBinaryPrimitive :: (MonadError String m, MonadState InterpreterState m) => BinaryPrim -> Expr -> Expr -> m Expr 
  evalBinaryPrimitive B_Eq      (E_Const c1)        (E_Const c2)
    | c1 == c2                                                            = 
      return $ E_Const C_True
    | otherwise                                                           =
      return $ E_Const C_False
  evalBinaryPrimitive B_Eq      e1@(E_Location _)   e2@(E_Location _)   =
    return $ E_Apply (E_Apply (E_BPrim B_Eq) $ E_Apply (E_UPrim U_Deref) e1) $ E_Apply (E_UPrim U_Deref) e2
  evalBinaryPrimitive B_Eq      (E_Tuple [])        (E_Tuple [])        =
    return $ E_Const C_True
  evalBinaryPrimitive B_Eq      (E_Tuple [])        (E_Tuple _)         =
    return $ E_Const C_False
  evalBinaryPrimitive B_Eq      (E_Tuple _)         (E_Tuple [])        =
    return $ E_Const C_False
  evalBinaryPrimitive B_Eq      (E_Tuple (e1:es1))  (E_Tuple (e2:es2))  =
    return $ E_And (E_Apply (E_Apply (E_BPrim B_Eq) e1) e2) $ E_Apply (E_Apply (E_BPrim B_Eq) $ E_Tuple es1) $ E_Tuple es2
  evalBinaryPrimitive B_Eq      (E_Const C_Nil)     (E_Cons _ _)        =
    return $ E_Const C_False
  evalBinaryPrimitive B_Eq      (E_Cons _ _)        (E_Const C_Nil)     =
    return $ E_Const C_False
  evalBinaryPrimitive B_Eq      (E_Cons e1 e2)      (E_Cons e3 e4)      =
    return $ E_And (E_Apply (E_Apply (E_BPrim B_Eq) e1) e3) $ E_Apply (E_Apply (E_BPrim B_Eq) e2) e4
  evalBinaryPrimitive B_I_Plus  (E_Const (C_Int n)) (E_Const (C_Int m)) =
    return $ E_Const $ C_Int $ n + m
  evalBinaryPrimitive B_I_Minus (E_Const (C_Int n)) (E_Const (C_Int m)) =
    return $ E_Const $ C_Int $ n - m
  evalBinaryPrimitive B_I_Mult  (E_Const (C_Int n)) (E_Const (C_Int m)) =
    return $ E_Const $ C_Int $ n * m
  evalBinaryPrimitive B_I_Div   _                   (E_Const (C_Int 0)) =
    throwError divisionBy0
  evalBinaryPrimitive B_I_Div   (E_Const (C_Int n)) (E_Const (C_Int m)) =
    return $ E_Const $ C_Int $ n `div` m
  evalBinaryPrimitive B_Assign  (E_Location addr)   e                   = do
    storeAt addr e
    return $ E_Const C_Unit

  evalFunction :: (MonadError String m, MonadState InterpreterState m) => [FunBinding] -> Expr -> m Expr
  evalFunction []           _  =
    throwError matchFailure
  evalFunction ((p, e2, g):bs) e1
    | e1 `matches` p            = do
      matchesOfPattern p e1
      eg <- evalExpr g
      case g of
        E_Const C_True  -> return e2
        E_Const C_False -> evalFunction bs e1
    | otherwise                 =
      evalFunction bs e1

  evalCase :: (MonadError String m, MonadState InterpreterState m) => [Binding] -> Expr -> m Expr
  evalCase []           _  =
    throwError matchFailure
  evalCase ((p, e2):bs) e1
    | e1 `matches` p            = do
      matchesOfPattern p e1
      return e2
    | otherwise                 =
      evalCase bs e1

  evalStepTuple :: (MonadError String m, MonadState InterpreterState m) => [Expr] -> [Expr] -> m Expr
  evalStepTuple []     acc = 
    return $ E_Tuple $ reverse acc
  evalStepTuple (e:es) acc
    | isValue e             =
      evalStepTuple es (e:acc)
    | not . isValue $ e     = do
      e' <- evalStepExpr e
      return $ E_Tuple $ (reverse acc) ++ e' : es

  evalStepExpr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  evalStepExpr (E_Val vn)                             = do
    env <- getEvalEnv
    case env vn of
      Nothing -> throwError $ unboundVariable vn
      Just e  -> return e
  evalStepExpr (E_Cons e1 e2)
    | isValue e1 && (not . isValue $ e2)              = do
      e2' <- evalStepExpr e2
      return $ E_Cons e1 e2'
    | not . isValue $ e1                               = do
      e1' <- evalStepExpr e1
      return $ E_Cons e1' e2
  evalStepExpr (E_And e1 e2)
    | not . isValue $ e1                               = do
      e1' <- evalStepExpr e1
      return $ E_And e1' e2
  evalStepExpr (E_And (E_Const C_False) _)            =
    return $ E_Const C_False
  evalStepExpr (E_And (E_Const C_True) e2)            =
    return e2
  evalStepExpr (E_Or e1 e2)
    | not . isValue $ e1                               = do
      e1' <- evalStepExpr e1
      return $ E_Or e1' e2
  evalStepExpr (E_Or (E_Const C_True) _)              =
    return $ E_Const C_True
  evalStepExpr (E_Or (E_Const C_False) e2)            =
    return e2
  evalStepExpr (E_ITE e1 e2 e3)
    | not . isValue $ e1                               = do
      e1' <- evalStepExpr e1
      return $ E_ITE e1' e2 e3
  evalStepExpr (E_ITE (E_Const C_True) e2 _)          =
    return e2
  evalStepExpr (E_ITE (E_Const C_False) _ e3)         =
    return e3
  evalStepExpr (E_Case e1 bs)                         =
    evalCase bs e1
  evalStepExpr (E_Seq e1 e2)
    | not . isValue $ e1                               = do
      e1' <- evalStepExpr e1
      return $ E_Seq e1' e2
  evalStepExpr (E_Seq (E_Const C_Unit) e2)            = 
    return e2
  evalStepExpr (E_Apply e1 e2)
    | isValue e1 && (not . isValue $ e2)              = do
      e2' <- evalStepExpr e2
      return $ E_Apply e1 e2'
    | not . isValue $ e1                               = do
      e1' <- evalStepExpr e1
      return $ E_Apply e1' e2 
  evalStepExpr (E_Apply (E_UPrim up) e1)              = 
    evalUnaryPrimitive up e1
  evalStepExpr (E_Apply (E_Apply (E_BPrim bp) e1) e2) = 
    evalBinaryPrimitive bp e1 e2
  evalStepExpr (E_Apply (E_Function pm) e2)           = 
    evalFunction pm e2
  evalStepExpr (E_Tuple es)                           =
    evalStepTuple es []
  evalStepExpr (E_Let [] e2)                          =
    return e2
  evalStepExpr (E_Let ((p, e1):bs) e2)
    | not . isValue $ e1                               = do
      e1' <- evalStepExpr e1
      return $ E_Let ((p, e1'):bs) e2
    | isValue e1                                       = do
      matchesOfPattern p e1
      return $ E_Let bs e2
  evalStepExpr (E_LetRec lrbs e)                      = do
    recfun lrbs
    return e
  evalStepExpr E_MatchFailure                         =
    throwError matchFailure
  evalStepExpr (E_FatBar E_MatchFailure e2)           = do
    return e2
  evalStepExpr (E_FatBar e1 e2)
    | not . isValue $ e1                               = do
      e1' <- evalStepExpr e1
      return (E_FatBar e1' e2)
    | isValue e1                                       = do
      return e1

  evalExpr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  evalExpr e
    | isValue e = return e
    | otherwise  = do 
      e' <- evalStepExpr e
      evalExpr e'

  evalDefinition :: (MonadError String m, MonadState InterpreterState m) => Definition -> m ()
  evalDefinition (D_Let [])          =
    return ()
  evalDefinition (D_Let ((p, e):bs)) = do
    e' <- evalExpr e
    matchesOfPattern p e'
    evalDefinition $ D_Let bs
  evalDefinition (D_LetRec lrbs)     = 
    recfun lrbs

  evalInstruction :: (MonadError String m, MonadState InterpreterState m) => Instruction -> m ()
  evalInstruction (IDF df) = 
    evalDefinition df
  evalInstruction (IEX ex) = do
    e <- evalExpr ex
    extendEvalEnv "it" e

  evalProgram :: (MonadError String m, MonadState InterpreterState m) => Program -> m Expr
  evalProgram []     = do
    env <- getEvalEnv
    case env "it" of
      Nothing -> return Null
      Just ex -> return ex
  evalProgram (i:is) = do
    evalInstruction i
    evalProgram is
