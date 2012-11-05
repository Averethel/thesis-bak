{-# LANGUAGE
  FlexibleContexts,
  TypeSynonymInstances,
  FlexibleInstances
  #-}

module Languages.MiniML.State where
  import Utils.Errors

  import Languages.MiniML.Syntax
  
  import Control.Monad.Error
  import Control.Monad.State
  import Data.Array

  type TypingEnv = ValueName -> Maybe TypeExpr

  instance Show TypingEnv where
    show _ = "TypingEnv"

  type EvalEnv = ValueName -> Maybe Expr

  instance Show EvalEnv where
    show _ = "EvalEnv"

  type Subst = TypeExpr -> TypeExpr
  
  instance Show Subst where
    show _ = "Subst"

  data Memory = M {
                    mem      :: Array Integer Expr,
                    freeCell :: [Integer],
                    used     :: Integer
                  }

  emptyMem :: Integer -> Memory
  emptyMem n = M {
                    mem      = listArray (1,n) $ replicate (fromInteger n) Null,
                    freeCell = [1..n],
                    used     = n
                  }

  getFreeAddr :: MonadError String m => Memory -> m Integer
  getFreeAddr mem = do
    case freeCell mem of
      []  -> throwError $ memoryFull
      f:_ -> return f
  
  clearFreeAddr :: Memory -> Memory
  clearFreeAddr m = m {freeCell = tail . freeCell $ m}
  
  update :: Memory -> Integer -> Expr -> Memory
  update m addr e = m {mem = (mem m)//[(addr, e)]}
  
  at :: Memory -> Integer -> Expr
  m `at` addr = (mem m) ! addr
  
  instance Show Memory where
    show _ = "Memory"

  data InterpreterState = S {
                              typingEnv          :: TypingEnv,
                              variableCounter    :: Integer,
                              constraints         :: [(TypeExpr, TypeExpr)],
                              simpleConstraints  :: [TypeExpr],
                              subst               :: Subst,
                              evalEnv            :: EvalEnv,
                              memory              :: Memory
                            }

  emptyState :: InterpreterState
  emptyState = S { 
                    typingEnv         = \_ -> Nothing,
                    variableCounter   = 0,
                    constraints        = [],
                    simpleConstraints = [],
                    subst              = id,
                    evalEnv           = \_ -> Nothing,
                    memory             = emptyMem 200
                  }

  getTypingEnv :: MonadState InterpreterState m => m TypingEnv
  getTypingEnv = do
    s <- get
    return $ typingEnv s
  
  extendTypingEnv :: MonadState InterpreterState m => String -> TypeExpr -> m ()
  extendTypingEnv v t = do
    s <- get
    put $ s {typingEnv = \x -> if x == v then Just t else (typingEnv s) x }
    return ()
  
  resetTypingEnv :: MonadState InterpreterState m => TypingEnv -> m ()
  resetTypingEnv env = do
    s <- get
    put $ s {typingEnv = env}
    return ()
  
  freshTypeVar :: MonadState InterpreterState m => m TypeExpr
  freshTypeVar = do
    s <- get
    let n = variableCounter s
    put $ s {variableCounter = n + 1}
    return $ TE_Var $ "a" ++ show n
  
  addConstraint :: MonadState InterpreterState m => TypeExpr -> TypeExpr -> m ()
  addConstraint t1 t2 = do
    s <- get
    put $ s {constraints = (t1, t2) : constraints s}
    return ()
  
  getConstraint :: MonadState InterpreterState m => m (Maybe (TypeExpr, TypeExpr))
  getConstraint = do
    s <- get
    case constraints s of
      []   -> return Nothing
      c:cs -> do
        put $ s {constraints = cs}
        return $ Just c
  
  addSimpleConstraint :: MonadState InterpreterState m => TypeExpr -> m ()
  addSimpleConstraint tp = do
    s <- get
    put $ s {simpleConstraints = tp : simpleConstraints s}
    return ()
  
  getSimpleConstraints :: MonadState InterpreterState m => m [TypeExpr]
  getSimpleConstraints = do
    s <- get
    return $ simpleConstraints s
  
  getSubst :: MonadState InterpreterState m => m Subst
  getSubst = do
    s <- get
    return $ subst s
  
  composeSubst :: MonadState InterpreterState m => Subst -> m ()
  composeSubst z = do
    s <- get
    put $ s {subst = z . (subst s)}
    return ()
  
  getMemory :: MonadState InterpreterState m => m Memory
  getMemory = do
    s <- get
    return $ memory s
  
  store :: (MonadState InterpreterState m, MonadError String m) => Expr -> m Integer
  store v = do
    s <- get
    let mem = memory s
    addr <- getFreeAddr mem
    let mem' = clearFreeAddr mem
    put $ s {memory = update mem' addr v}
    return addr
  
  storeAt :: (MonadState InterpreterState m, MonadError String m) => Integer -> Expr -> m ()
  storeAt addr v = do
    s <- get
    let mem = memory s
    put $ s {memory = update mem addr v}
    return ()
  
  load :: MonadState InterpreterState m => Integer -> m Expr
  load addr = do
    mem <- getMemory
    return $ mem `at` addr
  
  getEvalEnv :: MonadState InterpreterState m => m EvalEnv
  getEvalEnv = do
    s <- get
    return $ evalEnv s
  
  extendEvalEnv :: MonadState InterpreterState m => String -> Expr -> m ()
  extendEvalEnv v t = do
    s <- get
    put $ s {evalEnv = \x -> if x == v then Just t else (evalEnv s) x }
    return ()

