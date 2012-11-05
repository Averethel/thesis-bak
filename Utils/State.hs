{-# LANGUAGE
  DatatypeContexts,
  FlexibleContexts
  #-}

module Utils.State where
  import Utils.LanguageClass

  import Control.Monad.State

  type Counter            = Integer
  type Constraint t       = (t, t)
  type SimpleConstraint t = t

  data InterpreterState t =
    S {
        variableCounter   :: Counter,
        constraints       :: [Constraint t],
        simpleConstraints :: [SimpleConstraint t]
      }

  emptyState :: Type t => InterpreterState t
  emptyState = S {
                    variableCounter   = 0,
                    constraints       = [],
                    simpleConstraints = []
                  }

  addConstraint :: (Type t, MonadState (InterpreterState t) m) => t -> t -> m ()
  addConstraint t1 t2 = do
    s <- get
    put $ s { constraints = (t1, t2) : constraints s }
    return ()

  addConstraints :: (Type t, MonadState (InterpreterState t) m) => [Constraint t] -> m ()
  addConstraints cs = do
    _ <- mapM (uncurry addConstraint) cs
    return ()

  addSimpleConstraint :: (Type t, MonadState (InterpreterState t) m) => t -> m ()
  addSimpleConstraint t = do
    s <- get
    put $ s { simpleConstraints = t : simpleConstraints s }
    return ()

  getConstraint :: (Type t, MonadState (InterpreterState t) m) => m (Maybe (t, t))
  getConstraint = do
    s <- get
    case constraints s of
      []     -> return Nothing
      (c:cs) -> do
        put $ s { constraints = cs }
        return $ Just c

  getSimpleConstraint :: (Type t, MonadState (InterpreterState t) m) => m (Maybe t)
  getSimpleConstraint = do
    s <- get
    case simpleConstraints s of
      []     -> return Nothing
      (c:cs) -> do
        put $ s { simpleConstraints = cs }
        return $ Just c
