{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.Unification where
  import Utils.Classes.Type
  import Utils.Errors
  import Utils.State
  import Utils.Subst

  import Control.Monad.Error
  import Control.Monad.State

  checkCompare :: (Type t, MonadError String m, MonadState (InterpreterState t) m) => Subst t -> m ()
  checkCompare subst = do
    c <- getSimpleConstraint
    case c of
      Nothing -> return ()
      Just t  -> do
          let t' = t `applySubst` subst
          case canCompare t' of
            True  -> checkCompare subst
            False -> throwError $ cannotCompare t'

  unify :: (Type t, MonadError String m, MonadState (InterpreterState t) m) => m (Subst t)
  unify = unify' emptySubst where
    unify' :: (Type t, MonadError String m, MonadState (InterpreterState t) m) => Subst t -> m (Subst t)
    unify' s = do
      c <- getConstraint
      case c of
        Nothing     -> do
          checkCompare s
          return s
        Just (a, b) -> do
          let a' = a `applySubst` s
          let b' = b `applySubst` s
          case isVar a' of
            True  -> unify' $ composeSubst (getVar a', b') s
            False -> case isVar b' of
              True  -> unify' $ composeSubst (getVar b', a') s
              False -> case canUnify a' b' of
                True  -> do
                  addConstraints $ newConstraints a' b'
                  unify' s
                False -> throwError $ cannotUnify (a', b')
