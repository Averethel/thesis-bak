{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.Env where
  import Utils.Errors

  import Control.Monad.Error

  type Env a = [(String, a)]

  emptyEnv :: Env a
  emptyEnv = []

  lookup' :: MonadError String m => String -> Env a -> m a
  lookup' val env =
    case val `lookup` env of
      Nothing -> throwError $ unboundVariable val
      Just v  -> return v

  extend :: Env a -> (String, a) -> Env a
  extend = flip (:)
