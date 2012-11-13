{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.TypingEnv (Env, emptyEnv, get, extend) where
  --import Utils.Classes.Expression
  --import Utils.Classes.Value
  import Utils.Errors

  import Control.Monad.Error

  type Env tp = [(String, tp)]

  emptyEnv :: Env tp
  emptyEnv = []

  get :: MonadError String m => String -> Env tp -> m tp
  get v env =
    case v `lookup` env of
      Nothing -> throwError $ unboundVariable v
      Just tp -> return tp

  extend :: Env tp -> (String, tp) -> Env tp
  extend = flip (:)
