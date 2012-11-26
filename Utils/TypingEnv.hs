{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.TypingEnv (Env, emptyEnv, get, extend) where
  import Utils.Classes.Type
  import Utils.Errors

  import Control.Monad.Error

  type Env tp = [(String, tp)]

  emptyEnv :: Type tp => Env tp
  emptyEnv = []

  get :: (MonadError String m, Type tp) => String -> Env tp -> m tp
  get v env =
    case v `lookup` env of
      Nothing -> throwError $ unboundVariable v
      Just tp -> return tp

  extend :: Type tp => Env tp -> (String, tp) -> Env tp
  extend = flip (:)
