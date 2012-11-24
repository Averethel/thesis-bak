{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.TypingEnv (Env, emptyEnv, get, extend) where
  import Utils.Classes.Language
  import Utils.Errors

  import Control.Monad.Error

  type Env tp = [(String, tp)]

  emptyEnv :: Language n p tp e i v => Env tp
  emptyEnv = []

  get :: (MonadError String m, Language n p tp e i v) => String -> Env tp -> m tp
  get v env =
    case v `lookup` env of
      Nothing -> throwError $ unboundVariable v
      Just tp -> return tp

  extend :: Language n p tp e i v => Env tp -> (String, tp) -> Env tp
  extend = flip (:)
