{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.EvalEnv (Env, emptyEnv, get, extend, extendRec) where
  import Utils.Classes.Clojure
  import Utils.Classes.Expression
  import Utils.Classes.Value
  import Utils.Errors

  import Control.Monad.Error

  import Data.Maybe

  data Env v e =
      Init
    | Simple String v (Env v e)
    | Recursive [(String, e)] (Env v e)
    deriving Eq

  emptyEnv :: Env v e
  emptyEnv = Init

  get :: (Value v, Expression e, Clojure v e) => MonadError String m => String -> Env v e -> m v
  get val Init                     = throwError $ unboundVariable val
  get val (Simple n v e)
    | val == n                     = return v
    | otherwise                    = get val e
  get val e@(Recursive ex e')
    | val `elem` (map fst ex)      = return $ mkClo (fromJust $ val `lookup` ex) e
    | otherwise                    = get val e'

  extend :: (Value v, Expression e, Clojure v e) => Env v e -> (String, v) -> Env v e
  extend env (n, v) = Simple n v env

  extendRec :: (Value v, Expression e, Clojure v e) => Env v e -> [(String, e)] -> Env v e
  extendRec env e = Recursive e env
