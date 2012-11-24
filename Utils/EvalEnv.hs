{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.EvalEnv (Env, emptyEnv, get, extend, extendRec)
where
  import Utils.Classes.Clojure
  import Utils.Classes.Language
  import Utils.Errors

  import Control.Monad.Error

  import Data.Maybe

  data Env v e =
      Init
    | Simple String v (Env v e)
    | Recursive [(String, e)] (Env v e)
    deriving Eq

  emptyEnv :: Language n p tp e i v => Env v e
  emptyEnv = Init

  get :: (MonadError String m, Language n p tp e i v) => String -> Env v e -> m v
  get val Init                     = throwError $ unboundVariable val
  get val (Simple n v e)
    | val == n                     = return v
    | otherwise                    = get val e
  get val e@(Recursive ex e')
    | val `elem` (map fst ex)      = return $ mkClo (fromJust $ val `lookup` ex) e
    | otherwise                    = get val e'

  extend :: Language n p tp e i v => Env v e -> (String, v) -> Env v e
  extend env (n, v) = Simple n v env

  extendRec :: Language n p tp e i v => Env v e -> [(String, e)] -> Env v e
  extendRec env e = Recursive e env
