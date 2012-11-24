{-# LANGUAGE
  MultiParamTypeClasses,
  FunctionalDependencies
  #-}

module Utils.Classes.Clojure where
  import Utils.Classes.Expression
  import Utils.Classes.Value
  import {-# SOURCE #-} Utils.EvalEnv

  class (Expression e, Value v) => Clojure v e | e -> v, v -> e where
    mkClo :: e -> Env v e -> v
