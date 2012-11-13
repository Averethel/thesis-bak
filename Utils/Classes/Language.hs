{-# LANGUAGE
  FlexibleContexts,
  MultiParamTypeClasses
  #-}

module Utils.Classes.Language where
  import Utils.Classes.Clojure
  --import Utils.Classes.Definition
  import Utils.Classes.Expression
  import Utils.Classes.Program
  import Utils.Classes.Type
  import Utils.Classes.Value
  import qualified Utils.EvalEnv as EE
  import Utils.Memory
  import qualified Utils.TypingEnv as TE

  import Control.Monad.Error
  import Text.Parsec.Error

  class (Program p, Type tp, Expression e, Value v, Clojure v e) => Language p tp e v where
    typeOf  :: TE.Env tp -> p -> Either String tp
    eval    :: Memory v -> EE.Env v e -> p -> Either String (v, Memory v)
    parser  :: String -> Either ParseError p
