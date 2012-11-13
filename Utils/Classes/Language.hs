{-# LANGUAGE
  FlexibleContexts,
  MultiParamTypeClasses
  #-}

module Utils.Classes.Language where
  import Utils.Classes.Clojure
  import Utils.Classes.Expression
  import Utils.Classes.Instruction
  import Utils.Classes.Program
  import Utils.Classes.Type
  import Utils.Classes.Value
  import qualified Utils.EvalEnv as EE
  import Utils.Memory
  import qualified Utils.TypingEnv as TE

  import Control.Monad.Error
  import Text.Parsec.Error

  class (Clojure v e, Expression e, Instruction i, Program p, Type tp, Value v) => Language p tp e i v where
    typeOfExpression     :: TE.Env tp -> e -> Either String tp
    typeOfInstruction    :: TE.Env tp -> i -> Either String (Maybe tp, TE.Env tp)
    typeOfProgram        :: TE.Env tp -> p -> Either String (tp, TE.Env tp)
    evalInstruction      :: Memory v -> EE.Env v e -> i -> Either String (Maybe v, EE.Env v e, Memory v)
    evalProgram          :: Memory v -> EE.Env v e -> p -> Either String (v, EE.Env v e, Memory v)
    parseExpression      :: String -> Either ParseError e
    parseInstruction     :: String -> Either ParseError i
    parseProgramFromFile :: String -> IO (Either ParseError p)
