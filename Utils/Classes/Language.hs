{-# LANGUAGE
  FlexibleContexts,
  MultiParamTypeClasses,
  FunctionalDependencies
  #-}

module Utils.Classes.Language where
  import Utils.Classes.Clojure
  import Utils.Classes.Expression
  import Utils.Classes.Instruction
  import Utils.Classes.LanguageName
  import Utils.Classes.Program
  import Utils.Classes.Type
  import Utils.Classes.Value
  import qualified Utils.EvalEnv as EE
  import Utils.Memory
  import qualified Utils.TypingEnv as TE

  import Control.Monad.Error
  import Text.Parsec.Error

  class (Clojure v e,
         Expression e,
         Instruction i,
         LanguageName n,
         Program p,
         Type tp,
         Value v) =>
          Language n p tp e i v | n  -> p tp e i v,
                                  p  -> n tp e i v,
                                  tp -> n p e i v,
                                  e  -> n p tp i v,
                                  i  -> n p tp e v,
                                  v  -> n p tp e i where
    typeOfExpression     :: Integer -> TE.Env tp -> e -> Either String tp
    typeOfInstruction    :: Integer -> TE.Env tp -> i -> Either String (Maybe tp, TE.Env tp, Integer)
    typeOfProgram        :: Integer -> TE.Env tp -> p -> Either String (Maybe tp, TE.Env tp, Integer)
    evalInstruction      :: Memory v -> EE.Env v e -> i -> Either String (Maybe v, EE.Env v e, Memory v)
    evalProgram          :: Memory v -> EE.Env v e -> p -> Either String (v, EE.Env v e, Memory v)
    parseExpression      :: n -> String -> Either ParseError e
    parseInstruction     :: n -> String -> Either ParseError i
    parseProgramFromFile :: n -> String -> IO (Either ParseError p)
