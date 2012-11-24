{-# LANGUAGE
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeSynonymInstances
  #-}

module Languages.EnrichedLambda.Language where
  import Languages.EnrichedLambda.Instances
  import Languages.EnrichedLambda.Parser
  import Languages.EnrichedLambda.Syntax
  import Languages.EnrichedLambda.Typing
  import Languages.EnrichedLambda.Eval

  import qualified Utils.Classes.Language as LC

  instance LC.Language LambdaName Program Type Expr Instruction Value where
    typeOfExpression  = typeOfExpression
    typeOfProgram     = typeOfProgram
    typeOfInstruction = typeOfInstruction

    evalInstruction = evalInstruction
    evalProgram     = evalProgram

    parseExpression      _ = parseExpression
    parseInstruction     _ = parseInstruction
    parseProgramFromFile _ = parseProgramFromFile
