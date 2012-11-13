{-# LANGUAGE
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeSynonymInstances
  #-}

module EnrichedLambda.Language where
  import Languages.EnrichedLambda.Instances
  import Languages.EnrichedLambda.Parser
  import Languages.EnrichedLambda.Syntax
  import Languages.EnrichedLambda.Typing
  import Languages.EnrichedLambda.Eval

  import qualified Utils.Classes.Language as LC

  instance LC.Language Program Type Expr Instruction Value where
    typeOfExpression  = typeOfExpression
    typeOfProgram     = typeOfProgram
    typeOfInstruction = typeOfInstruction

    evalInstruction = evalInstruction
    evalProgram     = evalProgram

    parseExpression      = parseExpression
    parseInstruction     = parseInstruction
    parseProgramFromFile = parseProgramFromFile
