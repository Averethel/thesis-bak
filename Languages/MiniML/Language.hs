{-# LANGUAGE
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeSynonymInstances
  #-}

module Languages.MiniML.Language where
  import Languages.MiniML.Instances
  import Languages.MiniML.Parser
  import Languages.MiniML.Syntax
  import Languages.MiniML.Typing
  import Languages.MiniML.Eval

  import qualified Utils.Classes.Language as LC

  instance LC.Language MiniMLName Program TypeExpr Expr Instruction Value where
    typeOfExpression  = typeOfExpression
    typeOfProgram     = typeOfProgram
    typeOfInstruction = typeOfInstruction

    evalInstruction = evalInstruction
    evalProgram     = evalProgram

    parseExpression _      = parseExpression
    parseInstruction _     = parseInstruction
    parseProgramFromFile _ = parseProgramFromFile