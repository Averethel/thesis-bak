module Utils.Errors
  (tooManyArguments,
   invalid,
   unboundVariable,
   nonDistinctNames,
   cannotCompare,
   cannotUnify,
   divisionBy0,
   matchFailure,
   typingError,
   typingErrorFile,
   parseError,
   parseErrorFile,
   evalError,
   evalErrorFile,
   memoryFull,
   headOfNil,
   tailOfNil
  ) where
  memoryFull = "Memory full"

  tooManyArguments what e1 e2 = what ++ " " ++ show e1 ++ " is applied to to many arguments in: " ++ show e2

  invalid what expr expected (subexpr, actual) = "Invalid " ++ show what ++ " in: " ++ show expr ++
                                                  "\n\texpected: " ++ show expected ++
                                                  "\n\tgot: " ++ show subexpr ++ 
                                                  "\n\tof " ++ show what ++ ": " ++ show actual

  unboundVariable v = "Unbound variable: " ++ show v

  nonDistinctNames expr = "Non distinct names in: " ++ show expr

  cannotCompare te = "Cannot compare expressions of non simple type: " ++ show te

  cannotUnify (te1, te2) = "Cannot unify: " ++ show te1 ++ "\n\twith: " ++ show te2

  divisionBy0 = "Division by 0"

  matchFailure = "Match failure"

  headOfNil = "head function called on empty list"

  tailOfNil = "tail function called on empty list"

  typingError err ex = Utils.Errors.error "Typing" err "expression" (show ex)

  typingErrorFile err file = Utils.Errors.error "Typing" err "file" file

  parseError err ex = Utils.Errors.error "Parse" (show err) "expression" (show ex)

  parseErrorFile err file = Utils.Errors.error "Parse" (show err) "file" file

  evalError err ex = Utils.Errors.error "Evaluation" err "expression" (show ex)

  evalErrorFile err file = Utils.Errors.error "Evaluation" err "file" file

  error tp err wh w = tp ++ " error:\n" ++ err ++ "\nin " ++ wh ++ "\n\t" ++ w
