{-# LANGUAGE 
  NoMonomorphismRestriction
  #-}

module Languages.EnrichedLambdaLowLevelTypes.Errors where
  unbound_variable = ("Unbound variable: " ++).show
  
  cannot_unify te1 te2 = "Cannot unify: " ++ show te1 ++ "\n\twith: " ++ show te2
  
  cannot_compare = ("Cannot compare expressions of non simple type: " ++).show
  
  memory_full = "Memory full"
  
  division_by_0 = "Division by 0"
  
  match_failure = "Match failure"
  
  parse_error err ex = Languages.EnrichedLambdaLowLevelTypes.Errors.error "Parse" (show err) "expression" (show ex)
  
  typing_error err ex = Languages.EnrichedLambdaLowLevelTypes.Errors.error "Typing" err "expression" (show ex)
  
  eval_error err ex = Languages.EnrichedLambdaLowLevelTypes.Errors.error "Evaluation" err "expression" (show ex)
  
  error tp err wh w = tp ++ " error:\n" ++ err ++ "\nin " ++ wh ++ "\n\t" ++ w
