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