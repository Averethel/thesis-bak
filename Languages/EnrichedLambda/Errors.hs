module Languages.EnrichedLambda.Errors where
  unbound_variable v = "Unbound variable: " ++ show v
  
  cannot_unify te1 te2 = "Cannot unify: " ++ show te1 ++ "\n\twith: " ++ show te2
  
  cannot_compare te = "Cannot compare expressions of non simple type: " ++ show te
  
  memory_full = "Memory full"
  
  division_by_0 = "Division by 0"
  
  match_failure = "Match failure"
