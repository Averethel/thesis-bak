module Interpreters.MiniML.Errors where
  too_many_arguments what e1 e2 = what ++ " " ++ show e1 ++ " is applied to to many arguments in: " ++ show e2

  invalid what expr expected (subexpr, actual) = "Invalid " ++ show what ++ " in: " ++ show expr ++
                                                  "\n\texpected: " ++ show expected ++
                                                  "\n\tgot: " ++ show subexpr ++ 
                                                  "\n\tof " ++ show what ++ ": " ++ show actual

  unbound_variable v = "Unbound variable: " ++ show v

  non_distinct_names expr = "Non distinct names in: " ++ show expr

  non_simple_type te = "Cannot compare expressions of non simple type: " ++ show te

  cannot_unify (te1, te2) = "Cannot unify: " ++ show te1 ++ "\n\twith: " ++ show te2

  division_by_0 = "Division by 0"

  match_failure = "Match failure"