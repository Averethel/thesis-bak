module Interpreters.MiniML.Errors
  (too_many_arguments,
   invalid,
   unbound_variable,
   non_distinct_names,
   non_simple_type,
   cannot_unify,
   division_by_0,
   match_failure,
   typing_error,
   typing_error_file,
   parse_error,
   parse_error_file,
   eval_error,
   eval_error_file
  ) where
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

  typing_error err ex = Interpreters.MiniML.Errors.error "Typing" err "expression" (show ex)

  typing_error_file err file = Interpreters.MiniML.Errors.error "Typing" err "file" file

  parse_error err ex = Interpreters.MiniML.Errors.error "Parse" (show err) "expression" (show ex)

  parse_error_file err file = Interpreters.MiniML.Errors.error "Parse" (show err) "file" file

  eval_error err ex = Interpreters.MiniML.Errors.error "Evaluation" err "expression" (show ex)

  eval_error_file err file = Interpreters.MiniML.Errors.error "Evaluation" err "file" file

  error tp err wh w = tp ++ " error:\n" ++ err ++ "\nin " ++ wh ++ "\n\t" ++ w
