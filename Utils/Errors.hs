module Utils.Errors
  (too_many_arguments,
   invalid,
   unbound_variable,
   non_distinct_names,
   cannot_compare,
   cannot_unify,
   division_by_0,
   match_failure,
   typing_error,
   typing_error_file,
   parse_error,
   parse_error_file,
   eval_error,
   eval_error_file,
   memory_full,
   head_of_nil,
   tail_of_nil
  ) where
  memory_full = "Memory full"

  too_many_arguments what e1 e2 = what ++ " " ++ show e1 ++ " is applied to to many arguments in: " ++ show e2

  invalid what expr expected (subexpr, actual) = "Invalid " ++ show what ++ " in: " ++ show expr ++
                                                  "\n\texpected: " ++ show expected ++
                                                  "\n\tgot: " ++ show subexpr ++ 
                                                  "\n\tof " ++ show what ++ ": " ++ show actual

  unbound_variable v = "Unbound variable: " ++ show v

  non_distinct_names expr = "Non distinct names in: " ++ show expr

  cannot_compare te = "Cannot compare expressions of non simple type: " ++ show te

  cannot_unify (te1, te2) = "Cannot unify: " ++ show te1 ++ "\n\twith: " ++ show te2

  division_by_0 = "Division by 0"

  match_failure = "Match failure"

  head_of_nil = "head function called on empty list"

  tail_of_nil = "tail function called on empty list"

  typing_error err ex = Utils.Errors.error "Typing" err "expression" (show ex)

  typing_error_file err file = Utils.Errors.error "Typing" err "file" file

  parse_error err ex = Utils.Errors.error "Parse" (show err) "expression" (show ex)

  parse_error_file err file = Utils.Errors.error "Parse" (show err) "file" file

  eval_error err ex = Utils.Errors.error "Evaluation" err "expression" (show ex)

  eval_error_file err file = Utils.Errors.error "Evaluation" err "file" file

  error tp err wh w = tp ++ " error:\n" ++ err ++ "\nin " ++ wh ++ "\n\t" ++ w
