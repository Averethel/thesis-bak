module Interpreters.MiniML.Errors where
  too_many_arguments what e1 e2 = what ++ " " ++ show e1 ++ " is applied to to many arguments in: " ++ show e2

  invalid what expr expected (subexpr, actual) = "Invalid " ++ show what ++ " in: " ++ show expr ++
                                                  "\n\texpected: " ++ show expected ++
                                                  "\n\tgot: " ++ show subexpr ++ 
                                                  "\n\tof " ++ show what ++ ": " ++ show actual
