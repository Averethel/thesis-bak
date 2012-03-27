module Interpreters.MiniML.Errors where

  unbound_variable v = "Unbound variable: " ++ show v

  unbound_type_variable v = "Unbound type variable: " ++ show v

  invalid_kind expr expected (subexpr, kind) = "Invalid kinding in: " ++ show expr ++
                                                  "\n\texpected expression of kind: " ++ show expected ++
                                                  "\n\tgot expression: " ++ show subexpr ++ 
                                                  "\n\tof kind: " ++ show kind

  invalid_type_constr expr type_constr expected actual = "Invalid kinding in: " ++ show expr ++
                                                         "\n\t" ++ show type_constr ++ " expects " ++ show expected ++ " arguments, got " ++ show actual

  non_distinct_names expr = "Typing error in: " ++ show expr ++
                            "\n\tnon distinct names"

  invalid_type expr expected (subexpr, tp) = "Invalid typing in: " ++ show expr ++
                                             "\n\texpected expression of type: " ++ show expected ++
                                             "\n\tgot expression: " ++ show subexpr ++
                                             "\n\tof type: " ++ show tp

  empty_pattern_matching = "Empty pattern matching"

  empty_let_rec_binding = "Empty letrec binding"
