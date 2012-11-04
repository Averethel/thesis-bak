module Compiler.Passes.FoldTuples (fold_tuples) where
  import Languages.MiniML.Syntax

  pattern_tuple_to_pairs :: [Pattern] -> Pattern
  pattern_tuple_to_pairs [a, b] =
    P_Tuple [a, b]
  pattern_tuple_to_pairs (a:as) =
    let as' = pattern_tuple_to_pairs as
    in P_Tuple [a, as']    

  fold_tuples_pattern :: Pattern -> Pattern
  fold_tuples_pattern (P_Tuple ps)   =
    pattern_tuple_to_pairs ps
  fold_tuples_pattern (P_Cons p1 p2) =
    P_Cons (fold_tuples_pattern p1) (fold_tuples_pattern p2)
  fold_tuples_pattern p = 
    p

  tuple_to_pairs :: [Expr] -> Expr
  tuple_to_pairs [a, b] =
    E_Tuple [a, b] 
  tuple_to_pairs (a:as) = 
    let as' = tuple_to_pairs as
    in E_Tuple [a, as']

  fold_tuples_let_bindings :: [Binding] -> [Binding]
  fold_tuples_let_bindings = map (\(p, e) -> (fold_tuples_pattern p, 
                                              fold_tuples_expression e))

  fold_tuples_bindings :: [FunBinding] -> [FunBinding]
  fold_tuples_bindings = map (\(p, e, g) -> (fold_tuples_pattern p, 
                                             fold_tuples_expression e,
                                             fold_tuples_expression g))

  fold_tuples_letrec_bindings :: [LetRecBinding] -> [LetRecBinding]
  fold_tuples_letrec_bindings = map (\(n, e) -> (n, fold_tuples_expression e))

  fold_tuples_expression :: Expr -> Expr
  fold_tuples_expression (E_Tuple ps) =
    tuple_to_pairs ps
  fold_tuples_expression (E_Apply e1 e2)  =
    E_Apply (fold_tuples_expression e1) (fold_tuples_expression e2)
  fold_tuples_expression (E_Cons e1 e2)   =
    E_Cons (fold_tuples_expression e1) (fold_tuples_expression e2)
  fold_tuples_expression (E_And e1 e2)    =
    E_And (fold_tuples_expression e1) (fold_tuples_expression e2)
  fold_tuples_expression (E_Or e1 e2)     =
    E_Or (fold_tuples_expression e1) (fold_tuples_expression e2)
  fold_tuples_expression (E_ITE e1 e2 e3) =
    E_ITE (fold_tuples_expression e1) (fold_tuples_expression e2) 
          (fold_tuples_expression e3)
  fold_tuples_expression (E_Seq e1 e2)    =
    E_Seq (fold_tuples_expression e1) (fold_tuples_expression e2)
  fold_tuples_expression (E_Function bs)  =
    E_Function (fold_tuples_bindings bs)
  fold_tuples_expression (E_Let bs e)     =
    E_Let (fold_tuples_let_bindings bs) (fold_tuples_expression e)
  fold_tuples_expression (E_LetRec bs e)  =
    E_LetRec (fold_tuples_letrec_bindings bs) (fold_tuples_expression e)
  fold_tuples_expression e                = 
    e

  fold_tuples_definition :: Definition -> Definition
  fold_tuples_definition (D_Let bs)    =
    D_Let $ fold_tuples_let_bindings bs
  fold_tuples_definition (D_LetRec bs) =
    D_LetRec $ fold_tuples_letrec_bindings bs

  fold_tuples_instruction :: Instruction -> Instruction
  fold_tuples_instruction (IEX e) = 
    IEX $ fold_tuples_expression e
  fold_tuples_instruction (IDF d) =
    IDF $ fold_tuples_definition d

  fold_tuples_program :: Program -> Program
  fold_tuples_program = map fold_tuples_instruction

  fold_tuples = fold_tuples_program
