module Interpreters.OCamlLight.FreeVariables.Variables where
  import Data.List ((\\))
  import Interpreters.OCamlLight.Syntax
  import Interpreters.OCamlLight.Aux

  fv_letrec_binding :: LetrecBinding -> [ValueName]
  fv_letrec_binding (LRB_Simple _ pm) = fv_pattern_matching pm

  fv_letrec_bindings :: LetrecBindings -> [ValueName]
  fv_letrec_bindings = concatMap fv_letrec_binding

  fv_let_binding :: LetBinding -> [ValueName]
  fv_let_binding (LB_Simple _ e) = fv_expr e

  fv_pat_exp :: PatExp -> [ValueName]
  fv_pat_exp (PE_Inj p e) = fv_expr e \\ value_names_of_pattern p

  fv_pattern_matching :: PatternMatching -> [ValueName]
  fv_pattern_matching (PM_Pm pes) = concatMap fv_pat_exp pes

  fv_expr :: Expr -> [ValueName]
  fv_expr (Expr_Ident vn)            = [vn]
  fv_expr (Expr_Typed e _)           = fv_expr e
  fv_expr (Expr_Field e _)           = fv_expr e
  fv_expr (Expr_Assert e)            = fv_expr e
  fv_expr (Expr_Function pm)         = fv_pattern_matching pm
  fv_expr (Expr_Cons e1 e2)          = fv_expr e1 ++ fv_expr e2
  fv_expr (Expr_Apply e1 e2)         = fv_expr e1 ++ fv_expr e2
  fv_expr (Expr_And e1 e2)           = fv_expr e1 ++ fv_expr e2
  fv_expr (Expr_Or e1 e2)            = fv_expr e1 ++ fv_expr e2
  fv_expr (Expr_IfThenElse e1 e2 e3) = fv_expr e1 ++ fv_expr e2 ++ fv_expr e3
  fv_expr (Expr_While e1 e2)         = fv_expr e1 ++ fv_expr e2
  fv_expr (Expr_Sequence e1 e2)      = fv_expr e1 ++ fv_expr e2
  fv_expr (Expr_Match e pm)          = fv_expr e ++ fv_pattern_matching pm
  fv_expr (Expr_Try e pm)            = fv_expr e ++ fv_pattern_matching pm
  fv_expr (Expr_For vn e1 _ e2 e3)   = fv_expr e1 ++ fv_expr e2 ++ (fv_expr e2 \\ [vn])
  fv_expr (Expr_Letrec lrbs e)       = (fv_letrec_bindings lrbs \\ value_names_of_letrec_bindings lrbs) ++ (fv_expr e \\ value_names_of_letrec_bindings lrbs)
  fv_expr (Expr_Let lb e)            = fv_let_binding lb ++ (fv_expr e \\ value_names_of_let_binding lb)
  fv_expr (Expr_Tuple es)            = concatMap fv_expr es
  fv_expr (Expr_Construct c es)      = concatMap fv_expr es
  fv_expr (Expr_Record fes)          = concatMap (\(_, e) -> fv_expr e) fes
  fv_expr (Expr_Override e fes)      = fv_expr e ++ concatMap (\(_, e) -> fv_expr e) fes
  fv_expr _                          = []

  fv_definition :: Definition -> [ValueName]
  fv_definition (D_Let lb)      = fv_let_binding lb
  fv_definition (D_Letrec lrbs) = fv_letrec_bindings lrbs \\ value_names_of_letrec_bindings lrbs
  fv_definition _               = []

  fv_definitions :: Definitions -> [ValueName]
  fv_definitions []     = []
  fv_definitions (d:ds) = fv_definition d ++ (fv_definitions ds \\ value_names_of_definition d)

  fv_substs_x :: SubstsX -> [ValueName]
  fv_substs_x = concatMap (\(_, e) -> fv_expr e)

  fv_program :: Program -> [ValueName]
  fv_program (Prog_Defs ds) = fv_definitions ds
  fv_program (Prog_Raise e) = fv_expr e

  fv_trans_label :: TransLabel -> [ValueName]
  fv_trans_label _ = []
