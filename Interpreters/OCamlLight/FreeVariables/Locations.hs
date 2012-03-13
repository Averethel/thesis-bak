module Interpreters.OCamlLight.FreeVariables.Locations where
  import Interpreters.OCamlLight.Syntax
  import Interpreters.OCamlLight.Aux

  fl_letrec_binding :: LetrecBinding -> [Location]
  fl_letrec_binding (LRB_Simple _ pm) = fl_pattern_matching pm

  fl_letrec_bindings :: LetrecBindings -> [Location]
  fl_letrec_bindings = concatMap fl_letrec_binding

  fl_let_binding :: LetBinding -> [Location]
  fl_let_binding (LB_Simple _ e) = fl_expr e

  fl_pat_exp :: PatExp -> [Location]
  fl_pat_exp (PE_Inj _ e) = fl_expr e

  fl_pattern_matching :: PatternMatching -> [Location]
  fl_pattern_matching (PM_Pm pes) = concatMap fl_pat_exp pes

  fl_expr :: Expr -> [Location]
  fl_expr (Expr_Location l)          = [l]
  fl_expr (Expr_Typed e _)           = fl_expr e
  fl_expr (Expr_Field e _)           = fl_expr e
  fl_expr (Expr_Assert e)            = fl_expr e
  fl_expr (Expr_Cons e1 e2)          = fl_expr e1 ++ fl_expr e2
  fl_expr (Expr_Apply e1 e2)         = fl_expr e1 ++ fl_expr e2
  fl_expr (Expr_And e1 e2)           = fl_expr e1 ++ fl_expr e2
  fl_expr (Expr_Or e1 e2)            = fl_expr e1 ++ fl_expr e2
  fl_expr (Expr_Function pm)         = fl_pattern_matching pm
  fl_expr (Expr_IfThenElse e1 e2 e3) = fl_expr e1 ++ fl_expr e2 ++ fl_expr e3
  fl_expr (Expr_While e1 e2)         = fl_expr e1 ++ fl_expr e2
  fl_expr (Expr_For _ e1 _ e2 e3)    = fl_expr e1 ++ fl_expr e2 ++ fl_expr e3
  fl_expr (Expr_Sequence e1 e2)      = fl_expr e1 ++ fl_expr e2
  fl_expr (Expr_Match e pm)          = fl_expr e ++ fl_pattern_matching pm
  fl_expr (Expr_Try e pm)            = fl_expr e ++ fl_pattern_matching pm
  fl_expr (Expr_Let lb e)            = fl_let_binding lb ++ fl_expr e
  fl_expr (Expr_Letrec lrbs e)       = fl_letrec_bindings lrbs ++ fl_expr e
  fl_expr (Expr_Tuple es)            = concatMap fl_expr es
  fl_expr (Expr_Construct c es)      = concatMap fl_expr es
  fl_expr (Expr_Record fes)          = concatMap (\(_, e) -> fl_expr e) fes
  fl_expr (Expr_Override e fes)      = fl_expr e ++ concatMap (\(_, e) -> fl_expr e) fes
  fl_expr _                          = []

  fl_definition :: Definition -> [Location]
  fl_definition (D_Let lb)      = fl_let_binding lb
  fl_definition (D_Letrec lrbs) = fl_letrec_bindings lrbs
  fl_definition _               = []

  fl_definitions :: Definitions -> [Location]
  fl_definitions = concatMap fl_definition

  fl_substs_x :: SubstsX -> [Location]
  fl_substs_x = concatMap (\(_, e) -> fl_expr e)

  fl_program :: Program -> [Location]
  fl_program (Prog_Defs ds) = fl_definitions ds
  fl_program (Prog_Raise e) = fl_expr e

  fl_trans_label :: TransLabel -> [Location]
  fl_trans_label _ = []
