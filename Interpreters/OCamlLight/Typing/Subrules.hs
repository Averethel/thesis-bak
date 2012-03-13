module Interpreters.OCamlLight.Typing.Subrules where
  import Interpreters.OCamlLight.Syntax

  is_binary_prim_app_value_of_expr :: Expr -> Bool
  is_binary_prim_app_value_of_expr (Expr_Bprim _) = True
  is_binary_prim_app_value_of_expr _              = False

  is_definition_value_of_definition :: Definition -> Bool
  is_definition_value_of_definition (D_Type _)      = True
  is_definition_value_of_definition (D_Exception _) = True
  is_definition_value_of_definition _               = False

  is_value_of_expr :: Expr -> Bool
  is_value_of_expr (Expr_Uprim _)        = True
  is_value_of_expr (Expr_Bprim _)        = True
  is_value_of_expr (Expr_Constant _)     = True
  is_value_of_expr (Expr_Function _)     = True
  is_value_of_expr (Expr_Location _)     = True
  is_value_of_expr (Expr_Cons e1 e2)     = is_value_of_expr e1 && is_value_of_expr e2
  is_value_of_expr (Expr_Apply e1 e2)    = is_binary_prim_app_value_of_expr e1 && is_value_of_expr e2
  is_value_of_expr (Expr_Tuple tp)       = all is_value_of_expr tp
  is_value_of_expr (Expr_Construct _ es) = all is_value_of_expr es
  is_value_of_expr (Expr_Record fs)      = all (\(_,e) -> is_value_of_expr e) fs
  is_value_of_expr _                     = False

  is_non_expansive_of_expr :: Expr -> Bool
  is_non_expansive_of_expr (Expr_Uprim _)        = True
  is_non_expansive_of_expr (Expr_Bprim _)        = True
  is_non_expansive_of_expr (Expr_Ident _)        = True
  is_non_expansive_of_expr (Expr_Constant _)     = True
  is_non_expansive_of_expr (Expr_Function _)     = True
  is_non_expansive_of_expr (Expr_Location _)     = True
  is_non_expansive_of_expr (Expr_Typed e _)      = is_non_expansive_of_expr e
  is_non_expansive_of_expr (Expr_Letrec _ e)     = is_non_expansive_of_expr e
  is_non_expansive_of_expr (Expr_Apply e1 e2)    = is_binary_prim_app_value_of_expr e1 && is_non_expansive_of_expr e2
  is_non_expansive_of_expr (Expr_Cons e1 e2)     = is_non_expansive_of_expr e1 && is_non_expansive_of_expr e2
  is_non_expansive_of_expr (Expr_Tuple es)       = all is_non_expansive_of_expr es
  is_non_expansive_of_expr (Expr_Construct _ es) = all is_non_expansive_of_expr es
  is_non_expansive_of_expr (Expr_Record fs)      = all (\(_,e) -> is_non_expansive_of_expr e) fs
  is_non_expansive_of_expr _                     = False

  is_src_typexpr_of_typexpr :: TypeExpr -> Bool
  is_src_typexpr_of_typexpr (TE_Var _)         = True
  is_src_typexpr_of_typexpr TE_Any             = True
  is_src_typexpr_of_typexpr (TE_Arrow te1 te2) = is_src_typexpr_of_typexpr te1 && is_src_typexpr_of_typexpr te2
  is_src_typexpr_of_typexpr (TE_Tuple tes)     = all is_src_typexpr_of_typexpr tes
  is_src_typexpr_of_typexpr (TE_Constr tes _)  = all is_src_typexpr_of_typexpr tes
  is_src_typexpr_of_typexpr _                  = False

  is_definitions_value_of_definitions :: Definitions -> Bool
  is_definitions_value_of_definitions xs = all is_definition_value_of_definition xs

  is_trans_label_of_trans_label :: TransLabel -> Bool
  is_trans_label_of_trans_label Lab_Nil          = True
  is_trans_label_of_trans_label (Lab_Alloc e _)  = is_value_of_expr e
  is_trans_label_of_trans_label (Lab_Deref _ e)  = is_value_of_expr e
  is_trans_label_of_trans_label (Lab_Assign _ e) = is_value_of_expr e
