module Interpreters.OCamlLight.FreeVariables.TypeVariables where
  import Data.List ((\\))
  import Interpreters.OCamlLight.Syntax
  import Interpreters.OCamlLight.Aux

  ftv_typexpr :: TypeExpr -> [TypeVar]
  ftv_typexpr (TE_Var tv)        = [tv]
  ftv_typexpr (TE_Arrow te1 te2) = ftv_typexpr te1 ++ ftv_typexpr te2
  ftv_typexpr (TE_Tuple tes)     = concatMap ftv_typexpr tes
  ftv_typexpr (TE_Constr tes _)  = concatMap ftv_typexpr tes
  ftv_typexpr _                  = []

  ftv_constr_decl :: ConstrDecl -> [TypeVar]
  ftv_constr_decl (CD_NAry _ tes) = concatMap ftv_typexpr tes
  ftv_constr_decl _               = []

  ftv_field_decl :: FieldDecl -> [TypeVar]
  ftv_field_decl (FD_Immutable _ te) = ftv_typexpr te

  ftv_type_equation :: TypeEquation -> [TypeVar]
  ftv_type_equation (TE_Te te) = ftv_typexpr te

  ftv_type_representation :: TypeRepresentation -> [TypeVar]
  ftv_type_representation (TR_Variant cds) = concatMap ftv_constr_decl cds
  ftv_type_representation (TR_Record fds)  = concatMap ftv_field_decl fds

  ftv_type_information :: TypeInformation -> [TypeVar]
  ftv_type_information (TI_Eq te)  = ftv_type_equation te
  ftv_type_information (TI_Def tr) = ftv_type_representation tr

  ftv_pattern :: Pattern -> [TypeVar]
  ftv_pattern (P_Alias p _)      = ftv_pattern p
  ftv_pattern (P_Typed p te)     = ftv_pattern p ++ ftv_typexpr te
  ftv_pattern (P_Or p1 p2)       = ftv_pattern p1 ++ ftv_pattern p2
  ftv_pattern (P_Cons p1 p2)     = ftv_pattern p1 ++ ftv_pattern p2
  ftv_pattern (P_Construct _ ps) = concatMap ftv_pattern ps
  ftv_pattern (P_Tuple ps)       = concatMap ftv_pattern ps
  ftv_pattern (P_Record fps)     = concatMap (\(_, p) -> ftv_pattern p) fps
  ftv_pattern _                  = []

  ftv_typedef :: Typedef -> [TypeVar]
  ftv_typedef (TD_Td tpo _ ti) = ftv_type_information ti \\ typevars_of_type_params_opt tpo -- check if removing only first is enough!

  ftv_letrec_binding :: LetrecBinding -> [TypeVar]
  ftv_letrec_binding (LRB_Simple _ pm) = ftv_pattern_matching pm

  ftv_letrec_bindings :: LetrecBindings -> [TypeVar]
  ftv_letrec_bindings = concatMap ftv_letrec_binding

  ftv_let_binding :: LetBinding -> [TypeVar]
  ftv_let_binding (LB_Simple p e) = ftv_pattern p ++ ftv_expr e

  ftv_pat_exp :: PatExp -> [TypeVar]
  ftv_pat_exp (PE_Inj p e) = ftv_pattern p ++ ftv_expr e

  ftv_pattern_matching :: PatternMatching -> [TypeVar]
  ftv_pattern_matching (PM_Pm pes) = concatMap ftv_pat_exp pes

  ftv_expr :: Expr -> [TypeVar]
  ftv_expr (Expr_Field e f)           = ftv_expr e
  ftv_expr (Expr_Function pm)         = ftv_pattern_matching pm
  ftv_expr (Expr_Assert e)            = ftv_expr e
  ftv_expr (Expr_Typed e tp)          = ftv_expr e ++ ftv_typexpr tp
  ftv_expr (Expr_Cons e1 e2)          = ftv_expr e1 ++ ftv_expr e2
  ftv_expr (Expr_Apply e1 e2)         = ftv_expr e1 ++ ftv_expr e2
  ftv_expr (Expr_And e1 e2)           = ftv_expr e1 ++ ftv_expr e2
  ftv_expr (Expr_Or e1 e2)            = ftv_expr e1 ++ ftv_expr e2
  ftv_expr (Expr_IfThenElse e1 e2 e3) = ftv_expr e1 ++ ftv_expr e2 ++ ftv_expr e3
  ftv_expr (Expr_While e1 e2)         = ftv_expr e1 ++ ftv_expr e2
  ftv_expr (Expr_For _ e1 _ e2 e3)    = ftv_expr e1 ++ ftv_expr e2 ++ ftv_expr e3
  ftv_expr (Expr_Sequence e1 e2)      = ftv_expr e1 ++ ftv_expr e2
  ftv_expr (Expr_Match e pm)          = ftv_expr e ++ ftv_pattern_matching pm
  ftv_expr (Expr_Try e pm)            = ftv_expr e ++ ftv_pattern_matching pm
  ftv_expr (Expr_Let lb e)            = ftv_let_binding lb ++ ftv_expr e
  ftv_expr (Expr_Letrec lrbs e)       = ftv_letrec_bindings lrbs ++ ftv_expr e
  ftv_expr (Expr_Tuple es)            = concatMap ftv_expr es
  ftv_expr (Expr_Construct c es)      = concatMap ftv_expr es
  ftv_expr (Expr_Record fes)          = concatMap (\(_, e) -> ftv_expr e) fes
  ftv_expr (Expr_Override e fes)      = ftv_expr e ++ concatMap (\(_, e) -> ftv_expr e) fes
  ftv_expr _                          = []

  ftv_type_definition :: TypeDefinition -> [TypeVar]
  ftv_type_definition = concatMap ftv_typedef

  ftv_exception_definition :: ExceptionDefinition -> [TypeVar]
  ftv_exception_definition (ED_Def cd) = ftv_constr_decl cd

  ftv_definition :: Definition -> [TypeVar]
  ftv_definition (D_Let lb)       = ftv_let_binding lb
  ftv_definition (D_Letrec lrbs)  = ftv_letrec_bindings lrbs
  ftv_definition (D_Type td)      = ftv_type_definition td
  ftv_definition (D_Exception ed) = ftv_exception_definition ed

  ftv_definitions :: Definitions -> [TypeVar]
  ftv_definitions = concatMap ftv_definition

  ftv_typescheme :: TypeScheme -> [TypeVar]
  ftv_typescheme (TS_Forall tp) = ftv_typexpr tp

  ftv_typexprs :: TypeExprs -> [TypeVar]
  ftv_typexprs = concatMap ftv_typexpr

  ftv_substs_x :: SubstsX -> [TypeVar]
  ftv_substs_x = concatMap (\(_, e) -> ftv_expr e)

  ftv_program :: Program -> [TypeVar]
  ftv_program (Prog_Defs ds) = ftv_definitions ds
  ftv_program (Prog_Raise e) = ftv_expr e

  ftv_environment_binding :: EnvironmentBinding -> [TypeVar]
  ftv_environment_binding (EB_VN _ ts)        = ftv_typescheme ts
  ftv_environment_binding (EB_PC _ tpo tes _) = ftv_typexprs tes \\ typevars_of_type_params_opt tpo
  ftv_environment_binding (EB_FN _ tpo te _)  = ftv_typexpr te \\ typevars_of_type_params_opt tpo
  ftv_environment_binding (EB_TA tpo _ te)    = ftv_typexpr te \\ typevars_of_type_params_opt tpo
  ftv_environment_binding (EB_L _ te)         = ftv_typexpr te

  ftv_trans_label :: TransLabel -> [TypeVar]
  ftv_trans_label _ = []
