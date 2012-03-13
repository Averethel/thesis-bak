module Interpreters.OCamlLight.Aux where
  import Interpreters.OCamlLight.Syntax

  constr_name_of_constr_decl :: ConstrDecl -> ConstrName
  constr_name_of_constr_decl (CD_NullAry cn) = cn
  constr_name_of_constr_decl (CD_NAry cn _)  = cn

  constr_names_of_type_representation :: TypeRepresentation -> [ConstrName]
  constr_names_of_type_representation (TR_Variant decls) = map constr_name_of_constr_decl decls
  constr_names_of_type_representation (TR_Record _)      = []

  constr_names_of_type_information :: TypeInformation -> [ConstrName]
  constr_names_of_type_information (TI_Eq _)     = []
  constr_names_of_type_information (TI_Def repr) = constr_names_of_type_representation repr

  field_name_of_field_decl :: FieldDecl -> FieldName
  field_name_of_field_decl (FD_Immutable n _) = n

  value_names_of_pattern :: Pattern -> [ValueName]
  value_names_of_pattern (P_Var vn)         = [vn]
  value_names_of_pattern (P_Alias pat vn)   = reverse $ vn:value_names_of_pattern pat
  value_names_of_pattern (P_Typed pat te)   = value_names_of_pattern pat
  value_names_of_pattern (P_Or p1 p2)       = value_names_of_pattern p1
  value_names_of_pattern (P_Construct c ps) = concatMap value_names_of_pattern ps
  value_names_of_pattern (P_Tuple ps)       = concatMap value_names_of_pattern ps
  value_names_of_pattern (P_Record fps)     = concatMap (\(_,p) -> value_names_of_pattern p) fps
  value_names_of_pattern (P_Cons p1 p2)     = value_names_of_pattern p1 ++ value_names_of_pattern p2
  value_names_of_pattern _                  = []

  value_name_of_letrec_binding :: LetrecBinding -> ValueName
  value_name_of_letrec_binding (LRB_Simple vn _) = vn

  constr_names_of_typedef :: Typedef -> [ConstrName]
  constr_names_of_typedef (TD_Td _ _ ti) = constr_names_of_type_information ti

  field_names_of_type_representation :: TypeRepresentation -> [FieldName]
  field_names_of_type_representation (TR_Variant _)  = []
  field_names_of_type_representation (TR_Record fds) = map field_name_of_field_decl fds

  type_name_of_typedef :: Typedef -> TypeconstrName
  type_name_of_typedef (TD_Td _ tc_name _) = tc_name

  typevar_of_type_param :: TypeParam -> TypeVar
  typevar_of_type_param (TP_Var tv) = tv

  value_names_of_let_binding :: LetBinding -> [ValueName]
  value_names_of_let_binding (LB_Simple pat _) = value_names_of_pattern pat

  value_names_of_letrec_bindings :: LetrecBindings -> [ValueName]
  value_names_of_letrec_bindings = map value_name_of_letrec_binding

  constr_names_of_type_definition :: TypeDefinition -> [ConstrName]
  constr_names_of_type_definition = concatMap constr_names_of_typedef

  field_names_of_type_information :: TypeInformation -> [FieldName]
  field_names_of_type_information (TI_Eq _)   = []
  field_names_of_type_information (TI_Def tr) = field_names_of_type_representation tr

  type_names_of_type_definition :: TypeDefinition -> [TypeconstrName]
  type_names_of_type_definition = map type_name_of_typedef

  typevars_of_type_params_opt :: TypeParamsOpt -> [TypeVar]
  typevars_of_type_params_opt = map typevar_of_type_param

  value_names_of_definition :: Definition -> [ValueName]
  value_names_of_definition (D_Let lb)      = value_names_of_let_binding lb
  value_names_of_definition (D_Letrec lrbs) = value_names_of_letrec_bindings lrbs
  value_names_of_definition _               = []
