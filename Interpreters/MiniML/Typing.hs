module Interpreters.MiniML.Typing where
  import Interpreters.MiniML.Syntax

  type Kind = Integer

  data EnvBinding =
      EB_TV TypeVar
    | EB_TE ValueName TypeExpr
    deriving (Show, Eq)

  type Env = [EnvBinding]

  is_bound_in :: TypeVar -> Env -> Bool
  is_bound_in _ []                = False
  is_bound_in v ((EB_TE _ _):ebs) = v `is_bound_in` ebs
  is_bound_in v ((EB_TV tv):ebs)
    | v == tv                     = True
    | otherwise                   = v `is_bound_in` ebs


  kind_of_type_constr :: TypeConstr -> Kind
  kind_of_type_constr Int   = 0
  kind_of_type_constr Float = 0
  kind_of_type_constr Bool  = 0
  kind_of_type_constr Unit  = 0
  kind_of_type_constr List  = 1
  kind_of_type_constr Ref   = 1

  kind_of_type_expr :: Env -> TypeExpr -> Kind
  kind_of_type_expr e (TE_Var v)
    | v `is_bound_in` e                                  = 0
  kind_of_type_expr e (TE_Arrow e1 e2)
    | kind_of_type_expr e e1 == 0 &&
      kind_of_type_expr e e1 == 0                        = 0
  kind_of_type_expr e (TE_Tuple es)
    | all (\x -> kind_of_type_expr e x == 0) es          = 0
  kind_of_type_expr _ (TE_Constr [] tc)
    | kind_of_type_constr tc == 0                        = 0
  kind_of_type_expr e (TE_Constr es tc)
    | kind_of_type_constr tc == (toInteger.length $ es) &&
      all (\x -> kind_of_type_expr e x == 0) es          = 0

  type_of_constant :: Env -> TypeExpr -> Constant -> TypeExpr
  type_of_constant _ _ (C_Int _)   = TE_Constr [] Int
  type_of_constant _ _ (C_Float _) = TE_Constr [] Float
  type_of_constant _ _ C_False     = TE_Constr [] Bool
  type_of_constant _ _ C_True      = TE_Constr [] Bool
  type_of_constant e t C_Nil
    | kind_of_type_expr e t == 0   = TE_Constr [t] List
  type_of_constant _ _ C_Unit      = TE_Constr [] Unit

  type_of_unary_primitive :: Env -> TypeExpr -> UnaryPrim -> TypeExpr
  type_of_unary_primitive _ _ U_Not   = TE_Arrow (TE_Constr [] Bool) (TE_Constr [] Bool)
  type_of_unary_primitive e t U_Ref
    | kind_of_type_expr e t == 0      = TE_Arrow t (TE_Constr [t] Ref)
  type_of_unary_primitive e t U_Deref = 
    | kind_of_type_expr e t == 0      = TE_Arrow (TE_Constr [t] Ref) t
  type_of_unary_primitive _ _ U_Minus = TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)

  type_of_binary_primitive :: Env -> TypeExpr -> BinaryPrim -> TypeExpr
  type_of_binary_primitive e t B_Eq
    | kind_of_type_expr e t == 0         = TE_Arrow t (TE_Arrow t Bool)
  type_of_binary_primitive _ _ B_I_Plus  = TE_Arrow Int (TE_Arrow Int Bool)
  type_of_binary_primitive _ _ B_I_Minus = TE_Arrow Int (TE_Arrow Int Bool)
  type_of_binary_primitive _ _ B_I_Mult  = TE_Arrow Int (TE_Arrow Int Bool)
  type_of_binary_primitive _ _ B_I_Div   = TE_Arrow Int (TE_Arrow Int Bool)
  type_of_binary_primitive _ _ B_F_Plus  = TE_Arrow Float (TE_Arrow Float Bool)
  type_of_binary_primitive _ _ B_F_Minus = TE_Arrow Float (TE_Arrow Float Bool)
  type_of_binary_primitive _ _ B_F_Mult  = TE_Arrow Float (TE_Arrow Float Bool)
  type_of_binary_primitive _ _ B_F_Div   = TE_Arrow Float (TE_Arrow Float Bool)
  type_of_binary_primitive t e B_Assign
    | kind_of_type_expr e t == 0         = TE_Arrow (TE_Constr [t] Ref) (TE_Arrow t Bool)
  
