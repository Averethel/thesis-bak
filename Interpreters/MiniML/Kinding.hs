module Interpreters.MiniML.Kinding where
  import Data.List
  import Data.Maybe

  import Interpreters.MiniML.Syntax
  import Interpreters.MiniML.Errors

  kind_of_type_constr :: TypeConstr -> Kind
  kind_of_type_constr Int   = K_Type
  kind_of_type_constr Float = K_Type
  kind_of_type_constr Bool  = K_Type
  kind_of_type_constr Unit  = K_Type
  kind_of_type_constr List  = K_Arrow 1
  kind_of_type_constr Ref   = K_Arrow 1

  kind_of_type_expr :: Env -> TypeExpr -> Either String Kind
  kind_of_type_expr e (TE_Var v)
    | v `is_bound_in` e = Right K_Type
    | otherwise         = Left $ unbound_type_variable v
  kind_of_type_expr e te@(TE_Arrow e1 e2) =
    case (kind_of_type_expr e e1, kind_of_type_expr e e1) of
      (Left err,     _)            -> Left err
      (_,            Left err)     -> Left err
      (Right K_Type, Right K_Type) -> Right K_Type
      (Right K_Type, Right k2)     -> Left $ invalid_kind te K_Type (e2, k2)
      (Right k1,     _)            -> Left $ invalid_kind te K_Type (e1, k1)
  kind_of_type_expr e te@(TE_Tuple es)
    | all (\x -> kind_of_type_expr e x == Right K_Type) es = Right K_Type
    | otherwise                                            = Left $ invalid_kind te K_Type (expr, kind_of_type_expr e expr)
    where
      expr = fromJust $ find (\x -> kind_of_type_expr e x /= Right K_Type) es
  kind_of_type_expr e te@(TE_Constr es tc) =
    case (kind_of_type_constr tc, length es) of
      (K_Type,    0) -> Right K_Type
      (K_Type,    n) -> Left $ invalid_type_constr te tc 0 n
      (K_Arrow n, z) ->
        if n == toInteger z
        then 
          if all (\x -> kind_of_type_expr e x == Right K_Type) es
          then Right K_Type
          else Left $ invalid_kind te K_Type (expr, kind_of_type_expr e expr)
        else Left $ invalid_type_constr te tc n z
        where
          expr = fromJust $ find (\x -> kind_of_type_expr e x /= Right K_Type) es

  kind_of_type_scheme :: Env -> TypeScheme -> Either String Kind
  kind_of_type_scheme e (TS_Forall te) = kind_of_type_expr (e ++ map EB_TV (free_type_vars e te)) te
    where
      free_type_vars e (TE_Var v)
        | v `is_bound_in` e              = []
        | otherwise                      = [v]
      free_type_vars e (TE_Arrow e1 e2)  = (free_type_vars e e1) ++ (free_type_vars e e2)
      free_type_vars e (TE_Tuple es)     = concatMap (free_type_vars e) es 
      free_type_vars e (TE_Constr es tc) = concatMap (free_type_vars e) es