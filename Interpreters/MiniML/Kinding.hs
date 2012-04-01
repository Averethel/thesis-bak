module Interpreters.MiniML.Kinding (kind_of_type_expr) where
  import Data.List
  import Data.Maybe

  import Interpreters.MiniML.Syntax
  import Interpreters.MiniML.PrettyPrint
  import Interpreters.MiniML.Errors

  kind_of_type_constr :: TypeConstr -> Kind
  kind_of_type_constr Int   = K_Type
  kind_of_type_constr Bool  = K_Type
  kind_of_type_constr Unit  = K_Type
  kind_of_type_constr List  = K_Arrow K_Type K_Type
  kind_of_type_constr Ref   = K_Arrow K_Type K_Type

  kind_of_type_expr :: TypeExpr -> Either String Kind
  kind_of_type_expr (TE_Var _) = Right K_Type
  kind_of_type_expr te@(TE_Arrow e1 e2) =
    case (kind_of_type_expr e1, kind_of_type_expr e1) of
      (Left err,     _)            -> Left err
      (_,            Left err)     -> Left err
      (Right K_Type, Right K_Type) -> Right K_Type
      (Right K_Type, Right k2)     -> Left $ invalid "kind" te K_Type (e2, k2)
      (Right k1,     _)            -> Left $ invalid "kind" te K_Type (e1, k1)
  kind_of_type_expr te@(TE_Tuple es)
    | all (\x -> kind_of_type_expr x == Right K_Type) es = Right K_Type
    | otherwise                                          = Left $ invalid "kind" te K_Type (expr, kind_of_type_expr expr)
    where
      expr = fromJust $ find (\x -> kind_of_type_expr x /= Right K_Type) es
  kind_of_type_expr te@(TE_Constr es tc) = apply es $ kind_of_type_constr tc
    where
      apply [] fk     = Right fk
      apply (k:ks) fk =
        case (kind_of_type_expr k, fk) of
          (Left e, _)               -> Left e
          (Right kk, K_Arrow k1 k2) ->
            if   kk == k1
            then apply ks k2
            else Left $ invalid "kind" te k1 (k, kk)
          (_,       k)              -> Left $ too_many_arguments "Type constructor" tc te
