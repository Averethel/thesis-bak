module Interpreters.MiniML.Unification (unify, check_types_simple) where
  import Interpreters.MiniML.Syntax
  import Interpreters.MiniML.PrettyPrint
  import Interpreters.MiniML.Errors

  subst :: TypeVar -> TypeExpr -> TypeExpr -> TypeExpr
  subst x te (TE_Var tv)
    | x == tv                   = te
    | otherwise                 = TE_Var tv
  subst x te (TE_Arrow te1 te2) = TE_Arrow (subst x te te1) (subst x te te2)
  subst x te (TE_Tuple tes)     = TE_Tuple $ map (subst x te) tes
  subst x te (TE_Constr tes tc) = TE_Constr (map (subst x te) tes) tc

  subst_env :: TypeVar -> TypeExpr -> Env -> Env
  subst_env tv te []             = []
  subst_env tv te ((vn, te'):es) = (vn, subst tv te te') : subst_env tv te es

  unify :: Constraints -> SimpleConstraints -> Env -> Either String (SimpleConstraints, Env)
  unify []                                          scs env = Right (scs, env)
  unify ((TE_Var x, te):cs)                         scs env = unify cs (map (subst x te) scs) $ subst_env x te env -- tutaj pomyśleć czy nie trzeba dodać x <\- FV(te)
  unify ((te, TE_Var x):cs)                         scs env = unify cs (map (subst x te) scs) $ subst_env x te env 
  unify ((TE_Arrow t1 t2, TE_Arrow t1' t2'):cs)     scs env = unify ((t1,t1'):(t2,t2'):cs) scs env
  unify ((TE_Tuple ts1, TE_Tuple ts2):cs)           scs env
    | length ts1 == length ts2                              = unify ((zip ts1 ts2) ++ cs) scs env
    | otherwise                                             = Left $ cannot_unify (TE_Tuple ts1, TE_Tuple ts2)
  unify ((TE_Constr ts1 tc1, TE_Constr ts2 tc2):cs) scs env
    | length ts1 == length ts2 && tc1 == tc2                = unify ((zip ts1 ts2) ++ cs) scs env
    | otherwise                                             = Left $ cannot_unify (TE_Constr ts1 tc1, TE_Constr ts2 tc2)
  unify ((te1, te2):cs)                             scs env
    | te1 == te2                                            = unify cs scs env
    | otherwise                                             = Left $ cannot_unify (te1, te2)

  is_type_simple :: TypeExpr -> Bool
  is_type_simple (TE_Var _)        = True
  is_type_simple (TE_Arrow _ _)    = False
  is_type_simple (TE_Tuple tes)    = all is_type_simple tes
  is_type_simple (TE_Constr tes _) = all is_type_simple tes

  check_types_simple :: SimpleConstraints -> Env -> Either String Env
  check_types_simple []       env = Right env
  check_types_simple (te:tes) env
    | is_type_simple te           = check_types_simple tes env
    | otherwise                   = Left $ non_simple_type te
