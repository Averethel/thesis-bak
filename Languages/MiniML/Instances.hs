{-# LANGUAGE
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeSynonymInstances
  #-}

module Languages.MiniML.Instances where
  import qualified Utils.Classes.Clojure      as CC
  import qualified Utils.Classes.Expression   as EC
  import qualified Utils.Classes.Instruction  as IC
  import qualified Utils.Classes.LanguageName as LN
  import qualified Utils.Classes.Program      as PC
  import qualified Utils.Classes.Type         as TC
  import qualified Utils.Classes.Value        as VC
  import Utils.EvalEnv

  import Languages.MiniML.PrettyPrint
  import Languages.MiniML.Syntax

  instance LN.LanguageName MiniMLName

  instance PC.Program Program

  instance IC.Instruction Instruction

  instance TC.Type TypeExpr where
    isVar (TE_Var _) = True
    isVar _          = False

    canCompare (TE_Var _)       = True
    canCompare (TE_Arrow _ _)   = False
    canCompare (TE_Tuple ls)    = all TC.canCompare ls
    canCompare (TE_Constr ls _) = all TC.canCompare ls

    getVar (TE_Var x) = x

    canUnify (TE_Var _)        _                 = True
    canUnify _                 (TE_Var _)        = True
    canUnify (TE_Arrow _ _)    (TE_Arrow _ _)    = True
    canUnify (TE_Tuple l1)     (TE_Tuple l2)     =
      length l1 == length l2
    canUnify (TE_Constr l1 c1) (TE_Constr l2 c2) =
      length l1 == length l2 && c1 == c2
    canUnify t1                t2                =
      t1 == t2

    newConstraints (TE_Arrow t1 t2) (TE_Arrow t3 t4)    =
      [(t1, t3), (t2, t4)]
    newConstraints (TE_Tuple l1)      (TE_Tuple l2)
      | length l1 == length l2                          =
        zip l1 l2
    newConstraints (TE_Constr l1 c1)  (TE_Constr l2 c2)
      | c1 == c2 && length l1 == length l2              =
        zip l1 l2
    newConstraints t1                 t2
      | t1 == t2                                        =
        []

    applySubst t []     = t
    applySubst t (s:ss) = TC.applySubst (applySingleSubst s t) ss where
      applySingleSubst (s, t) (TE_Var v)
        | s == v                               =
          t
        | otherwise                            =
          TE_Var v
      applySingleSubst s      (TE_Arrow t1 t2) =
        TE_Arrow (applySingleSubst s t1) $ applySingleSubst s t2
      applySingleSubst s      (TE_Tuple ts)    =
        TE_Tuple $ map (applySingleSubst s) ts
      applySingleSubst s      (TE_Constr ts c) =
        TE_Constr (map (applySingleSubst s) ts) c

  instance EC.Expression Expr

  instance VC.Value Value where
    nullValue = V_Null

  instance CC.Clojure Value Expr where
    mkClo (E_Function bs) env =
      V_Clo env bs
