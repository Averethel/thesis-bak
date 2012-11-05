{-# LANGUAGE
  FlexibleInstances,
  TypeSynonymInstances
  #-}

module Languages.EnrichedLambda.Instances where
  import qualified Utils.LanguageClass as LC

  import Languages.EnrichedLambda.PrettyPrint
  import Languages.EnrichedLambda.Syntax

  instance LC.Program Program

  instance LC.Type Type where
    isVar (T_Var _) = True
    isVar _         = False

    canCompare (T_Var v)         = True
    canCompare (T_Arrow tp1 tp2) = False
    canCompare (T_Ref tp)        = True
    canCompare (T_Defined _ ts)  = all LC.canCompare ts

    getVar (T_Var v) = v

    canUnify (T_Var _)          _                  = True
    canUnify _                  (T_Var _)          = True
    canUnify (T_Arrow _ _)      (T_Arrow _ _)      = True
    canUnify (T_Ref _)          (T_Ref _)          = True
    canUnify (T_Defined n1 ts1) (T_Defined n2 ts2) = n1 == n2 && 
                                                    length ts1 == length ts2
    canUnify _                  _                  = False

    newConstraints (T_Arrow t1 t2)    (T_Arrow t3 t4)    = [(t1, t3), (t2, t4)]
    newConstraints (T_Ref t1)         (T_Ref t2)         = [(t1, t2)]
    newConstraints (T_Defined n1 ts1) (T_Defined n2 ts2)
      | n1 == n2 && length ts1 == length ts2             = zip ts1 ts2

    applySubst t []     = t
    applySubst t (s:ss) = LC.applySubst (applySingleSubst t s) ss where
      applySingleSubst (T_Var v)        (s, t)
        | v == s                               = t
        | otherwise                            = T_Var v
      applySingleSubst (T_Arrow t1 t2)  s      = T_Arrow (applySingleSubst t1 s)
                                                         (applySingleSubst t2 s)
      applySingleSubst (T_Ref t)        s      = T_Ref $ applySingleSubst t s
      applySingleSubst (T_Defined n ts) s      = T_Defined n $ map 
                                                  (flip applySingleSubst s)
                                                  ts


  instance LC.Value Value
