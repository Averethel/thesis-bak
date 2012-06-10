module Languages.EnrichedLambdaLowLevelTypes.PrettyPrint () where
  import Languages.EnrichedLambdaLowLevelTypes.Syntax
  
  instance Show Struct where
    show (S_Ref e)                = "[ " ++ show e ++ " ]"
    show (S_Str Tg_True _ _)      = "True"
    show (S_Str Tg_False _ _)     = "False"
    show (S_Str Tg_Unit _ _)      = "()"
    show (S_Str Tg_Nil _ _)       = "[]"
    show (S_Str Tg_Cons _ [a, b]) = show a ++ " :: " ++ show b
    show (S_Str Tg_Pair _ [a, b]) = "( " ++ show a ++ ", " ++ show b ++ " )"
    show (S_Ptr a)                = "Pointer: " ++ show a
    show (S_StaticPtr a)          = "Static pointer: " ++ show a
    show (S_Int n)                = show n
    show Null                     = "_"
  
  instance Show Prim where
    show P_AllocPair = "mkPair"
    show P_AllocList = "mkList"
    show P_Not       = "~"
    show P_Ref       = "!"
    show P_Deref     = "&"
    show P_Fst       = "π1"
    show P_Snd       = "π2"
    show P_Head      = "head"
    show P_Tail      = "tail"
    show P_Empty     = "empty?"
    show P_Eq        = "=="
    show P_Plus      = "+"
    show P_Minus     = "-"
    show P_Mult      = "*"
    show P_Div       = "/"
    show P_Assign    = ":="
  
  instance Show Expr where
    show (E_Prim p)         = show p
    show (E_Val s)          = s
    show (E_Struct s)       = show s
    show (E_ITE e1 e2 e3)   = "if ( " ++ show e1 ++ " ) then { " ++ show e2 ++ " } else { " ++ show e3 ++ " }"
    show (E_Seq e1 e2)      = show e1 ++ "; " ++ show e2
    show (E_Let s e1 e2)    = "let\n\t" ++ s ++ " = " ++ show e1 ++ "\nin\n\t" ++ show e2
    show (E_Letrec s e1 e2) = "letrec\n\t" ++ s ++ " = " ++ show e1 ++ "\nin\n\t" ++ show e2
    show (E_Apply e1@(E_Prim p) e2)
      | is_binary p         = show e2 ++ " " ++ show e1
      | otherwise           = show e1 ++ " " ++ show e2
    show (E_Apply e1 e2)    = show e1 ++ " " ++ show e2
    show (E_Function s e)   = "λ" ++ s ++ "." ++ show e
    show E_MatchFailure     = "Match Failure"
  
  instance Show Type where
    show (T_Var s)                     = s
    show T_Int                         = "Int"
    show T_Bool                        = "Bool"
    show T_Unit                        = "Unit"
    show (T_Pair t1 t2)                = "( " ++ show t1 ++ ", " ++ show t2 ++ " )"
    show (T_List t)                    = show t ++ " List"
    show (T_Arrow t1@(T_Arrow _ _) t2) = "( " ++ show t1 ++ " ) -> " ++ show t2
    show (T_Arrow t1 t2)               = show t1 ++ " -> " ++ show t2
    show (T_Ref t)                     = show t ++ " Ref"
