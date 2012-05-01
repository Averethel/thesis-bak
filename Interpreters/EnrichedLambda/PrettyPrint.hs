module Interpreters.EnrichedLambda.PrettyPrint () where
  import Interpreters.EnrichedLambda.Syntax
  
  instance Show Constant where
    show (C_Int n) = show n
    show C_True    = "True"
    show C_False   = "False"
    show C_Nil     = "[]"
    show C_Unit    = "()"
  
  instance Show Expr where
    show (E_Var s)          = s
    show (E_Const c)        = show c
    show (E_Location n)     = "Mem@" ++ show n
    show (E_Not e)          = "~(" ++ show e ++ ")"
    show (E_Ref e)          = "!(" ++ show e ++ ")"
    show (E_Deref e)        = "&(" ++ show e ++ ")"
    show (E_Eq e1 e2)       = show e1 ++ " == " ++ show e2
    show (E_Plus e1 e2)     = show e1 ++ " + " ++ show e2
    show (E_Minus e1 e2)    = show e1 ++ " - " ++ show e2
    show (E_Div e1 e2)      = show e1 ++ " / " ++ show e2
    show (E_Mult e1 e2)     = show e1 ++ " * " ++ show e2
    show (E_Assign e1 e2)   = show e1 ++ " := " ++ show e2
    show (E_Head e)         = "head " ++ show e
    show (E_Tail e)         = "tail " ++ show e
    show (E_Cons e1 e2)     = show e1 ++ " :: " ++ show e2
    show (E_ITE e1 e2 e3)   = "if ( " ++ show e1 ++ " ) then { " ++ show e2 ++ " } else { " ++ show e3 ++ " }"
    show (E_Seq e1 e2)      = show e1 ++ "; " ++ show e2
    show (E_Fst e)          = "first " ++ show e
    show (E_Snd e)          = "second " ++ show e
    show (E_Pair e1 e2)     = "( " ++ show e1 ++ ", " ++ show e2 ++  " )"
    show (E_Let v e1 e2)    = "let\n\t" ++ v ++ " = " ++ show e1 ++ "\nin\n\t" ++ show e2
    show (E_Letrec v e1 e2) = "letrec\n\t" ++ v ++ " = " ++ show e1 ++ "\nin\n\t" ++ show e2
    show (E_Apply e1 e2)    = show e1 ++ " " ++ show e2
    show (E_Function s e)   = "λ" ++ s ++ "." ++ show e
    show E_MatchFailure     = "Match Failure"
    show Null               = "_"
  
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
  
