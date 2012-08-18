module Languages.EnrichedLambda.PrettyPrint () where
  import Languages.EnrichedLambda.Syntax

  showBindings []            = []
  showBindings ((v, e):bs) = "\n\t" ++ v ++ " = function " ++ show e ++ showBindings bs
  
  instance Show Constant where
    show (C_Int n) = show n
    show C_True    = "True"
    show C_False   = "False"
    show C_Nil     = "[]"
    show C_Unit    = "()"
  
  instance Show UnaryPrim where
    show U_Not   = "~"
    show U_Ref   = "!"
    show U_Deref = "&"
    show U_Fst   = "π1"
    show U_Snd   = "π2"
    show U_Head  = "head"
    show U_Tail  = "tail"
    show U_Empty = "empty?"
  
  instance Show BinaryPrim where
    show B_Eq     = "=="
    show B_Plus   = "+"
    show B_Minus  = "-"
    show B_Mult   = "*"
    show B_Div    = "/"
    show B_Assign = ":="
  
  instance Show Expr where
    show (E_Val s)          = s
    show (E_UPrim up)       = show up
    show (E_BPrim bp)       = show bp
    show (E_Const c)        = show c
    show (E_Location n)     = "Mem@" ++ show n
    show (E_Cons e1 e2)     = show e1 ++ " :: " ++ show e2
    show (E_ITE e1 e2 e3)   = "if ( " ++ show e1 ++ " ) then { " ++ show e2 ++ " } else { " ++ show e3 ++ " }"
    show (E_Seq e1 e2)      = show e1 ++ "; " ++ show e2
    show (E_Pair e1 e2)     = "( " ++ show e1 ++ ", " ++ show e2 ++  " )"
    show (E_Let v e1 e2)    = "let\n\t" ++ v ++ " = " ++ show e1 ++ "\nin\n\t" ++ show e2
    show (E_LetRec lrbs e2) = "letrec\n\t" ++ showBindings lrbs ++ "\nin\n\t" ++ show e2
    show (E_Apply e1 e2)    = 
      case e1 of 
        E_BPrim bp -> show e2 ++ " " ++ show bp
        _          -> show e1 ++ " " ++ show e2
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

  instance Show Definition where 
    show (D_Let v e)    = "let " ++ v ++ " = " ++ show e
    show (D_LetRec lrbs) = "letrec " ++ showBindings lrbs

  instance Show Instruction where
    show (IDF df) = show df
    show (IEX ex) = show ex

    showList []     c = c
    showList (i:is) c = show i ++ ";;\n" ++ showList is c
