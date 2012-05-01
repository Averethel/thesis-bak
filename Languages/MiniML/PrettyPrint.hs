module Languages.MiniML.PrettyPrint () where
  import Languages.MiniML.Syntax

  showTuple []     = ""
  showTuple [x]    = show x
  showTuple (x:xs) = show x ++ ", " ++ showTuple xs

  showMatchings []         = ""
  showMatchings ((p,e):ms) = "\n\t|" ++ show p ++ " -> " ++ show e ++ showMatchings ms

  showBindings []            = []
  showBindings ((vn, ms):bs) = "\n\t" ++ vn ++ " = " ++ showMatchings ms ++ showBindings bs

  instance Show Constant where
    show (C_Int n) = show n
    show C_False   = "false"
    show C_True    = "true"
    show C_Nil     = "[]"
    show C_Unit    = "()"

  instance Show UnaryPrim where
    show U_Not     = "~"
    show U_Ref     = "!"
    show U_Deref   = "&"
    show U_I_Minus = "-"

  instance Show BinaryPrim where
    show B_Eq      = "=="
    show B_I_Plus  = "+"
    show B_I_Minus = "-"
    show B_I_Mult  = "*"
    show B_I_Div   = "/"
    show B_Assign  = ":="

  instance Show Pattern where
    show (P_Val vn)     = vn
    show P_Wildcard     = "_"
    show (P_Const c)    = show c
    show (P_Tuple ps)   = "( " ++ showTuple ps ++ " )"
    show (P_Cons p1 p2) = show p1 ++ " :: ( " ++ show p2 ++ " )"

  instance Show Expr where
    show (E_UPrim up)       = show up
    show (E_BPrim bp)       = show bp
    show (E_Val vn)         = vn
    show (E_Const c)        = show c
    show (E_Location l)     = "Mem@?" ++ show l
    show (E_Apply e1 e2)    =
      case e1 of
        E_BPrim bp         -> show e2 ++ " " ++ show bp
        _                  -> show e1 ++ " " ++ show e2
    show (E_Cons e1 e2)     = show e1 ++ " :: " ++ show e2
    show (E_Tuple es)       = "( " ++ showTuple es ++ " )"
    show (E_And e1 e2)      = show e1 ++ " && " ++ show e2
    show (E_Or e1 e2)       = show e1 ++ " || " ++ show e2
    show (E_ITE e1 e2 e3)   = "if ( " ++ show e1 ++ " ) then { " ++ show e2 ++ " } else { " ++ show e3 ++ " }"
    show (E_Seq e1 e2)      = show e1 ++ "; " ++ show e2
    show (E_Function ms)    = "function " ++ showMatchings ms
    show (E_Let (p, e1) e2) = "let \n\t" ++ show p ++ " = " ++ show e1 ++ "\nin " ++ show e2
    show (E_LetRec bs e)    = "letrec " ++ showBindings bs ++ "\nin " ++ show e

  instance Show Definition where 
    show (D_Let (p, e)) = "let " ++ show p ++ " = " ++ show e
    show (D_LetRec bs)  = "letrec " ++ showBindings bs

    showList []     c = c
    showList (d:ds) c = show d ++ ";;\n" ++ showList ds c

  instance Show Instruction where
    show (IDF df) = show df
    show (IEX ex) = show ex

    showList []     c = c
    showList (i:is) c = show i ++ ";;\n" ++ showList is c

  instance Show Kind where
    show K_Type          = "*"
    show (K_Arrow k1 k2) = "( " ++ show k1 ++ " ) => " ++ show k2

  instance Show TypeExpr where
    show (TE_Var tv)                       = tv
    show (TE_Arrow te1@(TE_Arrow _ _) te2) = "( " ++ show te1 ++ " ) -> " ++ show te2
    show (TE_Arrow te1 te2)                = show te1 ++ " -> " ++ show te2
    show (TE_Tuple tes)                    = "( " ++ showTuple tes ++ " )"
    show (TE_Constr [] tc)                 = show tc
    show (TE_Constr (t:ts) tc)             = show t ++ " " ++ show (TE_Constr ts tc)
