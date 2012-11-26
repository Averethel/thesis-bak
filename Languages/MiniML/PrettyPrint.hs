module Languages.MiniML.PrettyPrint where
  import Utils.Iseq

  import Languages.MiniML.Syntax

  instance Show MiniMLName where
    show MiniML = "Mini ML"

  isInfix :: BinaryPrim -> Bool
  isInfix _ = True

  isAtomicPattern :: Pattern -> Bool
  isAtomicPattern P_Wildcard     = True
  isAtomicPattern (P_Val _)      = True
  isAtomicPattern (P_Const _)    = True
  isAtomicPattern _              = False

  isAtomicExpr :: Expr -> Bool
  isAtomicExpr (E_Val _)      = True
  isAtomicExpr (E_Const _)    = True
  isAtomicExpr (E_UPrim _)    = True
  isAtomicExpr _              = False

  isAtomicValue :: Value -> Bool
  isAtomicValue (V_UPrim _)    = True
  isAtomicValue (V_BPrim _)    = True
  isAtomicValue V_Unit         = True
  isAtomicValue (V_Int _)      = True
  isAtomicValue (V_Bool _)     = True
  isAtomicValue (V_List [])    = True
  isAtomicValue (V_List (_:_)) = False
  isAtomicValue (V_Tuple _)    = True
  isAtomicValue (V_Clo _ _)    = True
  isAtomicValue (V_Pointer _)     = True

  isAtomicKind :: Kind -> Bool
  isAtomicKind K_Type = True
  isAtomicKind _      = False

  isAtomicTypeExpr :: TypeExpr -> Bool
  isAtomicTypeExpr (TE_Arrow  _     _) = False
  isAtomicTypeExpr (TE_Constr (_:_) _) = False
  isAtomicTypeExpr _                   = True

  pprConstant :: Constant -> Iseq
  pprConstant (C_Int n) = iStr . show $ n
  pprConstant C_False   = iStr "false"
  pprConstant C_True    = iStr "true"
  pprConstant C_Nil     = iStr "[]"
  pprConstant C_Unit    = iStr "()"

  instance Show Constant where
    show = show . pprConstant

  pprUnaryPrim :: UnaryPrim -> Iseq
  pprUnaryPrim U_Not     = iStr "not"
  pprUnaryPrim U_Ref     = iStr "!"
  pprUnaryPrim U_Deref   = iStr "&"
  pprUnaryPrim U_I_Minus = iStr "-"
  pprUnaryPrim U_Fst     = iStr "fst"
  pprUnaryPrim U_Snd     = iStr "snd"
  pprUnaryPrim U_Empty   = iStr "empty?"
  pprUnaryPrim U_Head    = iStr "head"
  pprUnaryPrim U_Tail    = iStr "tail"

  instance Show UnaryPrim where
    show = show . pprUnaryPrim

  pprBinaryPrim :: BinaryPrim -> Iseq
  pprBinaryPrim B_Eq      = iStr "=="
  pprBinaryPrim B_I_Plus    = iStr "+"
  pprBinaryPrim B_I_Minus   = iStr "-"
  pprBinaryPrim B_I_Mult    = iStr "*"
  pprBinaryPrim B_I_Div     = iStr "/"
  pprBinaryPrim B_Assign  = iStr ":="

  instance Show BinaryPrim where
    show = show . pprBinaryPrim

  pprAPattern :: Pattern -> Iseq
  pprAPattern p
    | isAtomicPattern p = pprPattern p
    | otherwise         = iStr "(" `iAppend` pprPattern p `iAppend` iStr ")"

  pprPattern :: Pattern -> Iseq
  pprPattern P_Wildcard     =
    iStr "_"
  pprPattern (P_Val v)      =
    iStr v
  pprPattern (P_Const c)    =
    pprConstant c
  pprPattern (P_Tuple ps)   =
    iConcat [ iStr "(",  iInterleave (iStr ", ") $
              map pprAPattern ps, iStr ")" ]
  pprPattern (P_Cons p1 p2) =
    pprAPattern p1 `iAppend` iStr " :: " `iAppend` pprAPattern p2

  instance Show Pattern where
    show = show . pprPattern

  pprBinding :: String -> Binding -> Iseq
  pprBinding sep (p, e) =
    iConcat [ pprPattern p,
              iStr " ", iStr sep,
              iStr " ", pprExpr e]

  pprBindings :: [Binding] -> Iseq
  pprBindings bs =
    iInterleave (iConcat [iNewline, iStr "| "]) $
    map (pprBinding "->") bs

  pprLetBindings :: [Binding] -> Iseq
  pprLetBindings bs =
    iInterleave sep $ map (pprBinding "=") bs where
      sep = iConcat [ iStr " and", iNewline ]

  pprFunBinding :: FunBinding -> Iseq
  pprFunBinding (p, e, E_Const C_True) =
    iConcat [ pprPattern p,
              iStr " -> ",
              pprExpr e ]
  pprFunBinding (p, e, g) =
    iConcat [ pprPattern p,
              iStr " {",
              pprExpr g,
              iStr "} -> ",
              pprExpr e ]

  pprFunBindings :: [FunBinding] -> Iseq
  pprFunBindings bs =
    iInterleave sep $ map pprFunBinding bs where
      sep = iConcat [ iStr " |", iNewline ]

  pprLetRecBinding :: LetRecBinding -> Iseq
  pprLetRecBinding (vn, e) =
    iConcat [ iStr vn, iStr " ",
              iIndent $ iConcat [
              iStr "= ", pprExpr e ]]

  pprLetRecBindings :: [LetRecBinding] -> Iseq
  pprLetRecBindings bs =
    iInterleave sep $ map pprLetRecBinding bs where
      sep = iConcat [ iStr " and", iNewline ]

  pprAExpr :: Expr -> Iseq
  pprAExpr e
    | isAtomicExpr e = pprExpr e
    | otherwise      = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

  pprExpr :: Expr -> Iseq
  pprExpr (E_UPrim up)                            =
    pprUnaryPrim up
  pprExpr (E_BPrim bp)                            =
    pprBinaryPrim bp
  pprExpr (E_Val v)                               =
    iStr v
  pprExpr (E_Const c)                             =
    pprConstant c
  pprExpr (E_Apply (E_Apply (E_BPrim bp) e1) e2)
    | isInfix bp                                  =
      iConcat [ pprAExpr e1, iStr " ", pprBinaryPrim bp,
                iStr " ", pprAExpr e2 ]
  pprExpr (E_Apply e1 e2)                         =
    pprAExpr e1 `iAppend` iStr " " `iAppend` pprAExpr e2
  pprExpr (E_Cons e1 e2)                          =
    pprAExpr e1 `iAppend` iStr " :: " `iAppend` pprAExpr e2
  pprExpr (E_Tuple es)                            =
    iConcat [ iStr "(",  iInterleave (iStr ", ") $
              map pprExpr es, iStr ")" ]
  pprExpr (E_And e1 e2)                           =
    iConcat [ pprAExpr e1, iStr " && ",
              pprAExpr e2 ]
  pprExpr (E_Or e1 e2)                            =
    iConcat [ pprAExpr e1, iStr " || ",
              pprAExpr e2 ]
  pprExpr (E_ITE e1 e2 e3)                        =
    iConcat [ iStr "if ", pprAExpr e1, iStr " then", iNewline,
              indentation, iIndent $ pprExpr e2, iNewline,
              iStr "else",
              indentation, iIndent $ pprExpr e3 ]
  pprExpr (E_Case e bs)                           =
    iConcat [ iStr "case ", pprAExpr e, iStr " of", iNewline,
              indentation, iIndent $ pprBindings bs]
  pprExpr (E_Seq e1 e2)                           =
    iConcat [ iNewline, indentation, iIndent $ pprExpr e1, iStr ";",
              iNewline, indentation, iIndent $ pprExpr e2]
  pprExpr (E_Function bs)                         =
    iConcat [ iStr "functio", iIndent $
              iConcat [ iStr "n ", iNewline,
                        pprFunBindings bs]]
  pprExpr (E_Let bs e)                            =
    iConcat [ iStr "let", iNewline,
              indentation, iIndent $ pprLetBindings bs,
              iNewline, iStr "in", iNewline,
              indentation, iIndent $ pprExpr e ]
  pprExpr (E_LetRec lrbs e)                       =
    iConcat [ iStr "let", iNewline,
              indentation, iIndent $ pprLetRecBindings lrbs,
              iNewline, iStr "in", iNewline,
              indentation, iIndent $ pprExpr e ]
  pprExpr E_MatchFailure                          =
    iStr "matchFailure"
  pprExpr (E_FatBar e1 e2)                        =
    iConcat [ pprAExpr e1, iStr "⌷", pprAExpr e2]

  instance Show Expr where
    show = show . pprExpr

  pprAValue :: Value -> Iseq
  pprAValue v
    | isAtomicValue v = pprValue v
    | otherwise       = iStr "(" `iAppend` pprValue v `iAppend` iStr ")"

  pprValue :: Value -> Iseq
  pprValue (V_UPrim up)     = iConcat [ iStr "<",
                                        pprUnaryPrim up,
                                        iStr ">" ]
  pprValue (V_BPrim bp)     = iConcat [ iStr "<",
                                        pprBinaryPrim bp,
                                        iStr ">" ]
  pprValue V_Unit           = iStr . show $ ()
  pprValue (V_Int n)        = iStr . show $ n
  pprValue (V_Bool b)       = iStr . show $ b
  pprValue (V_List [])      = iStr "[]"
  pprValue (V_List (v:vs))  = pprAValue v `iAppend`
                              iStr "::" `iAppend`
                              (pprAValue $ V_List vs)
  pprValue (V_Tuple vs)     = iConcat [ iStr "(",
                                        iInterleave (iStr ", ") $
                                        map pprValue vs,
                                        iStr ")" ]
  pprValue (V_Clo _ _)      = iStr "Function."
  pprValue V_Null           = iNil
  pprValue (V_Pointer n)    = (iStr "Mem@") `iAppend`
                              (iStr $ show n)

  instance Show Value where
    show = show . pprValue

  pprDefinition :: Definition -> Iseq
  pprDefinition (D_Let bs)      =
    iConcat [ iStr "let", iNewline, indentation,
              iIndent $ pprLetBindings bs ]
  pprDefinition (D_LetRec lrbs) =
    iConcat [ iStr "let", iNewline, indentation,
              iIndent $ pprLetRecBindings lrbs ]

  instance Show Definition where
    show = show . pprDefinition

  pprInstruction :: Instruction -> Iseq
  pprInstruction (IDF df) =
    pprDefinition df
  pprInstruction (IEX ex) =
    pprExpr ex

  pprProgram :: Program -> Iseq
  pprProgram ins =
    iInterleave sep $ map pprInstruction ins where
      sep = iConcat [iStr ";;", iNewline]

  instance Show Instruction where
    show     = show . pprInstruction
    showList p _ = show . pprProgram $ p

  pprAKind :: Kind -> Iseq
  pprAKind k
    | isAtomicKind k = pprKind k
    | otherwise      = iStr "(" `iAppend` pprKind k `iAppend` iStr ")"

  pprKind :: Kind -> Iseq
  pprKind K_Type          =
    iStr "*"
  pprKind (K_Arrow k1 k2) =
    pprAKind k1 `iAppend` iStr " -> " `iAppend` pprKind k2

  instance Show Kind where
    show = show . pprKind

  pprTypeConstr :: TypeConstr -> Iseq
  pprTypeConstr Int  = iStr "Int"
  pprTypeConstr Bool = iStr "Bool"
  pprTypeConstr Unit = iStr "Unit"
  pprTypeConstr List = iStr "List"
  pprTypeConstr Ref  = iStr "Ref"

  instance Show TypeConstr where
    show = show . pprTypeConstr

  pprATypeExpr :: TypeExpr -> Iseq
  pprATypeExpr te
    | isAtomicTypeExpr te = pprTypeExpr te
    | otherwise           = iStr "(" `iAppend` pprTypeExpr te `iAppend` iStr ")"

  pprTypeExpr :: TypeExpr -> Iseq
  pprTypeExpr (TE_Var v)          =
    iStr v
  pprTypeExpr (TE_Arrow te1 te2)  =
    pprATypeExpr te1 `iAppend` iStr " -> " `iAppend` pprTypeExpr te2
  pprTypeExpr (TE_Tuple ts)       =
    iConcat [ iStr "(", iInterleave
              (iStr ", ") $ map pprTypeExpr
              ts, iStr ")"]
  pprTypeExpr (TE_Constr te tc)   =
    iConcat [ iInterleave (iStr " ") $
              map pprATypeExpr te,
              sep, pprTypeConstr tc] where
                sep = case te of
                  [] -> iNil
                  _  -> iStr " "

  instance Show TypeExpr where
    show = show . pprTypeExpr
