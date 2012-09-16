{-# LANGUAGE 
  FlexibleInstances,
  TypeSynonymInstances
  #-}

module Languages.EnrichedLambda.PrettyPrint () where 
  import Utils.Iseq
  import Languages.EnrichedLambda.Syntax

  pprUnaryPrim :: UnaryPrim -> Iseq
  pprUnaryPrim U_Not    = iStr "not"
  pprUnaryPrim U_Ref    = iStr "!"
  pprUnaryPrim U_Deref  = iStr "&"
  pprUnaryPrim U_Head   = iStr "hd"
  pprUnaryPrim U_Tail   = iStr "tl"
  pprUnaryPrim U_Fst    = iStr "fst"
  pprUnaryPrim U_Snd    = iStr "snd"
  
  instance Show UnaryPrim where
    show = show . pprUnaryPrim

  pprBinaryPrim :: BinaryPrim -> Iseq
  pprBinaryPrim B_Eq      = iStr "=="
  pprBinaryPrim B_Plus    = iStr "+"
  pprBinaryPrim B_Minus   = iStr "-"
  pprBinaryPrim B_Mult    = iStr "*"
  pprBinaryPrim B_Div     = iStr "/"
  pprBinaryPrim B_Assign  = iStr ":="
  pprBinaryPrim B_And     = iStr "&&"
  pprBinaryPrim B_Or      = iStr "||"

  instance Show BinaryPrim where
    show = show . pprBinaryPrim

  pprAExpr :: Expr -> Iseq
  pprAExpr e
    | isAtomicExpr e = pprExpr e
    | otherwise      = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

  pprLetBindings :: [LetBinding] -> Iseq
  pprLetBindings defns = iInterleave sep (map pprLetBinding defns) where
    sep = iConcat [ iStr " and", iNewline ]
  
  pprLetBinding :: LetBinding -> Iseq
  pprLetBinding (n, e) = iConcat [ iStr n, iStr " = ", iIndent (pprExpr e) ]

  pprClause :: Clause -> Iseq
  pprClause (tag, vars, expr) = 
    iConcat [iStr "<", iStr $ show tag, 
             iInterleave (iStr " ") $ (iStr ">") : map iStr vars,
             iStr " -> ", pprExpr expr]

  pprClauses :: [Clause] -> Iseq
  pprClauses cls = iInterleave iNewline (map pprClause cls)

  pprExpr :: Expr -> Iseq
  pprExpr (E_UPrim up)                           =
    pprUnaryPrim up
  pprExpr (E_BPrim bp)                           =
    pprBinaryPrim bp
  pprExpr (E_Val ident)                          =
    iStr ident
  pprExpr (E_Num n)                              =
    iStr $ show n
  pprExpr (E_Location n)                         =
    (iStr "Mem@") `iAppend` (iStr $ show n)
  pprExpr (E_Constr tp t a)                      =
    iStr "Pack{" `iAppend` 
    iInterleave (iStr ",") [iStr $ show tp, iStr $ show t, iStr $ show a] `iAppend`
    iStr "}"
  pprExpr (E_Seq e1 e2)                          =
    iConcat [ iNewline, indentation, iIndent $ pprExpr e1, iStr ";", 
              iNewline, indentation, iIndent $ pprExpr e2]
  pprExpr (E_Apply (E_Apply (E_BPrim bp) e1) e2)
    | isInfix bp                                 =
      iConcat [ pprAExpr e1, iStr " ", pprBinaryPrim bp, 
                iStr " ", pprAExpr e2]
  pprExpr (E_Apply e1 e2)                        =
    (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
  pprExpr (E_Let bs e)                           =
    iConcat [ iStr "let", iNewline,
              indentation, iIndent $ pprLetBindings bs,
              iNewline, iStr "in", iNewline,
              indentation, iIndent $ pprExpr e ]
  pprExpr (E_LetRec bs e)                        =
    iConcat [ iStr "letrec", iNewline,
              indentation, iIndent $ pprLetBindings bs,
              iNewline, iStr "in ",
              indentation, iIndent $ pprExpr e ]
  pprExpr (E_Case e cs)                          =
    iConcat [ iStr "case ", pprExpr e, iStr " of", iNewline,
              indentation, iIndent $ pprClauses cs]
  pprExpr (E_Function var e)                     =
    iConcat [ iStr "function ", iStr var, 
              iStr " -> ", pprExpr e]
  pprExpr E_MatchFailure                         =
    iStr "Match_Failure"
  pprExpr Null                                   = 
    iNil

  instance Show Expr where
    show = show . pprExpr

  pprBinding :: Binding -> Iseq
  pprBinding (ident, vars, e) =
    iConcat [ iStr ident, 
              iInterleave (iStr " ") $ map iStr vars,
              iStr " = ", pprExpr e]

  pprBindings :: Bool -> [Binding] -> Iseq
  pprBindings recursive bs = iInterleave sep $ map pprBinding bs where
    sep
      | not recursive = iNewline `iAppend` iStr "and "
      | recursive     = iNewline `iAppend` iStr "   and "

  pprDefinition :: Definition -> Iseq
  pprDefinition (D_Let bs) = 
    iConcat [ iStr "let ", pprBindings False bs]
  pprDefinition (D_LetRec bs) = 
    iConcat [ iStr "letrec ", pprBindings False bs]

  instance Show Definition where
    show = show . pprDefinition

  pprProgram :: Program -> Iseq
  pprProgram (defs, expr) = 
    iConcat [ iInterleave sep (map pprDefinition defs), sep,
              pprExpr expr, sep] where
                sep = iConcat [iStr ";;", iNewline]

  instance Show Program where
    show = show . pprProgram

  -- Think about how to handle types here
  --pprAType :: Type -> Iseq
  --pprType :: Type -> Iseq
  --pprType (T_Var s)       = 
  --  iStr s
  --pprType (T_Arrow t1 t2) = 
  --  pprType t1 `iAppend` iStr " -> " `iAppend` pprAType t2
  --pprType (T_Ref t)       =
  --  iStr "Ref " $
  ----  | T_Defined Type_Tag [Type]
  ----  deriving Eq






