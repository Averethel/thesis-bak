module Compiler.Passes.FoldTuples (foldTuples) where
  import Languages.MiniML.Syntax

  patternTupleToPairs :: [Pattern] -> Pattern
  patternTupleToPairs [a, b] =
    P_Tuple [a, b]
  patternTupleToPairs (a:as) =
    let as' = patternTupleToPairs as
    in P_Tuple [a, as']    

  foldTuplesPattern :: Pattern -> Pattern
  foldTuplesPattern (P_Tuple ps)   =
    patternTupleToPairs ps
  foldTuplesPattern (P_Cons p1 p2) =
    P_Cons (foldTuplesPattern p1) (foldTuplesPattern p2)
  foldTuplesPattern p = 
    p

  tupleToPairs :: [Expr] -> Expr
  tupleToPairs [a, b] =
    E_Tuple [a, b] 
  tupleToPairs (a:as) = 
    let as' = tupleToPairs as
    in E_Tuple [a, as']

  foldTuplesLetBindings :: [Binding] -> [Binding]
  foldTuplesLetBindings = map (\(p, e) -> (foldTuplesPattern p, 
                                              foldTuplesExpression e))

  foldTuplesBindings :: [FunBinding] -> [FunBinding]
  foldTuplesBindings = map (\(p, e, g) -> (foldTuplesPattern p, 
                                             foldTuplesExpression e,
                                             foldTuplesExpression g))

  foldTuplesLetrecBindings :: [LetRecBinding] -> [LetRecBinding]
  foldTuplesLetrecBindings = map (\(n, e) -> (n, foldTuplesExpression e))

  foldTuplesExpression :: Expr -> Expr
  foldTuplesExpression (E_Tuple ps) =
    tupleToPairs ps
  foldTuplesExpression (E_Apply e1 e2)  =
    E_Apply (foldTuplesExpression e1) (foldTuplesExpression e2)
  foldTuplesExpression (E_Cons e1 e2)   =
    E_Cons (foldTuplesExpression e1) (foldTuplesExpression e2)
  foldTuplesExpression (E_And e1 e2)    =
    E_And (foldTuplesExpression e1) (foldTuplesExpression e2)
  foldTuplesExpression (E_Or e1 e2)     =
    E_Or (foldTuplesExpression e1) (foldTuplesExpression e2)
  foldTuplesExpression (E_ITE e1 e2 e3) =
    E_ITE (foldTuplesExpression e1) (foldTuplesExpression e2) 
          (foldTuplesExpression e3)
  foldTuplesExpression (E_Seq e1 e2)    =
    E_Seq (foldTuplesExpression e1) (foldTuplesExpression e2)
  foldTuplesExpression (E_Function bs)  =
    E_Function (foldTuplesBindings bs)
  foldTuplesExpression (E_Let bs e)     =
    E_Let (foldTuplesLetBindings bs) (foldTuplesExpression e)
  foldTuplesExpression (E_LetRec bs e)  =
    E_LetRec (foldTuplesLetrecBindings bs) (foldTuplesExpression e)
  foldTuplesExpression e                = 
    e

  foldTuplesDefinition :: Definition -> Definition
  foldTuplesDefinition (D_Let bs)    =
    D_Let $ foldTuplesLetBindings bs
  foldTuplesDefinition (D_LetRec bs) =
    D_LetRec $ foldTuplesLetrecBindings bs

  foldTuplesInstruction :: Instruction -> Instruction
  foldTuplesInstruction (IEX e) = 
    IEX $ foldTuplesExpression e
  foldTuplesInstruction (IDF d) =
    IDF $ foldTuplesDefinition d

  foldTuplesProgram :: Program -> Program
  foldTuplesProgram = map foldTuplesInstruction

  foldTuples = foldTuplesProgram
