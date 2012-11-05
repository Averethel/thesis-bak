{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.TranslationToEL (programToEnrichedLambda) where
  import qualified Languages.MiniML.Syntax as ML
  import qualified Languages.EnrichedLambda.Syntax as EL

  import Control.Monad.State

  -- Assumptions:
  -- 1. Tuples are folded to pairs
  -- 2. Wildcards are substituted with variables
  -- 3. Expressions are typechecked
  -- 4. Pattern matchings are simplified

  unaryPrimToEnrichedLambda :: ML.UnaryPrim -> EL.Expr
  unaryPrimToEnrichedLambda ML.U_Not     =
    EL.E_UPrim EL.U_Not
  unaryPrimToEnrichedLambda ML.U_Ref     =
    EL.E_UPrim EL.U_Ref
  unaryPrimToEnrichedLambda ML.U_Deref   =
    EL.E_UPrim EL.U_Deref
  unaryPrimToEnrichedLambda ML.U_I_Minus =
    EL.E_Apply (EL.E_BPrim EL.B_Minus) $ EL.E_Num 0
  unaryPrimToEnrichedLambda ML.U_Fst     =
    EL.E_UPrim EL.U_Fst
  unaryPrimToEnrichedLambda ML.U_Snd     =
    EL.E_UPrim EL.U_Snd
  unaryPrimToEnrichedLambda ML.U_Empty   =
    EL.E_UPrim EL.U_Empty
  unaryPrimToEnrichedLambda ML.U_Head    =
    EL.E_UPrim EL.U_Head
  unaryPrimToEnrichedLambda ML.U_Tail    =
    EL.E_UPrim EL.U_Tail

  binaryPrimToEnrichedLambda :: ML.BinaryPrim -> EL.Expr
  binaryPrimToEnrichedLambda ML.B_Eq      =
    EL.E_BPrim EL.B_Eq
  binaryPrimToEnrichedLambda ML.B_I_Plus  =
    EL.E_BPrim EL.B_Plus
  binaryPrimToEnrichedLambda ML.B_I_Minus =
    EL.E_BPrim EL.B_Minus
  binaryPrimToEnrichedLambda ML.B_I_Mult  =
    EL.E_BPrim EL.B_Mult
  binaryPrimToEnrichedLambda ML.B_I_Div   =
    EL.E_BPrim EL.B_Div
  binaryPrimToEnrichedLambda ML.B_Assign  =
    EL.E_BPrim EL.B_Assign

  constantToEnrichedLambda :: ML.Constant -> EL.Expr
  constantToEnrichedLambda (ML.C_Int n) =
    EL.E_Num n
  constantToEnrichedLambda (ML.C_False) =
    EL.E_Constr EL.boolTag EL.falseTag 0
  constantToEnrichedLambda (ML.C_True)  =
    EL.E_Constr EL.boolTag EL.trueTag 0
  constantToEnrichedLambda (ML.C_Nil)   =
    EL.E_Constr EL.listTag EL.nilTag 0
  constantToEnrichedLambda (ML.C_Unit)  =
    EL.E_Constr EL.unitTag EL.unitTagC 0

  bindingToEnrichedLambda :: ML.Binding -> EL.LetBinding
  bindingToEnrichedLambda (ML.P_Val n, e) = 
    (n, expressionToEnrichedLambda e)

  letrecBindingEnrichedLambda :: ML.LetRecBinding -> EL.LetBinding
  letrecBindingEnrichedLambda (v, e) =
    (v, expressionToEnrichedLambda e)

  bindingToClause :: ML.Binding -> EL.Clause
  bindingToClause (ML.P_Const ML.C_True, e)                 =
    (EL.boolTag, EL.trueTag, [], 
     expressionToEnrichedLambda e)
  bindingToClause (ML.P_Const ML.C_False, e)                =
    (EL.boolTag, EL.falseTag, [], 
     expressionToEnrichedLambda e)
  bindingToClause (ML.P_Const ML.C_Unit, e)                 =
    (EL.unitTag, EL.unitTagC, [], 
     expressionToEnrichedLambda e)
  bindingToClause (ML.P_Const ML.C_Nil, e)                  =
    (EL.listTag, EL.nilTag, [], 
     expressionToEnrichedLambda e)
  bindingToClause (ML.P_Cons (ML.P_Val x) (ML.P_Val xs), e) =
    (EL.listTag, EL.consTag, [x, xs], 
     expressionToEnrichedLambda e)
  bindingToClause (ML.P_Tuple [ML.P_Val a, ML.P_Val b], e)  =
    (EL.pairTag, EL.pairTagC, [a, b], 
     expressionToEnrichedLambda e)

  expressionToEnrichedLambda :: ML.Expr -> EL.Expr
  expressionToEnrichedLambda (ML.E_UPrim up)       =
    unaryPrimToEnrichedLambda up
  expressionToEnrichedLambda (ML.E_BPrim bp)       =
    binaryPrimToEnrichedLambda bp
  expressionToEnrichedLambda (ML.E_Val vn)         =
    EL.E_Val vn
  expressionToEnrichedLambda (ML.E_Const c)        =
    constantToEnrichedLambda c
  expressionToEnrichedLambda (ML.E_Apply e1 e2)    =
    let
      e1' = expressionToEnrichedLambda e1
      e2' = expressionToEnrichedLambda e2
    in
      EL.E_Apply e1' e2'
  expressionToEnrichedLambda (ML.E_Cons e1 e2)     =
    let 
      e1' = expressionToEnrichedLambda e1
      e2' = expressionToEnrichedLambda e2
    in
      EL.E_Apply 
        (EL.E_Apply 
          (EL.E_Constr EL.listTag EL.consTag 2) 
          e1') 
        e2'
  expressionToEnrichedLambda (ML.E_Tuple [e1, e2]) =
    let
      e1' = expressionToEnrichedLambda e1
      e2' = expressionToEnrichedLambda e2
    in
      EL.E_Apply 
        (EL.E_Apply 
          (EL.E_Constr EL.pairTag EL.pairTagC 2) 
          e1') 
        e2'
  expressionToEnrichedLambda (ML.E_And e1 e2)      =
    let
      e1' = expressionToEnrichedLambda e1
      e2' = expressionToEnrichedLambda e2
    in
      EL.E_Apply (EL.E_Apply (EL.E_BPrim EL.B_And) e1') e2'
  expressionToEnrichedLambda (ML.E_Or e1 e2)       =
    let 
      e1' = expressionToEnrichedLambda e1
      e2' = expressionToEnrichedLambda e2
    in
      EL.E_Apply (EL.E_Apply (EL.E_BPrim EL.B_Or) e1') e2'
  expressionToEnrichedLambda (ML.E_ITE e1 e2 e3)   =
    let 
      e1' = expressionToEnrichedLambda e1
      e2' = expressionToEnrichedLambda e2
      e3' = expressionToEnrichedLambda e3
    in
      EL.E_Case e1' [(EL.boolTag, EL.trueTag,  [], e2'), 
                     (EL.boolTag, EL.falseTag, [], e3')]
  expressionToEnrichedLambda (ML.E_Case e bs)      =
    let
      e'  = expressionToEnrichedLambda e
      bs' = map bindingToClause bs
    in
      EL.E_Case e' bs'
  expressionToEnrichedLambda (ML.E_Seq e1 e2)      =
    let
      e1' = expressionToEnrichedLambda e1
      e2' = expressionToEnrichedLambda e2
    in
      EL.E_Seq e1' e2'
  expressionToEnrichedLambda (ML.E_Function 
             [(ML.P_Val v, e, ML.E_Const ML.C_True)]) =
    EL.E_Function v $ expressionToEnrichedLambda e
  expressionToEnrichedLambda (ML.E_Let bs e)       =
    let
      bs' = map bindingToEnrichedLambda bs
      e'  = expressionToEnrichedLambda e
    in
      EL.E_Let bs' e'
  expressionToEnrichedLambda (ML.E_LetRec lrbs e)  =
    let
      lrbs' = map letrecBindingEnrichedLambda lrbs
      e'    = expressionToEnrichedLambda e
    in
      EL.E_LetRec lrbs' e'
  expressionToEnrichedLambda ML.E_MatchFailure     =
    EL.E_MatchFailure
  expressionToEnrichedLambda (ML.E_FatBar e1 e2)   =
    let
      e1' = expressionToEnrichedLambda e1
      e2' = expressionToEnrichedLambda e2
    in
      EL.E_Rescue e1' e2'
  expressionToEnrichedLambda ML.Null               =
    EL.Null

  bindingToDefinition :: ML.Binding -> EL.Binding
  bindingToDefinition (ML.P_Val n, ML.E_Function 
                [(ML.P_Val v, e, ML.E_Const ML.C_True)]) =
    (n, [v], expressionToEnrichedLambda e)
  bindingToDefinition (ML.P_Val n, e)                  =
    (n, [], expressionToEnrichedLambda e)

  letrecBindingToDefinition :: ML.LetRecBinding -> EL.Binding
  letrecBindingToDefinition (n, ML.E_Function 
                 [(ML.P_Val v, e, ML.E_Const ML.C_True)]) =
    (n, [v], expressionToEnrichedLambda e)
  letrecBindingToDefinition (n, e)                     =
    (n, [], expressionToEnrichedLambda e)


  definitionToEnrichedLambda :: ML.Definition -> EL.Definition
  definitionToEnrichedLambda (ML.D_Let bs)      =
    EL.D_Let $ map bindingToDefinition bs
  definitionToEnrichedLambda (ML.D_LetRec lrbs) =
    EL.D_LetRec $ map letrecBindingToDefinition lrbs

  progPairToEnrichedLambda :: ([ML.Definition], ML.Expr) -> EL.Program
  progPairToEnrichedLambda (dfs, e) =
    (map definitionToEnrichedLambda dfs, expressionToEnrichedLambda e)

  definitionsToFront :: ML.Program -> ([ML.Definition], ML.Expr)
  definitionsToFront = definitionsToFront' [] [] where
    definitionsToFront' :: [ML.Definition] -> [ML.Expr] -> ML.Program -> ([ML.Definition], ML.Expr)
    definitionsToFront' dfs []     []               = 
      (reverse dfs, ML.Null)
    definitionsToFront' dfs (e:es) []               = 
      (reverse dfs, foldr (\e -> ML.E_Let [(ML.P_Val "it", e)]) e $ reverse es)
    definitionsToFront' dfs es     ((ML.IDF df):is) =
      definitionsToFront' (df:dfs) es is
    definitionsToFront' dfs es     ((ML.IEX ex):is) =
      definitionsToFront' dfs (ex:es) is

  programToEnrichedLambda :: ML.Program -> EL.Program
  programToEnrichedLambda prog = 
    progPairToEnrichedLambda $ definitionsToFront prog
