{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.TranslationToEL (program_to_enriched_lambda) where
  import qualified Languages.MiniML.Syntax as ML
  import qualified Languages.EnrichedLambda.Syntax as EL

  import Control.Monad.State

  -- Assumptions:
  -- 1. Tuples are folded to pairs
  -- 2. Wildcards are substituted with variables
  -- 3. Expressions are typechecked
  -- 4. Pattern matchings are simplified

  unary_prim_to_enriched_lambda :: ML.UnaryPrim -> EL.Expr
  unary_prim_to_enriched_lambda ML.U_Not     =
    EL.E_UPrim EL.U_Not
  unary_prim_to_enriched_lambda ML.U_Ref     =
    EL.E_UPrim EL.U_Ref
  unary_prim_to_enriched_lambda ML.U_Deref   =
    EL.E_UPrim EL.U_Deref
  unary_prim_to_enriched_lambda ML.U_I_Minus =
    EL.E_Apply (EL.E_BPrim EL.B_Minus) $ EL.E_Num 0
  unary_prim_to_enriched_lambda ML.U_Fst     =
    EL.E_UPrim EL.U_Fst
  unary_prim_to_enriched_lambda ML.U_Snd     =
    EL.E_UPrim EL.U_Snd
  unary_prim_to_enriched_lambda ML.U_Empty   =
    EL.E_UPrim EL.U_Empty
  unary_prim_to_enriched_lambda ML.U_Head    =
    EL.E_UPrim EL.U_Head
  unary_prim_to_enriched_lambda ML.U_Tail    =
    EL.E_UPrim EL.U_Tail

  binary_prim_to_enriched_lambda :: ML.BinaryPrim -> EL.Expr
  binary_prim_to_enriched_lambda ML.B_Eq      =
    EL.E_BPrim EL.B_Eq
  binary_prim_to_enriched_lambda ML.B_I_Plus  =
    EL.E_BPrim EL.B_Plus
  binary_prim_to_enriched_lambda ML.B_I_Minus =
    EL.E_BPrim EL.B_Minus
  binary_prim_to_enriched_lambda ML.B_I_Mult  =
    EL.E_BPrim EL.B_Mult
  binary_prim_to_enriched_lambda ML.B_I_Div   =
    EL.E_BPrim EL.B_Div
  binary_prim_to_enriched_lambda ML.B_Assign  =
    EL.E_BPrim EL.B_Assign

  constant_to_enriched_lambda :: ML.Constant -> EL.Expr
  constant_to_enriched_lambda (ML.C_Int n) =
    EL.E_Num n
  constant_to_enriched_lambda (ML.C_False) =
    EL.E_Constr EL.bool_tag EL.false_tag 0
  constant_to_enriched_lambda (ML.C_True)  =
    EL.E_Constr EL.bool_tag EL.true_tag 0
  constant_to_enriched_lambda (ML.C_Nil)   =
    EL.E_Constr EL.list_tag EL.nil_tag 0
  constant_to_enriched_lambda (ML.C_Unit)  =
    EL.E_Constr EL.unit_tag EL.unit_tag_c 0

  binding_to_enriched_lambda :: ML.Binding -> EL.LetBinding
  binding_to_enriched_lambda (ML.P_Val n, e) = 
    (n, expression_to_enriched_lambda e)

  letrec_binding_enriched_lambda :: ML.LetRecBinding -> EL.LetBinding
  letrec_binding_enriched_lambda (v, e) =
    (v, expression_to_enriched_lambda e)

  binding_to_clause :: ML.Binding -> EL.Clause
  binding_to_clause (ML.P_Const ML.C_True, e)                 =
    (EL.bool_tag, EL.true_tag, [], 
     expression_to_enriched_lambda e)
  binding_to_clause (ML.P_Const ML.C_False, e)                =
    (EL.bool_tag, EL.false_tag, [], 
     expression_to_enriched_lambda e)
  binding_to_clause (ML.P_Const ML.C_Unit, e)                 =
    (EL.unit_tag, EL.unit_tag_c, [], 
     expression_to_enriched_lambda e)
  binding_to_clause (ML.P_Const ML.C_Nil, e)                  =
    (EL.list_tag, EL.nil_tag, [], 
     expression_to_enriched_lambda e)
  binding_to_clause (ML.P_Cons (ML.P_Val x) (ML.P_Val xs), e) =
    (EL.list_tag, EL.cons_tag, [x, xs], 
     expression_to_enriched_lambda e)
  binding_to_clause (ML.P_Tuple [ML.P_Val a, ML.P_Val b], e)  =
    (EL.pair_tag, EL.pair_tag_c, [a, b], 
     expression_to_enriched_lambda e)

  expression_to_enriched_lambda :: ML.Expr -> EL.Expr
  expression_to_enriched_lambda (ML.E_UPrim up)       =
    unary_prim_to_enriched_lambda up
  expression_to_enriched_lambda (ML.E_BPrim bp)       =
    binary_prim_to_enriched_lambda bp
  expression_to_enriched_lambda (ML.E_Val vn)         =
    EL.E_Val vn
  expression_to_enriched_lambda (ML.E_Const c)        =
    constant_to_enriched_lambda c
  expression_to_enriched_lambda (ML.E_Apply e1 e2)    =
    let
      e1' = expression_to_enriched_lambda e1
      e2' = expression_to_enriched_lambda e2
    in
      EL.E_Apply e1' e2'
  expression_to_enriched_lambda (ML.E_Cons e1 e2)     =
    let 
      e1' = expression_to_enriched_lambda e1
      e2' = expression_to_enriched_lambda e2
    in
      EL.E_Apply 
        (EL.E_Apply 
          (EL.E_Constr EL.list_tag EL.cons_tag 2) 
          e1') 
        e2'
  expression_to_enriched_lambda (ML.E_Tuple [e1, e2]) =
    let
      e1' = expression_to_enriched_lambda e1
      e2' = expression_to_enriched_lambda e2
    in
      EL.E_Apply 
        (EL.E_Apply 
          (EL.E_Constr EL.pair_tag EL.pair_tag_c 2) 
          e1') 
        e2'
  expression_to_enriched_lambda (ML.E_And e1 e2)      =
    let
      e1' = expression_to_enriched_lambda e1
      e2' = expression_to_enriched_lambda e2
    in
      EL.E_Apply (EL.E_Apply (EL.E_BPrim EL.B_And) e1') e2'
  expression_to_enriched_lambda (ML.E_Or e1 e2)       =
    let 
      e1' = expression_to_enriched_lambda e1
      e2' = expression_to_enriched_lambda e2
    in
      EL.E_Apply (EL.E_Apply (EL.E_BPrim EL.B_Or) e1') e2'
  expression_to_enriched_lambda (ML.E_ITE e1 e2 e3)   =
    let 
      e1' = expression_to_enriched_lambda e1
      e2' = expression_to_enriched_lambda e2
      e3' = expression_to_enriched_lambda e3
    in
      EL.E_Case e1' [(EL.bool_tag, EL.true_tag,  [], e2'), 
                     (EL.bool_tag, EL.false_tag, [], e3')]
  expression_to_enriched_lambda (ML.E_Case e bs)      =
    let
      e'  = expression_to_enriched_lambda e
      bs' = map binding_to_clause bs
    in
      EL.E_Case e' bs'
  expression_to_enriched_lambda (ML.E_Seq e1 e2)      =
    let
      e1' = expression_to_enriched_lambda e1
      e2' = expression_to_enriched_lambda e2
    in
      EL.E_Seq e1' e2'
  expression_to_enriched_lambda (ML.E_Function 
             [(ML.P_Val v, e, ML.E_Const ML.C_True)]) =
    EL.E_Function v $ expression_to_enriched_lambda e
  expression_to_enriched_lambda (ML.E_Let bs e)       =
    let
      bs' = map binding_to_enriched_lambda bs
      e'  = expression_to_enriched_lambda e
    in
      EL.E_Let bs' e'
  expression_to_enriched_lambda (ML.E_LetRec lrbs e)  =
    let
      lrbs' = map letrec_binding_enriched_lambda lrbs
      e'    = expression_to_enriched_lambda e
    in
      EL.E_LetRec lrbs' e'
  expression_to_enriched_lambda ML.E_MatchFailure     =
    EL.E_MatchFailure
  expression_to_enriched_lambda (ML.E_FatBar e1 e2)   =
    let
      e1' = expression_to_enriched_lambda e1
      e2' = expression_to_enriched_lambda e2
    in
      EL.E_Rescue e1' e2'
  expression_to_enriched_lambda ML.Null               =
    EL.Null

  binding_to_definition :: ML.Binding -> EL.Binding
  binding_to_definition (ML.P_Val n, ML.E_Function 
                [(ML.P_Val v, e, ML.E_Const ML.C_True)]) =
    (n, [v], expression_to_enriched_lambda e)
  binding_to_definition (ML.P_Val n, e)                  =
    (n, [], expression_to_enriched_lambda e)

  letrec_binding_to_definition :: ML.LetRecBinding -> EL.Binding
  letrec_binding_to_definition (n, ML.E_Function 
                 [(ML.P_Val v, e, ML.E_Const ML.C_True)]) =
    (n, [v], expression_to_enriched_lambda e)
  letrec_binding_to_definition (n, e)                     =
    (n, [], expression_to_enriched_lambda e)


  definition_to_enriched_lambda :: ML.Definition -> EL.Definition
  definition_to_enriched_lambda (ML.D_Let bs)      =
    EL.D_Let $ map binding_to_definition bs
  definition_to_enriched_lambda (ML.D_LetRec lrbs) =
    EL.D_LetRec $ map letrec_binding_to_definition lrbs

  prog_pair_to_enriched_lambda :: ([ML.Definition], ML.Expr) -> EL.Program
  prog_pair_to_enriched_lambda (dfs, e) =
    (map definition_to_enriched_lambda dfs, expression_to_enriched_lambda e)

  definitions_to_front :: ML.Program -> ([ML.Definition], ML.Expr)
  definitions_to_front = definitions_to_front' [] [] where
    definitions_to_front' :: [ML.Definition] -> [ML.Expr] -> ML.Program -> ([ML.Definition], ML.Expr)
    definitions_to_front' dfs []     []               = 
      (reverse dfs, ML.Null)
    definitions_to_front' dfs (e:es) []               = 
      (reverse dfs, foldr (\e -> ML.E_Let [(ML.P_Val "it", e)]) e $ reverse es)
    definitions_to_front' dfs es     ((ML.IDF df):is) =
      definitions_to_front' (df:dfs) es is
    definitions_to_front' dfs es     ((ML.IEX ex):is) =
      definitions_to_front' dfs (ex:es) is

  program_to_enriched_lambda :: ML.Program -> EL.Program
  program_to_enriched_lambda prog = 
    prog_pair_to_enriched_lambda $ definitions_to_front prog
