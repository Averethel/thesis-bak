{-# LANGUAGE
  FlexibleContexts,
  NoMonomorphismRestriction
  #-}
module Compiler.Translations.ELtoELLT where
  import qualified Languages.EnrichedLambda.Syntax as EL
  import Languages.EnrichedLambda.PrettyPrint
  import qualified Languages.EnrichedLambdaLowLevelTypes.Syntax as LT
  
  contant_to_low_level_lambda :: EL.Constant -> LT.Struct
  contant_to_low_level_lambda (EL.C_Int n) = LT.S_Int n
  contant_to_low_level_lambda EL.C_True    = LT.S_Str LT.Tg_True 0 []
  contant_to_low_level_lambda EL.C_False   = LT.S_Str LT.Tg_False 0 []
  contant_to_low_level_lambda EL.C_Nil     = LT.S_Str LT.Tg_Nil 0 []
  contant_to_low_level_lambda EL.C_Unit    = LT.S_Str LT.Tg_Unit 0 []
  
  unary_primitive_to_low_level_lambda :: EL.UnaryPrim -> LT.Prim
  unary_primitive_to_low_level_lambda EL.U_Not   = LT.P_Not
  unary_primitive_to_low_level_lambda EL.U_Ref   = LT.P_Ref
  unary_primitive_to_low_level_lambda EL.U_Deref = LT.P_Deref
  unary_primitive_to_low_level_lambda EL.U_Fst   = LT.P_Fst
  unary_primitive_to_low_level_lambda EL.U_Snd   = LT.P_Snd
  unary_primitive_to_low_level_lambda EL.U_Head  = LT.P_Head
  unary_primitive_to_low_level_lambda EL.U_Tail  = LT.P_Tail
  unary_primitive_to_low_level_lambda EL.U_Empty = LT.P_Empty
  
  binary_primitive_to_low_level_lambda :: EL.BinaryPrim -> LT.Prim
  binary_primitive_to_low_level_lambda EL.B_Eq     = LT.P_Eq
  binary_primitive_to_low_level_lambda EL.B_Plus   = LT.P_Plus
  binary_primitive_to_low_level_lambda EL.B_Minus  = LT.P_Minus
  binary_primitive_to_low_level_lambda EL.B_Mult   = LT.P_Mult
  binary_primitive_to_low_level_lambda EL.B_Div    = LT.P_Div
  binary_primitive_to_low_level_lambda EL.B_Assign = LT.P_Assign
  
  expression_to_low_level_lambda :: EL.Expr -> LT.Expr
  expression_to_low_level_lambda (EL.E_Var s)          = LT.E_Val s
  expression_to_low_level_lambda (EL.E_UPrim p)        = LT.E_Prim $ unary_primitive_to_low_level_lambda p
  expression_to_low_level_lambda (EL.E_BPrim p)        = LT.E_Prim $ binary_primitive_to_low_level_lambda p
  expression_to_low_level_lambda (EL.E_Const c)        = LT.E_Struct $ contant_to_low_level_lambda c
  expression_to_low_level_lambda (EL.E_Cons e1 e2)     = LT.E_Apply (LT.E_Apply (LT.E_Prim LT.P_AllocList) $ expression_to_low_level_lambda e1) $ expression_to_low_level_lambda e2
  expression_to_low_level_lambda (EL.E_ITE e1 e2 e3)   = LT.E_ITE (expression_to_low_level_lambda e1) (expression_to_low_level_lambda e2) $ expression_to_low_level_lambda e3
  expression_to_low_level_lambda (EL.E_Seq e1 e2)      = LT.E_Seq (expression_to_low_level_lambda e1) $ expression_to_low_level_lambda e2
  expression_to_low_level_lambda (EL.E_Pair e1 e2)     = LT.E_Apply (LT.E_Apply (LT.E_Prim LT.P_AllocPair) $ expression_to_low_level_lambda e1) $ expression_to_low_level_lambda e2
  expression_to_low_level_lambda (EL.E_Let s e1 e2)    = LT.E_Let s (expression_to_low_level_lambda e1) $ expression_to_low_level_lambda e2
  expression_to_low_level_lambda (EL.E_Letrec s e1 e2) = LT.E_Letrec s (expression_to_low_level_lambda e1) $ expression_to_low_level_lambda e2
  expression_to_low_level_lambda (EL.E_Apply e1 e2)    = LT.E_Apply (expression_to_low_level_lambda e1) $ expression_to_low_level_lambda e2
  expression_to_low_level_lambda (EL.E_Function s e)   = LT.E_Function s $ expression_to_low_level_lambda e
  expression_to_low_level_lambda (EL.E_MatchFailure)   = LT.E_MatchFailure
  