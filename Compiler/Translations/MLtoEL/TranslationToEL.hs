{-# LANGUAGE
  FlexibleContexts
  #-}
module Compiler.Translations.MLtoEL.TranslationToEL (program_to_enriched_lambda) where
  import qualified Languages.MiniML.Syntax as ML
  import qualified Languages.EnrichedLambda.Syntax as EL
  
  import Control.Monad.State

  data NamerState = 
    S {
        let_var :: Integer, 
        arg_var :: Integer
      }

  empty_state :: NamerState
  empty_state = S { let_var = 0, arg_var = 0 }

  new_let_var :: MonadState NamerState m => m String
  new_let_var = do
    s <- get
    put $ s { let_var = let_var s + 1 }
    return $ "_let" ++ (show $ let_var s)

  new_arg_var :: MonadState NamerState m => m String
  new_arg_var = do
    s <- get
    put $ s { arg_var = arg_var s + 1 }
    return $ "_arg" ++ (show $ arg_var s)

  constant_to_enriched_lambda :: MonadState NamerState m => ML.Constant -> m EL.Constant
  constant_to_enriched_lambda (ML.C_Int n) = return $ EL.C_Int n
  constant_to_enriched_lambda ML.C_True    = return EL.C_True
  constant_to_enriched_lambda ML.C_False   = return EL.C_False
  constant_to_enriched_lambda ML.C_Nil     = return EL.C_Nil
  constant_to_enriched_lambda ML.C_Unit    = return EL.C_Unit 

  unary_prim_to_enriched_lambda :: MonadState NamerState m => ML.UnaryPrim -> m EL.Expr
  unary_prim_to_enriched_lambda ML.U_Not     = 
    return $ EL.E_UPrim EL.U_Not
  unary_prim_to_enriched_lambda ML.U_Ref     =
    return $ EL.E_UPrim EL.U_Ref
  unary_prim_to_enriched_lambda ML.U_Deref   =
    return $ EL.E_UPrim EL.U_Deref
  unary_prim_to_enriched_lambda ML.U_I_Minus =
    return $ EL.E_Apply (EL.E_BPrim EL.B_Minus) (EL.E_Const $ EL.C_Int 0)

  binary_prim_to_enriched_lambda :: MonadState NamerState m => ML.BinaryPrim -> m EL.Expr
  binary_prim_to_enriched_lambda ML.B_Eq      =
    return $ EL.E_BPrim EL.B_Eq
  binary_prim_to_enriched_lambda ML.B_I_Plus  =
    return $ EL.E_BPrim EL.B_Plus
  binary_prim_to_enriched_lambda ML.B_I_Minus =
    return $ EL.E_BPrim EL.B_Minus
  binary_prim_to_enriched_lambda ML.B_I_Mult  =
    return $ EL.E_BPrim EL.B_Mult
  binary_prim_to_enriched_lambda ML.B_I_Div   =
    return $ EL.E_BPrim EL.B_Div
  binary_prim_to_enriched_lambda ML.B_Assign  =
    return $ EL.E_BPrim EL.B_Assign

  pattern_to_bindings :: MonadState NamerState m => ML.Pattern -> EL.Expr -> m [(String, EL.Expr)]
  pattern_to_bindings (ML.P_Val v)        e =
    return [(v, e)]
  pattern_to_bindings ML.P_Wildcard       _ =
    return []
  pattern_to_bindings (ML.P_Const _)      _ =
    return []
  pattern_to_bindings (ML.P_Tuple [a, b]) e = do
    b1 <- pattern_to_bindings a (EL.E_Apply (EL.E_UPrim EL.U_Fst) e)
    b2 <- pattern_to_bindings b (EL.E_Apply (EL.E_UPrim EL.U_Snd) e)
    return $ b1 ++ b2
  pattern_to_bindings (ML.P_Tuple (p:ps)) e = do
    b1 <- pattern_to_bindings p (EL.E_Apply (EL.E_UPrim EL.U_Fst) e)
    b2 <- pattern_to_bindings (ML.P_Tuple ps) (EL.E_Apply (EL.E_UPrim EL.U_Snd) e)
    return $ b1 ++ b2
  pattern_to_bindings (ML.P_Cons p1 p2)   e = do
    b1 <- pattern_to_bindings p1 (EL.E_Apply (EL.E_UPrim EL.U_Head) e)
    b2 <- pattern_to_bindings p2 (EL.E_Apply (EL.E_UPrim EL.U_Tail) e)
    return $ b1 ++ b2
  
  pattern_to_definitions :: MonadState NamerState m => ML.Pattern -> EL.Expr -> m [EL.Definition]
  pattern_to_definitions p e = do
    bs <- pattern_to_bindings p e
    return $ map (uncurry EL.D_Let) bs

  pattern_to_variables :: MonadState NamerState m => ML.Pattern -> EL.Expr -> m (EL.Expr -> EL.Expr)
  pattern_to_variables p e = do
    bs <- pattern_to_bindings p e
    return $ flip (foldr (uncurry EL.E_Let)) bs

  pattern_to_test :: MonadState NamerState m => ML.Pattern -> EL.Expr -> m EL.Expr
  pattern_to_test (ML.P_Val _)        _  = 
    return $ EL.E_Const EL.C_True
  pattern_to_test ML.P_Wildcard       _  = 
    return $ EL.E_Const EL.C_True
  pattern_to_test (ML.P_Const c)      e  = do
    c' <- constant_to_enriched_lambda c
    return $ EL.E_Apply (EL.E_Apply (EL.E_BPrim EL.B_Eq) (EL.E_Const c')) e
  pattern_to_test (ML.P_Tuple [a,b])  e  = do
    e1' <- pattern_to_test a $ EL.E_Apply (EL.E_UPrim EL.U_Fst) e
    e2' <- pattern_to_test b $ EL.E_Apply (EL.E_UPrim EL.U_Snd) e
    return $ EL.E_ITE e1' e2' $ EL.E_Const EL.C_False
  pattern_to_test (ML.P_Tuple (p:ps)) e = do
    e1' <- pattern_to_test p $ EL.E_Apply (EL.E_UPrim EL.U_Fst) e
    e2' <- pattern_to_test (ML.P_Tuple ps) $ EL.E_Apply (EL.E_UPrim EL.U_Snd) e
    return $ EL.E_ITE e1' e2' $ EL.E_Const EL.C_False
  pattern_to_test (ML.P_Cons p1 p2)   e = do
    let t1 = EL.E_Apply (EL.E_UPrim EL.U_Not) $ EL.E_Apply (EL.E_UPrim EL.U_Empty) e
    e1' <- pattern_to_test p1 $ EL.E_Apply (EL.E_UPrim EL.U_Head) e
    e2' <- pattern_to_test p2 $ EL.E_Apply (EL.E_UPrim EL.U_Tail) e
    return $ EL.E_ITE t1 (EL.E_ITE e1' e2' $ EL.E_Const EL.C_False) $ EL.E_Const EL.C_False

  bindings_to_ifs :: MonadState NamerState m => [ML.Binding] -> EL.Expr -> m EL.Expr
  bindings_to_ifs []           _ = return $ EL.E_MatchFailure
  bindings_to_ifs ((p, e1):ps) e = do
    bs  <- pattern_to_variables p e
    t   <- pattern_to_test p e
    e1' <- expression_to_enriched_lambda e1
    e2' <- bindings_to_ifs ps e
    return $ EL.E_ITE (bs t) (bs e1') e2'

  function_to_enriched_lambda :: MonadState NamerState m => [ML.Binding] -> m EL.Expr
  function_to_enriched_lambda bs = do
    v <- new_arg_var
    t <- bindings_to_ifs bs (EL.E_Val v) 
    return $ EL.E_Function v t

  bindings_to_enriched_lambda :: MonadState NamerState m => [ML.LetRecBinding] -> m [(String, EL.Expr)]
  bindings_to_enriched_lambda = mapM (\(v, bs) -> do { bs' <- function_to_enriched_lambda bs; return (v, bs') })

  tuple_to_pairs :: MonadState NamerState m => [ML.Expr] -> m EL.Expr
  tuple_to_pairs [e]    = 
    expression_to_enriched_lambda e
  tuple_to_pairs (e:es) = do
    e1' <- expression_to_enriched_lambda e
    e2' <- tuple_to_pairs es
    return $ EL.E_Pair e1' e2'

  expression_to_enriched_lambda :: MonadState NamerState m => ML.Expr -> m EL.Expr
  expression_to_enriched_lambda (ML.E_UPrim up)       =
    unary_prim_to_enriched_lambda up
  expression_to_enriched_lambda (ML.E_BPrim bp)       =
    binary_prim_to_enriched_lambda bp
  expression_to_enriched_lambda (ML.E_Val v)          = 
    return $ EL.E_Val v
  expression_to_enriched_lambda (ML.E_Const c)        = do
    c' <- constant_to_enriched_lambda c
    return $ EL.E_Const c'
  expression_to_enriched_lambda (ML.E_Apply e1 e2)    = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Apply e1' e2'
  expression_to_enriched_lambda (ML.E_Cons e1 e2)     = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Cons e1' e2'
  expression_to_enriched_lambda (ML.E_Tuple es)       =
    tuple_to_pairs es
  expression_to_enriched_lambda (ML.E_And e1 e2)      = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_ITE e1' e2' (EL.E_Const EL.C_False)
  expression_to_enriched_lambda (ML.E_Or e1 e2)       = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_ITE e1' (EL.E_Const EL.C_True) e2'
  expression_to_enriched_lambda (ML.E_ITE e1 e2 e3)   = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    e3' <- expression_to_enriched_lambda e3
    return $ EL.E_ITE e1' e2' e3'
  expression_to_enriched_lambda (ML.E_Seq e1 e2)      = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Seq e1' e2'
  expression_to_enriched_lambda (ML.E_Function bs)    =
    function_to_enriched_lambda bs
  expression_to_enriched_lambda (ML.E_Let (p, e1) e2) = do
    v   <- new_let_var
    dfs <- pattern_to_variables p $ EL.E_Val v
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Let v e1' $ dfs e2'
  expression_to_enriched_lambda (ML.E_LetRec lrbs e)  = do
    lrbs' <- bindings_to_enriched_lambda lrbs
    e'    <- expression_to_enriched_lambda e
    return $ EL.E_Letrec lrbs' e'

  definition_to_enriched_lambda :: MonadState NamerState m => ML.Definition -> m [EL.Definition]
  definition_to_enriched_lambda (ML.D_Let (p, e))  = do
    v   <- new_let_var
    e'  <- expression_to_enriched_lambda e
    dfs <- pattern_to_definitions p $ EL.E_Val v
    return $ (EL.D_Let v e') : dfs
  definition_to_enriched_lambda (ML.D_LetRec lrbs) = do
    lrbs' <- bindings_to_enriched_lambda lrbs
    return $ [EL.D_Letrec lrbs']

  instruction_to_enriched_lambda :: MonadState NamerState m => ML.Instruction -> m EL.Program
  instruction_to_enriched_lambda (ML.IDF df) = do
    df' <- definition_to_enriched_lambda df
    return $ map EL.IDF df'
  instruction_to_enriched_lambda (ML.IEX ex) = do
    ex' <- expression_to_enriched_lambda ex
    return $ [EL.IEX ex']

  program_to_enriched_lambda :: ML.Program -> EL.Program
  program_to_enriched_lambda prog = concat $ fst $ runState (program_to_enriched_lambda' prog) empty_state where
    program_to_enriched_lambda' :: MonadState NamerState m => ML.Program -> m [EL.Program]
    program_to_enriched_lambda' = mapM instruction_to_enriched_lambda
