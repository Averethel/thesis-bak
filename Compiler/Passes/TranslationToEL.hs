{-# LANGUAGE
  FlexibleContexts
  #-}
module Compiler.Passes.TranslationToEL 
(program_to_enriched_lambda) 
where
  import qualified Languages.MiniML.Syntax as ML
  import qualified Languages.EnrichedLambda.Syntax as EL

  import Control.Monad.State

  -- for testing
  import Languages.MiniML.PrettyPrint

  -- Assumptions:
  -- 1. Tuples are folded to pairs
  -- 2. Wildcards are substituted with variables
  -- 3. Expressions are typechecked
  -- 4. Pattern matchings are simplified

  type NamerState = Integer

  empty_state :: NamerState
  empty_state = 0

  new_arg_var :: MonadState NamerState m => m String
  new_arg_var = do
    s <- get
    put $ s + 1
    return $ "__arg__" ++ show s

  unary_prim_to_enriched_lambda :: MonadState NamerState m => ML.UnaryPrim -> m EL.Expr
  unary_prim_to_enriched_lambda ML.U_Not     =
    return $ EL.E_UPrim EL.U_Not
  unary_prim_to_enriched_lambda ML.U_Ref     =
    return $ EL.E_UPrim EL.U_Ref
  unary_prim_to_enriched_lambda ML.U_Deref   =
    return $ EL.E_UPrim EL.U_Deref
  unary_prim_to_enriched_lambda ML.U_I_Minus =
    return $ EL.E_Apply (EL.E_BPrim EL.B_Minus) $ EL.E_Num 0
  unary_prim_to_enriched_lambda ML.U_Fst     =
    return $ EL.E_UPrim EL.U_Fst
  unary_prim_to_enriched_lambda ML.U_Snd     =
    return $ EL.E_UPrim EL.U_Snd
  unary_prim_to_enriched_lambda ML.U_Empty   =
    return $ EL.E_UPrim EL.U_Empty
  unary_prim_to_enriched_lambda ML.U_Head    =
    return $ EL.E_UPrim EL.U_Head
  unary_prim_to_enriched_lambda ML.U_Tail    =
    return $ EL.E_UPrim EL.U_Tail

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

  constant_to_enriched_lambda :: MonadState NamerState m => ML.Constant -> m EL.Expr
  constant_to_enriched_lambda (ML.C_Int n) =
    return $ EL.E_Num n
  constant_to_enriched_lambda (ML.C_False) =
    return $ EL.E_Constr EL.bool_tag EL.false_tag 0
  constant_to_enriched_lambda (ML.C_True)  =
    return $ EL.E_Constr EL.bool_tag EL.true_tag 0
  constant_to_enriched_lambda (ML.C_Nil)   =
    return $ EL.E_Constr EL.list_tag EL.nil_tag 0
  constant_to_enriched_lambda (ML.C_Unit)  =
    return $ EL.E_Constr EL.unit_tag EL.unit_tag_c 0

  expression_to_enriched_lambda :: MonadState NamerState m => ML.Expr -> m EL.Expr
  expression_to_enriched_lambda (ML.E_UPrim up)       =
    unary_prim_to_enriched_lambda up
  expression_to_enriched_lambda (ML.E_BPrim bp)       =
    binary_prim_to_enriched_lambda bp
  expression_to_enriched_lambda (ML.E_Val vn)         =
    return $ EL.E_Val vn
  expression_to_enriched_lambda (ML.E_Const c)        =
    constant_to_enriched_lambda c
  expression_to_enriched_lambda (ML.E_Apply e1 e2)    = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Apply e1' e2'
  expression_to_enriched_lambda (ML.E_Cons e1 e2)     = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Apply (EL.E_Apply (EL.E_Constr EL.list_tag EL.cons_tag 2) e1') e2'
  expression_to_enriched_lambda (ML.E_Tuple [e1, e2]) = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Apply (EL.E_Apply (EL.E_Constr EL.pair_tag EL.pair_tag_c 2) e1') e2'
  expression_to_enriched_lambda (ML.E_And e1 e2)      = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Apply (EL.E_Apply (EL.E_BPrim EL.B_And) e1') e2'
  expression_to_enriched_lambda (ML.E_Or e1 e2)       = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Apply (EL.E_Apply (EL.E_BPrim EL.B_Or) e1') e2'
  expression_to_enriched_lambda (ML.E_ITE e1 e2 e3)   = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    e3' <- expression_to_enriched_lambda e3
    return $ EL.E_Case e1' [(EL.bool_tag, EL.true_tag, [], e2'), 
                            (EL.bool_tag, EL.false_tag, [], e3')]
  expression_to_enriched_lambda (ML.E_Case e bs)      = do
    e'  <- expression_to_enriched_lambda e
    bs' <- mapM binding_to_clause bs
    return $ EL.E_Case e' bs'
  expression_to_enriched_lambda (ML.E_Seq e1 e2)      = do
    e1' <- expression_to_enriched_lambda e1
    e2' <- expression_to_enriched_lambda e2
    return $ EL.E_Seq e1' e2'
  expression_to_enriched_lambda (ML.E_Function bs)    =
    bindings_to_function bs
  expression_to_enriched_lambda (ML.E_Let bs e)       = do
    bs' <- mapM binding_to_definition bs
    e'  <- expression_to_enriched_lambda e
    return $ EL.E_Let (map (\(a, [], c) -> (a, c)) bs') e'
  expression_to_enriched_lambda (ML.E_LetRec lrbs e)  = do
    lrbs' <- mapM letrec_binding_to_definition lrbs
    e'    <- expression_to_enriched_lambda e
    return $ EL.E_LetRec (map (\(a, [], c) -> (a, c)) lrbs') e'
  expression_to_enriched_lambda ML.E_MatchFailure     =
    return EL.E_MatchFailure
  expression_to_enriched_lambda ML.Null               =
    return EL.Null

  binding_to_clause :: MonadState NamerState m => ML.Binding -> m EL.Clause
  binding_to_clause (ML.P_Const ML.C_True, e)                 = do
    e' <- expression_to_enriched_lambda e
    return (EL.bool_tag, EL.true_tag, [], e')
  binding_to_clause (ML.P_Const ML.C_False, e)                = do
    e' <- expression_to_enriched_lambda e
    return (EL.bool_tag, EL.false_tag, [], e')
  binding_to_clause (ML.P_Const ML.C_Unit, e)                 = do
    e' <- expression_to_enriched_lambda e
    return (EL.unit_tag, EL.unit_tag_c, [], e')
  binding_to_clause (ML.P_Const ML.C_Nil, e)                  = do
    e' <- expression_to_enriched_lambda e
    return (EL.list_tag, EL.nil_tag, [], e')
  binding_to_clause (ML.P_Cons (ML.P_Val x) (ML.P_Val xs), e) = do
    e' <- expression_to_enriched_lambda e
    return (EL.list_tag, EL.cons_tag, [x, xs], e')
  binding_to_clause (ML.P_Tuple [ML.P_Val a, ML.P_Val b], e)  = do
    e' <- expression_to_enriched_lambda e
    return (EL.pair_tag, EL.pair_tag_c, [a, b], e')

  bindings_to_function :: MonadState NamerState m => [ML.Binding] -> m EL.Expr
  bindings_to_function [(ML.P_Val n, e)] = do
    e' <- expression_to_enriched_lambda e
    return $ EL.E_Function n e'
  bindings_to_function bs             = do
    n'  <- new_arg_var
    bs' <- mapM binding_to_clause bs
    return $ EL.E_Function n' $ EL.E_Case (EL.E_Val n') bs'

  binding_to_definition :: MonadState NamerState m => ML.Binding -> m EL.Binding
  binding_to_definition (ML.P_Val n, e) = do
    e' <- expression_to_enriched_lambda e
    return (n, [], e')

  letrec_binding_to_definition :: MonadState NamerState m => ML.LetRecBinding -> m EL.Binding
  letrec_binding_to_definition (n, bs)  = do
    fun <- bindings_to_function bs
    return (n, [], fun)

  definition_to_enriched_lambda :: MonadState NamerState m => ML.Definition -> m EL.Definition
  definition_to_enriched_lambda (ML.D_Let bs)      = do
    bs' <- mapM binding_to_definition bs
    return $ EL.D_Let bs'
  definition_to_enriched_lambda (ML.D_LetRec lrbs) = do
    lrbs' <- mapM letrec_binding_to_definition lrbs
    return $ EL.D_LetRec lrbs'

  prog_pair_to_enriched_lambda :: MonadState NamerState m => ([ML.Definition], ML.Expr) -> m EL.Program
  prog_pair_to_enriched_lambda (dfs, e) = do
    dfs' <- mapM definition_to_enriched_lambda dfs
    e'   <- expression_to_enriched_lambda e
    return (dfs', e')

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
    fst $ runState 
          (prog_pair_to_enriched_lambda $ definitions_to_front prog)
          empty_state