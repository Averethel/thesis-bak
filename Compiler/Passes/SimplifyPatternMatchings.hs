{-# LANGUAGE 
  FlexibleContexts
  #-}

module Compiler.Passes.SimplifyPatternMatchings (simplify_pattern_matchings) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  -- Assumptions:
  -- 1. Wildcards were translated to variables
  -- 2. Tuples were folded to pairs
  -- 3. Expression was typechecked

  -- I thought that it is possible to easilly fold pattern matchings which 
  -- contains nested constructors and integer constants to simple pattern 
  -- matches without them, butthis turned out to be much more difficult than it
  -- seemed so I decided to leave matches that are already in simple form and 
  -- transform others to the if then else tests as in previous version of 
  -- enriched lambda calculus

  data SimplifierState = 
    S {
        let_var :: Integer, 
        arg_var :: Integer
      }

  empty_state :: SimplifierState
  empty_state = S { let_var = 0, arg_var = 0 }

  new_let_var :: MonadState SimplifierState m => m String
  new_let_var = do
    s <- get
    put $ s { let_var = let_var s + 1 }
    return $ "_let" ++ (show $ let_var s)

  new_arg_var :: MonadState SimplifierState m => m String
  new_arg_var = do
    s <- get
    put $ s { arg_var = arg_var s + 1 }
    return $ "_arg" ++ (show $ arg_var s)

  is_variable :: Pattern -> Bool
  is_variable (P_Val _) = True
  is_variable _         = False

  is_binding_variable :: Binding -> Bool
  is_binding_variable (p, _) = is_variable p

  is_let_binding_variables :: [Binding] -> Bool
  is_let_binding_variables = all is_binding_variable 

  is_pattern_simple :: Pattern -> Bool
  is_pattern_simple (P_Const C_False)            = True
  is_pattern_simple (P_Const C_True)             = True
  is_pattern_simple (P_Const C_Nil)              = True
  is_pattern_simple (P_Const C_Unit)             = True
  is_pattern_simple (P_Tuple [P_Val _, P_Val _]) = True
  is_pattern_simple (P_Cons (P_Val _) (P_Val _)) = True
  is_pattern_simple _                            = False

  is_binding_simple :: Binding -> Bool
  is_binding_simple (p, _) = is_pattern_simple p

  is_matching_simple :: [Binding] -> Bool
  is_matching_simple = all is_binding_simple

  pattern_to_bindings :: MonadState SimplifierState m => Pattern -> Expr -> m [(String, Expr)]
  pattern_to_bindings (P_Val v)        e =
    return [(v, e)]
  pattern_to_bindings (P_Const _)      _ =
    return []
  pattern_to_bindings (P_Tuple [a, b]) e = do
    b1 <- pattern_to_bindings a (E_Apply (E_UPrim U_Fst) e)
    b2 <- pattern_to_bindings b (E_Apply (E_UPrim U_Snd) e)
    return $ b1 ++ b2
  pattern_to_bindings (P_Cons p1 p2)   e = do
    b1 <- pattern_to_bindings p1 (E_Apply (E_UPrim U_Head) e)
    b2 <- pattern_to_bindings p2 (E_Apply (E_UPrim U_Tail) e)
    return $ b1 ++ b2

  pattern_to_variables :: MonadState SimplifierState m => Pattern -> Expr -> m (Expr -> Expr)
  pattern_to_variables p e = do
    bs <- pattern_to_bindings p e
    return $ E_Let $ map (\(s, e) -> (P_Val s, e)) bs

  pattern_to_test :: MonadState SimplifierState m => Pattern -> Expr -> m Expr
  pattern_to_test (P_Val _)        _  = 
    return $ E_Const C_True
  pattern_to_test (P_Const c)      e  =
    return $ E_Apply (E_Apply (E_BPrim B_Eq) (E_Const c)) e
  pattern_to_test (P_Tuple [a,b])  e  = do
    e1' <- pattern_to_test a $ E_Apply (E_UPrim U_Fst) e
    e2' <- pattern_to_test b $ E_Apply (E_UPrim U_Snd) e
    return $ E_ITE e1' e2' $ E_Const C_False
  pattern_to_test (P_Cons p1 p2)   e = do
    let t1 = E_Apply (E_UPrim U_Not) $ E_Apply (E_UPrim U_Empty) e
    e1' <- pattern_to_test p1 $ E_Apply (E_UPrim U_Head) e
    e2' <- pattern_to_test p2 $ E_Apply (E_UPrim U_Tail) e
    return $ E_ITE t1 (E_ITE e1' e2' $ E_Const C_False) $ E_Const C_False

  bindings_to_ifs :: MonadState SimplifierState m => [Binding] -> Expr -> m Expr
  bindings_to_ifs []           _ = return $ E_MatchFailure
  bindings_to_ifs ((p, e1):ps) e = do
    bs  <- pattern_to_variables p e
    t   <- pattern_to_test p e
    e1' <- simplify_pattern_matchings_expression e1
    e2' <- bindings_to_ifs ps e
    return $ E_ITE (bs t) (bs e1') e2'

  binding_to_lets :: MonadState SimplifierState m => Binding -> m [Binding]
  binding_to_lets (p, e)
    | is_variable p = return [(p, e)]
    | otherwise     = do
      v   <- new_let_var
      bs  <- pattern_to_bindings p $ E_Val v
      e'  <- simplify_pattern_matchings_expression e
      return $ (P_Val v, e') : map (\(s, e) -> (P_Val s, e)) bs

  simplify_in_binding :: MonadState SimplifierState m => Binding -> m Binding
  simplify_in_binding (p, e) = do 
    e' <- simplify_pattern_matchings_expression e;
    return (p, e')

  simplify_in_function :: MonadState SimplifierState m => [Binding] -> m [Binding]
  simplify_in_function bs
    | not (is_matching_simple bs) = do
      v   <- new_arg_var
      ifs <- bindings_to_ifs bs $ E_Val v
      return [(P_Val v, ifs)]
    | otherwise                   = do
      bs' <- mapM simplify_in_binding bs
      return bs'

  simplify_in_letrec_binding :: MonadState SimplifierState m => LetRecBinding -> m LetRecBinding
  simplify_in_letrec_binding (v, bs) = do
    bs' <- simplify_in_function bs
    return (v, bs')

  simplify_pattern_matchings_expression :: MonadState SimplifierState m => Expr -> m Expr
  simplify_pattern_matchings_expression (E_Apply e1 e2)   = do
    e1' <- simplify_pattern_matchings_expression e1
    e2' <- simplify_pattern_matchings_expression e2
    return $ E_Apply e1' e2'
  simplify_pattern_matchings_expression (E_Cons e1 e2)    = do
    e1' <- simplify_pattern_matchings_expression e1
    e2' <- simplify_pattern_matchings_expression e2
    return $ E_Cons e1' e2'
  simplify_pattern_matchings_expression (E_Tuple es)      = do
    es' <- mapM simplify_pattern_matchings_expression es
    return $ E_Tuple es'
  simplify_pattern_matchings_expression (E_And e1 e2)     = do
    e1' <- simplify_pattern_matchings_expression e1
    e2' <- simplify_pattern_matchings_expression e2
    return $ E_And e1' e2'
  simplify_pattern_matchings_expression (E_Or e1 e2)      = do
    e1' <- simplify_pattern_matchings_expression e1
    e2' <- simplify_pattern_matchings_expression e2
    return $ E_Or e1' e2'
  simplify_pattern_matchings_expression (E_ITE e1 e2 e3)  = do
    e1' <- simplify_pattern_matchings_expression e1
    e2' <- simplify_pattern_matchings_expression e2
    e3' <- simplify_pattern_matchings_expression e3
    return $ E_ITE e1' e2' e3'
  simplify_pattern_matchings_expression (E_Case e bs)
    | not (is_matching_simple bs)                         = do
      e' <- simplify_pattern_matchings_expression e
      bindings_to_ifs bs e'
    | otherwise                                           = do
      e'  <- simplify_pattern_matchings_expression e
      bs' <- mapM simplify_in_binding bs
      return $ E_Case e' bs'
  simplify_pattern_matchings_expression (E_Seq e1 e2)     = do
    e1' <- simplify_pattern_matchings_expression e1
    e2' <- simplify_pattern_matchings_expression e2
    return $ E_Seq e1' e2'
  simplify_pattern_matchings_expression (E_Function bs)   = do
    bs' <- simplify_in_function bs
    return $ E_Function bs'
  simplify_pattern_matchings_expression (E_Let bs e)
    | not (is_let_binding_variables bs)                   = do
      e'  <- simplify_pattern_matchings_expression e
      bs' <- mapM binding_to_lets bs
      return $ E_Let (concat bs') e'
    | otherwise                                           = do
      e'  <- simplify_pattern_matchings_expression e
      bs' <- mapM simplify_in_binding bs
      return $ E_Let bs' e'
  simplify_pattern_matchings_expression (E_LetRec lrbs e) = do
    e'    <- simplify_pattern_matchings_expression e
    lrbs' <- mapM simplify_in_letrec_binding lrbs
    return $ E_LetRec lrbs' e'
  simplify_pattern_matchings_expression e                 = return e

  simplify_pattern_matchings_definition :: MonadState SimplifierState m => Definition -> m Definition
  simplify_pattern_matchings_definition (D_Let bs)
    | not (is_let_binding_variables bs)                 = do
      bs' <- mapM binding_to_lets bs
      return $ D_Let $ concat bs'
    | otherwise                                         = do
      bs' <- mapM simplify_in_binding bs
      return $ D_Let bs'
  simplify_pattern_matchings_definition (D_LetRec lrbs) = do
    lrbs' <- mapM simplify_in_letrec_binding lrbs
    return $ D_LetRec lrbs'

  simplify_pattern_matchings_instruction :: MonadState SimplifierState m => Instruction -> m Instruction
  simplify_pattern_matchings_instruction (IDF df) = do
    df' <- simplify_pattern_matchings_definition df
    return $ IDF df'
  simplify_pattern_matchings_instruction (IEX ex) = do
    ex' <- simplify_pattern_matchings_expression ex
    return $ IEX ex'

  simplify_pattern_matchings :: Program -> Program
  simplify_pattern_matchings is = fst $ runState 
    (mapM simplify_pattern_matchings_instruction is)
    empty_state
