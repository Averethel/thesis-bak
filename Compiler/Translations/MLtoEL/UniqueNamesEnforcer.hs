{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Translations.MLtoEL.UniqueNamesEnforcer (enforce_unique_names) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  type Counter = ValueName -> Integer

  empty_counter :: Counter
  empty_counter _ = 0

  empty_max :: Counter
  empty_max _ = 1

  inc_by :: Integer -> Counter -> ValueName -> Counter
  inc_by i c m = \x -> if x == m then c x + i else c x

  inc :: Counter -> ValueName -> Counter
  inc = inc_by 1

  set :: Counter -> ValueName -> Integer -> Counter
  set c m v = \x -> if x == m then v else c x

  data EnforcerState = 
    S {
        counter :: Counter,
        history :: [Counter],
        maxs    :: Counter
      }

  empty_state :: EnforcerState
  empty_state = S { counter = empty_counter, history = [], maxs = empty_max }

  backup :: (MonadState EnforcerState m) => m ()
  backup = do
    s <- get
    put $ s { history = counter s : history s }

  restore :: (MonadState EnforcerState m) => m ()
  restore = do
    s <- get
    let (c:cs) = history s
    put $ s { counter = c, history = cs }

  increment :: (MonadState EnforcerState m) => ValueName -> m ()
  increment v = do
    s <- get
    let c = counter s
    let m = maxs s
    put $ s { counter = set c v $ m v, maxs = m `inc` v }

  value :: (MonadState EnforcerState m) => ValueName -> m ValueName
  value v = do
    s <- get
    return $ ('_':v) ++ show (counter s $ v)

  rename_to_unique_pattern_list :: (MonadState EnforcerState m) => [Pattern] -> m [Pattern]
  rename_to_unique_pattern_list []     = return []
  rename_to_unique_pattern_list (p:ps) = do
    p'  <- rename_to_unique_pattern p
    ps' <- rename_to_unique_pattern_list ps
    return $ p':ps'

  rename_to_unique_pattern :: (MonadState EnforcerState m) => Pattern -> m Pattern
  rename_to_unique_pattern (P_Val vn) = do
    increment vn
    vn' <- value vn
    return $ P_Val vn'
  rename_to_unique_pattern (P_Tuple ps) = do
    ps' <- rename_to_unique_pattern_list ps
    return $ P_Tuple ps'
  rename_to_unique_pattern (P_Cons p1 p2) = do
    p1' <- rename_to_unique_pattern p1
    p2' <- rename_to_unique_pattern p2
    return $ P_Cons p1' p2'
  rename_to_unique_pattern p = return p

  rename_to_unique_function :: (MonadState EnforcerState m) => [Binding] -> m [Binding]
  rename_to_unique_function []          = return []
  rename_to_unique_function ((p, e):bs) = do
    backup
    p'  <- rename_to_unique_pattern p
    e'  <- rename_to_unique_expression e
    restore
    bs' <- rename_to_unique_function bs
    return $ (p', e'):bs'

  rename_to_unique_bindings :: (MonadState EnforcerState m) => [LetRecBinding] -> m [LetRecBinding]
  rename_to_unique_bindings []             = return []
  rename_to_unique_bindings ((v, bs):lrbs) = do
    increment v
    v'    <- value v
    bs'   <- rename_to_unique_function bs
    lrbs' <- rename_to_unique_bindings lrbs
    return $ (v',bs'):lrbs'

  rename_to_unique_expression_list :: (MonadState EnforcerState m) => [Expr] -> m [Expr]
  rename_to_unique_expression_list []     = return []
  rename_to_unique_expression_list (e:es) = do
    e'  <- rename_to_unique_expression e
    es' <- rename_to_unique_expression_list es
    return $ e':es' 

  rename_to_unique_expression :: (MonadState EnforcerState m) => Expr -> m Expr
  rename_to_unique_expression (E_Val v) = do
    v' <- value v
    return $ E_Val v'
  rename_to_unique_expression (E_Apply e1 e2) = do
    e1' <- rename_to_unique_expression e1
    e2' <- rename_to_unique_expression e2
    return $ E_Apply e1' e2'
  rename_to_unique_expression (E_Cons e1 e2) = do
    e1' <- rename_to_unique_expression e1
    e2' <- rename_to_unique_expression e2
    return $ E_Cons e1' e2'
  rename_to_unique_expression (E_Tuple es) = do
    es' <- rename_to_unique_expression_list es
    return $ E_Tuple es'
  rename_to_unique_expression (E_And e1 e2) = do
    e1' <- rename_to_unique_expression e1
    e2' <- rename_to_unique_expression e2
    return $ E_And e1' e2'
  rename_to_unique_expression (E_Or e1 e2) = do
    e1' <- rename_to_unique_expression e1
    e2' <- rename_to_unique_expression e2
    return $ E_And e1' e2'
  rename_to_unique_expression (E_ITE e1 e2 e3) = do
    e1' <- rename_to_unique_expression e1
    e2' <- rename_to_unique_expression e2
    e3' <- rename_to_unique_expression e3
    return $ E_ITE e1' e2' e3'
  rename_to_unique_expression (E_Seq e1 e2) = do
    e1' <- rename_to_unique_expression e1
    e2' <- rename_to_unique_expression e2
    return $ E_Seq e1' e2'
  rename_to_unique_expression (E_Function bs) = do
    bs' <- rename_to_unique_function bs
    return $ E_Function bs'
  rename_to_unique_expression (E_Let (p, e1) e2) = do
    e1' <- rename_to_unique_expression e1
    backup
    p'  <- rename_to_unique_pattern p
    e2' <- rename_to_unique_expression e2
    restore
    return $ E_Let (p', e1') e2'
  rename_to_unique_expression (E_LetRec lrbs e) = do
    backup
    lrbs' <- rename_to_unique_bindings lrbs
    e'    <- rename_to_unique_expression e
    restore
    return $ E_LetRec lrbs' e'
  rename_to_unique_expression e = return e

  rename_to_unique_definition :: (MonadState EnforcerState m) => Definition -> m Definition
  rename_to_unique_definition (D_Let (p, e))  = do
    p' <- rename_to_unique_pattern p
    e' <- rename_to_unique_expression e
    return $ D_Let (p', e')
  rename_to_unique_definition (D_LetRec lrbs) = do
    lrbs' <- rename_to_unique_bindings lrbs
    return $ D_LetRec lrbs'

  rename_to_unique_instruction :: (MonadState EnforcerState m) => Instruction -> m Instruction
  rename_to_unique_instruction (IDF df) = do
    df' <- rename_to_unique_definition df
    return $ IDF df'
  rename_to_unique_instruction (IEX ex) = do
    ex' <- rename_to_unique_expression ex
    return $ IEX ex'

  rename_to_unique :: (MonadState EnforcerState m) => Program -> m Program
  rename_to_unique []     = return []
  rename_to_unique (i:is) = do
    i'  <- rename_to_unique_instruction i
    is' <- rename_to_unique is
    return $ i':is'

  enforce_unique_names :: Program -> Program
  enforce_unique_names prog = fst $ runState (rename_to_unique prog) Compiler.Translations.MLtoEL.UniqueNamesEnforcer.empty_state
