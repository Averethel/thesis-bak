{-# LANGUAGE 
  FlexibleContexts,
  FlexibleInstances,
  TypeSynonymInstances
  #-}

module Languages.EnrichedLambda.State where
  import Languages.EnrichedLambda.Errors
  import Languages.EnrichedLambda.Syntax
  
  import Control.Monad.Error
  import Control.Monad.State
  import Data.Array
  import Data.List
  
  type TypingEnv = String -> Maybe Type
  instance Show TypingEnv where
    show _ = "TypingEnv"
  
  type Subst = Type -> Type
  instance Show Subst where
    show _ = "Subst"
  
  type EvalEnv = String -> Maybe Expr
  instance Show EvalEnv where
    show _ = "EvalEnv"
  
  data Memory = M { 
                    mem      :: Array Integer Expr,
                    freeCell :: [Integer],
                    used     :: Integer
                  }

  get_free_addr :: MonadError String m => Memory -> m Integer
  get_free_addr mem = do
    case freeCell mem of
      []  -> throwError $ memory_full
      f:_ -> return f
  
  clear_free_addr :: Memory -> Memory
  clear_free_addr m = m {freeCell = tail . freeCell $ m}
  
  update :: Memory -> Integer -> Expr -> Memory
  update m addr e = m {mem = (mem m)//[(addr, e)]}
  
  at :: Memory -> Integer -> Expr
  m `at` addr = (mem m) ! addr

  instance Show Memory where
    show _ = "Memory"
  
  empty_mem :: Integer -> Memory
  empty_mem n = M (listArray (1,n) $ replicate (fromInteger n) Null) [1..n] n
  
  data InterpreterState = S { 
                              typing_env         :: TypingEnv, 
                              variable_counter   :: Integer, 
                              constraints        :: [(Type, Type)], 
                              simple_constraints :: [Type], 
                              subst              :: Subst, 
                              eval_env           :: EvalEnv, 
                              memory             :: Memory
                            }
  
  empty_state :: InterpreterState
  empty_state = S {
                    typing_env         = \n -> Nothing, 
                    variable_counter   = 0, 
                    constraints        = [], 
                    simple_constraints = [], 
                    subst              = id, 
                    eval_env           = \n -> Nothing, 
                    memory             = empty_mem 200
                  }
  
  get_typing_env :: MonadState InterpreterState m => m TypingEnv
  get_typing_env = do
    s <- get
    return $ typing_env s
  
  extend_typing_env :: MonadState InterpreterState m => String -> Type -> m ()
  extend_typing_env v t = do
    s <- get
    put $ s {typing_env = \x -> if x == v then Just t else (typing_env s) x }
    return ()
  
  reset_typing_env :: MonadState InterpreterState m => TypingEnv -> m ()
  reset_typing_env env = do
    s <- get
    put $ s {typing_env = env}
    return ()
  
  fresh_type_var :: MonadState InterpreterState m => m Type
  fresh_type_var = do
    s <- get
    let n = variable_counter s
    put $ s {variable_counter = n + 1}
    return $ T_Var $ "a" ++ show n
  
  add_constraint :: MonadState InterpreterState m => Type -> Type -> m ()
  add_constraint t1 t2 = do
    s <- get
    put $ s {constraints = (t1, t2) : constraints s}
    return ()
  
  get_constraint :: MonadState InterpreterState m => m (Maybe (Type, Type))
  get_constraint = do
    s <- get
    case constraints s of
      []   -> return Nothing
      c:cs -> do
        put $ s {constraints = cs}
        return $ Just c
  
  add_simple_constraint :: MonadState InterpreterState m => Type -> m ()
  add_simple_constraint tp = do
    s <- get
    put $ s {simple_constraints = tp : simple_constraints s}
    return ()
  
  get_simple_constraints :: MonadState InterpreterState m => m [Type]
  get_simple_constraints = do
    s <- get
    return $ simple_constraints s
  
  get_subst :: MonadState InterpreterState m => m Subst
  get_subst = do
    s <- get
    return $ subst s
  
  compose_subst :: MonadState InterpreterState m => Subst -> m ()
  compose_subst z = do
    s <- get
    put $ s {subst = z . (subst s)}
    return ()
  
  get_memory :: MonadState InterpreterState m => m Memory
  get_memory = do
    s <- get
    return $ memory s
  
  store :: (MonadState InterpreterState m, MonadError String m) => Expr -> m Integer
  store v = do
    s <- get
    let mem = memory s
    addr <- get_free_addr mem
    let mem' = clear_free_addr mem
    put $ s {memory = update mem' addr v}
    return addr
  
  store_at :: (MonadState InterpreterState m, MonadError String m) => Integer -> Expr -> m ()
  store_at addr v = do
    s <- get
    let mem = memory s
    put $ s {memory = update mem addr v}
    return ()
  
  load :: MonadState InterpreterState m => Integer -> m Expr
  load addr = do
    mem <- get_memory
    return $ mem `at` addr
  
  get_eval_env :: MonadState InterpreterState m => m EvalEnv
  get_eval_env = do
    s <- get
    return $ eval_env s
  
  extend_eval_env :: MonadState InterpreterState m => String -> Expr -> m ()
  extend_eval_env v t = do
    s <- get
    put $ s {eval_env = \x -> if x == v then Just t else (eval_env s) x }
    return ()

