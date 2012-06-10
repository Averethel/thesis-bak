{-# LANGUAGE
  FlexibleContexts,
  TypeSynonymInstances
  #-}

module Languages.EnrichedLambdaLowLevelTypes.State where
  import Languages.EnrichedLambdaLowLevelTypes.Errors
  import Languages.EnrichedLambdaLowLevelTypes.Syntax
  
  import Control.Monad.Error
  import Control.Monad.State
  import Data.Array
  
  type TypingEnv = String -> Maybe Type

  instance Show TypingEnv where
    show _ = "TypingEnv"
  
  type EvalEnv = String -> Maybe Expr
  
  instance Show EvalEnv where
    show _ = "EvalEnv"
  
  type Subst = Type -> Type
  
  instance Show Subst where
    show _ = "Subst"
  
  data MemoryType =
      Static
    | Dynamic
    deriving (Show, Eq)
  
  data Memory = M {
                    memory   :: Array Integer Struct,
                    freeCell :: [Integer],
                    used     :: Integer
                  }
  
  empty_mem :: Integer -> Memory
  empty_mem n = M (listArray (1,n) $ replicate (fromInteger n) Null) [1..n] n
  
  get_free_addr :: MonadError String m => Memory -> m Integer
  get_free_addr mem = do
    case freeCell mem of
      []  -> throwError $ memory_full
      f:_ -> return f
  
  clear_free_addr :: Memory -> Memory
  clear_free_addr mem = mem {freeCell = tail . freeCell $ mem}
  
  update :: Memory -> Integer -> Expr -> Memory
  update mem addr (E_Struct v) = mem {memory = (memory mem)//[(addr, v)]}
  update mem addr e            = mem {memory = (memory mem)//[(addr, S_Ref e)]}
  
  at :: Memory -> Integer -> Expr
  mem `at` addr = case (memory mem) ! addr of
    S_Ref e -> e
    s       -> E_Struct s
  
  instance Show Memory where
    show _ = "Memory"
  
  data InterpreterState = S {
                              typing_env          :: TypingEnv,
                              variable_counter    :: Integer,
                              constraints         :: [(Type, Type)],
                              simple_constraints  :: [Type],
                              subst               :: Subst,
                              eval_env            :: EvalEnv,
                              static_memory       :: Memory,
                              dynamic_memory      :: Memory
                            }
  
  empty_state :: InterpreterState
  empty_state = S (\_ -> Nothing) 0 [] [] id (\_ -> Nothing) (empty_mem 200) $ empty_mem 200
  
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
  
  get_memory :: MonadState InterpreterState m => MemoryType -> m Memory
  get_memory Static = do
    s <- get
    return $ static_memory s
  get_memory Dynamic = do
    s <- get
    return $ dynamic_memory s
  
  store :: (MonadState InterpreterState m, MonadError String m) => MemoryType -> Expr -> m Integer
  store Static v = do
    s <- get
    let mem = static_memory s
    addr <- get_free_addr mem
    let mem' = clear_free_addr mem
    put $ s {static_memory = update mem' addr v}
    return addr
  store Dynamic v = do
    s <- get
    let mem = dynamic_memory s
    addr <- get_free_addr mem
    let mem' = clear_free_addr mem
    put $ s {dynamic_memory = update mem' addr v}
    return addr
  
  store_at :: (MonadState InterpreterState m, MonadError String m) => MemoryType -> Integer -> Expr -> m ()
  store_at Static addr v = do
    s <- get
    let mem = static_memory s
    put $ s {static_memory = update mem addr v}
    return ()
  store_at Dynamic addr v = do
    s <- get
    let mem = dynamic_memory s
    put $ s {dynamic_memory = update mem addr v}
    return ()
  
  load :: MonadState InterpreterState m => MemoryType -> Integer -> m Expr
  load mt addr = do
    mem <- get_memory mt
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
