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
                    memory   :: Array Integer Expr,
                    freeCell :: [Integer],
                    used     :: Integer
                  }
  instance Show Memory where
    show _ = "Memory"
  
  empty_mem :: Integer -> Memory
  empty_mem n = M (listArray (1,n) $ replicate (fromInteger n) Null) [1..n] n
  
  type InterpreterState = (TypingEnv, Integer, [(Type, Type)], [Type], Subst, EvalEnv, Memory)
  
  empty_state :: InterpreterState
  empty_state = (\n -> Nothing, 0, [], [], id, \n -> Nothing, empty_mem 200)
  
  get_const_var :: MonadState InterpreterState m => m Integer
  get_const_var = do
    (_, n, _, _, _, _, _) <- get
    return n
  
  inc_const_var :: MonadState InterpreterState m => m ()
  inc_const_var = do
    (e, n, c, s, f, v, m) <- get
    put (e, n+1, c, s, f, v, m)
    return ()
  
  get_typing_env :: MonadState InterpreterState m => m TypingEnv
  get_typing_env = do
    (e, _, _, _, _, _, _) <- get
    return e
  
  reset_typing_env :: MonadState InterpreterState m => TypingEnv -> m ()
  reset_typing_env e = do
     (_, n, cs, scs, f, v, m) <- get
     put (e, n, cs, scs, f, v, m)
     return ()
  
  add_to_typing_env :: MonadState InterpreterState m => String -> Type -> m ()
  add_to_typing_env vn tp = do
    (env, n, cs, scs, f, v, m) <- get
    put (\x -> if x == vn then Just tp else env x, n, cs, scs, f, v, m)
    return ()

  get_eval_env :: MonadState InterpreterState m => m EvalEnv
  get_eval_env = do
    (_, _, _, _, _, e, _) <- get
    return e
  
  reset_eval_env :: MonadState InterpreterState m => EvalEnv -> m ()
  reset_eval_env e = do
     (v, n, cs, scs, f, _, m) <- get
     put (v, n, cs, scs, f, e, m)
     return ()
  
  add_to_eval_env :: MonadState InterpreterState m => String -> Expr -> m ()
  add_to_eval_env vn ex = do
    (v, n, cs, scs, f, env, m) <- get
    put (v, n, cs, scs, f, \x -> if x == vn then Just ex else env x, m)
    return ()
  
  add_constraint :: MonadState InterpreterState m => Type -> Type -> m ()
  add_constraint t1 t2 = do
    (e, n, cs, scs, f, v, m) <- get
    put (e, n, (t1, t2):cs, scs, f, v, m)
    return ()
  
  get_constraint :: MonadState InterpreterState m => m (Maybe (Type, Type))
  get_constraint = do
    (e, n, cs, scs, f, v, m) <- get
    case cs of
      []      -> return Nothing
      (c:cs') -> do
        put (e, n, cs', scs, f, v, m)
        return $ Just c
  
  add_simple_constraint :: MonadState InterpreterState m => Type -> m ()
  add_simple_constraint tp = do
    (e, n, cs, scs, f, v, m) <- get
    put (e, n, cs, tp:scs, f, v,  m)
    return ()
  
  get_simple_constraints :: (MonadState InterpreterState m) => m [Type]
  get_simple_constraints = do
    (_, _, _, scs, _, _, _) <- get
    return scs
  
  get_subst :: MonadState InterpreterState m => m Subst
  get_subst = do
    (_, _, _, _, f, _, _) <- get
    return f
  
  compose_subst :: MonadState InterpreterState m => Subst -> m ()
  compose_subst z = do
    (e, n, c, s, f, v, m) <- get
    put (e, n, c, s, z . f, v, m)
    return ()
  
  get_memory :: MonadState InterpreterState m => m Memory
  get_memory = do
    (_, _, _, _, _, _, mem) <- get
    return mem
  
  get_free_addr :: (MonadError String m, MonadState InterpreterState m) => m Integer
  get_free_addr = do
    (e, n, c, s, f, v, m) <- get
    case m of
      (M _ [] _)        -> throwError $ memory_full
      (M m (fc:fcs) fr) -> do
        put (e, n, c, s, f, v, (M m fcs $ fr - 1))
        return fc
  
  update_mem :: MonadState InterpreterState m => Integer -> Expr -> m ()
  update_mem addr ex = do
    (e, n, c, s, f, v, (M m fcs fr)) <- get
    put (e, n, c, s, f, v, (M (m//[(addr, ex)]) fcs fr))
    return ()
  
  lookup_mem :: MonadState InterpreterState m => Integer -> m Expr
  lookup_mem l = do
    (M m _ _) <- get_memory
    return $ m ! l
  
  alloc_addr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Integer
  alloc_addr ex = do
    fc <- get_free_addr
    update_mem fc ex
    return fc
  
  fresh_type_var :: (MonadState InterpreterState m) => m Type
  fresh_type_var = do
    n <- get_const_var
    inc_const_var
    return $ T_Var $ "a" ++ show n
