{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.EnrichedLambdaLowLevelTypes.Eval where
  import Languages.EnrichedLambdaLowLevelTypes.Errors
  import Languages.EnrichedLambdaLowLevelTypes.Syntax
  import Languages.EnrichedLambdaLowLevelTypes.State
  
  import Control.Monad.Error
  import Control.Monad.State
  
  is_pointer :: Struct -> Bool
  is_pointer (S_Ptr _)       = True
  is_pointer (S_StaticPtr _) = True
  -- is_pointer (S_Ref _)       = True
  is_pointer _               = False
  
  is_function :: Expr -> Bool
  is_function (E_Prim _)             = True
  is_function (E_Function _ _)       = True
  is_function (E_Apply (E_Prim p) _) = is_binary p
  is_function _                      = False
  
  is_value :: Expr -> Bool
  is_value (E_Prim _)                          = True
  is_value (E_Function _ _)                    = True
  is_value (E_Struct (S_Ref e))                = is_value e
  is_value (E_Struct (S_Str Tg_True _ _))      = True
  is_value (E_Struct (S_Str Tg_False _ _))     = True
  is_value (E_Struct (S_Str Tg_Unit _ _))      = True
  is_value (E_Struct (S_Str Tg_Nil _ _))       = True
  is_value (E_Struct (S_Str Tg_Cons _ [a, b])) = is_value (E_Struct a) && is_value (E_Struct b)
  is_value (E_Struct (S_Str Tg_Pair _ [a, b])) = is_value (E_Struct a) && is_value (E_Struct b)
  is_value (E_Struct (S_Ptr _))                = True
  is_value (E_Struct (S_StaticPtr _))          = True
  is_value (E_Struct (S_Int _))                = True
  is_value (E_Apply (E_Prim p) v)              = is_value v && is_binary p
  is_value _                                   = False
  
  alloc_mem :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Struct
  alloc_mem expr
    | is_function expr = alloc_static_mem expr
    | otherwise        = alloc_dynamic_mem expr
    where
      alloc_static_mem e  = do
        ptr <- store Static e
        return $ S_StaticPtr ptr
      alloc_dynamic_mem s = do
        ptr <- store Dynamic s
        return $ S_Ptr ptr
  
  eval_prim :: (MonadError String m, MonadState InterpreterState m) => Prim -> [Expr] -> m Expr
  eval_prim P_AllocPair [e1, e2]                                                        = do
    p1 <- alloc_mem e1
    p2 <- alloc_mem e2
    return $ E_Struct $ S_Str Tg_Pair 2 [p1, p2]
  eval_prim P_AllocList [e1, e2]                                                        = do
    let v1                                                                              = case e1 of
                E_Struct s -> s
                e          -> S_Ref e
    p <- alloc_mem e2
    return $ E_Struct $ S_Str Tg_Cons 2 [v1, p]
  eval_prim P_Not [E_Struct (S_Str Tg_True s a)]                                        = return $ E_Struct $ S_Str Tg_False s a
  eval_prim P_Not [E_Struct (S_Str Tg_False s a)]                                       = return $ E_Struct $ S_Str Tg_True s a
  eval_prim P_Ref [e]                                                                   = do
    ptr <- alloc_mem e
    return $ E_Struct ptr
  eval_prim P_Deref [E_Struct (S_StaticPtr ptr)]                                        = load Static ptr
  eval_prim P_Deref [E_Struct (S_Ptr ptr)]                                              = load Dynamic ptr
  eval_prim P_Deref [E_Struct (S_Ref e)]                                                = return e
  eval_prim P_Fst [E_Struct (S_Str Tg_Pair _ [ptr1, _])]                                = return $ E_Apply (E_Prim P_Deref) $ E_Struct ptr1
  eval_prim P_Snd [E_Struct (S_Str Tg_Pair _ [_, ptr2])]                                = return $ E_Apply (E_Prim P_Deref) $ E_Struct ptr2
  eval_prim P_Head [E_Struct (S_Str Tg_Cons _ [h, _])]                                  = return $ E_Struct h
  eval_prim P_Tail [E_Struct (S_Str Tg_Cons _ [_, ptr])]                                = return $ E_Apply (E_Prim P_Deref) $ E_Struct ptr
  eval_prim P_Empty [E_Struct (S_Str Tg_Nil _ _)]                                       = return $ E_Struct $ S_Str Tg_True 0 []
  eval_prim P_Empty [E_Struct (S_Str Tg_Cons _ _)]                                      = return $ E_Struct $ S_Str Tg_False 0 []
  eval_prim P_Eq [E_Struct (S_Ref e1), E_Struct (S_Ref e2)]                             = return $ E_Apply (E_Apply (E_Prim P_Eq) e1) e2
  eval_prim P_Eq [E_Struct (S_Str Tg_Nil _ _), E_Struct (S_Str Tg_Cons _ _)]            = return $ E_Struct $ S_Str Tg_False 0 []
  eval_prim P_Eq [E_Struct (S_Str Tg_Cons _ _), E_Struct (S_Str Tg_Nil _ _)]            = return $ E_Struct $ S_Str Tg_False 0 []
  eval_prim P_Eq [E_Struct (S_Str Tg_Cons _ [a, b]), E_Struct (S_Str Tg_Cons _ [c, d])] = 
    return $ E_ITE (E_Apply (E_Apply (E_Prim P_Eq) (E_Struct a)) (E_Struct c)) (E_Apply (E_Apply (E_Prim P_Eq) (E_Struct b)) (E_Struct d)) $ E_Struct $ S_Str Tg_False 0 []
  eval_prim P_Eq [E_Struct (S_Str Tg_Pair _ [a, b]), E_Struct (S_Str Tg_Pair _ [c, d])] = 
    return $ E_ITE (E_Apply (E_Apply (E_Prim P_Eq) (E_Struct a)) (E_Struct c)) (E_Apply (E_Apply (E_Prim P_Eq) (E_Struct b)) (E_Struct d)) $ E_Struct $ S_Str Tg_False 0 []
  eval_prim P_Eq [e1@(E_Struct s1), e2@(E_Struct s2)]
    | is_pointer s1 && is_pointer s2                                                    = return $ E_Apply (E_Apply (E_Prim P_Eq) (E_Apply (E_Prim P_Deref) e1)) $ E_Apply (E_Prim P_Deref) e2
    | s1 == s2                                                                          = return $ E_Struct $ S_Str Tg_True 0 []
    | otherwise                                                                         = return $ E_Struct $ S_Str Tg_False 0 []
  eval_prim P_Plus [E_Struct (S_Int n), E_Struct (S_Int m)]                             = return $ E_Struct $ S_Int (n+m)
  eval_prim P_Minus [E_Struct (S_Int n), E_Struct (S_Int m)]                            = return $ E_Struct $ S_Int (n-m)
  eval_prim P_Mult [E_Struct (S_Int n), E_Struct (S_Int m)]                             = return $ E_Struct $ S_Int (n*m)
  eval_prim P_Div [E_Struct (S_Int n), E_Struct (S_Int m)]
    | m /= 0                                                                            = return $ E_Struct $ S_Int (n+m)
    | otherwise                                                                         = throwError division_by_0
  eval_prim P_Assign [E_Struct (S_Ptr n), e]                                            = do
    store_at Dynamic n e
    return $ E_Struct $ S_Str Tg_Unit 0 []
  eval_prim P_Assign [E_Struct (S_StaticPtr n), e]                                      = do
    store_at Static n e
    return $ E_Struct $ S_Str Tg_Unit 0 []
  
  eval_step_expr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  eval_step_expr (E_Val s)                                   = do
    env <- get_eval_env
    case env s of
      Nothing -> throwError $ unbound_variable s
      Just e  -> return e
  eval_step_expr (E_ITE e1 e2 e3)
    | not . is_value $ e1                                    = do
      e1' <- eval_step_expr e1
      return $ E_ITE e1' e2 e3
  eval_step_expr (E_ITE (E_Struct (S_Str Tg_True _ _)) e1 _) = return e1
  eval_step_expr (E_ITE (E_Struct (S_Str Tg_False _ _)) _ e) = return e
  eval_step_expr (E_Seq e1 e2)
    | not . is_value $ e1                                    = do
      e1' <- eval_step_expr e1
      return $ E_Seq e1' e2
  eval_step_expr (E_Seq (E_Struct (S_Str Tg_Unit _ _)) e2)   = return e2
  eval_step_expr (E_Let s e1 e2) 
    | not . is_value $ e1                                    = do
      e1' <- eval_step_expr e1
      return $ E_Let s e1' e2
    | otherwise                                              = do
      extend_eval_env s e1
      return e2
  eval_step_expr (E_LetRec s e1 e2)                          = do
    extend_eval_env s e1
    return e2
  eval_step_expr (E_Apply e1 e2)
    | not . is_value $ e1                                    = do
      e1' <- eval_step_expr e1
      return $ E_Apply e1' e2
    | is_value e1 && (not . is_value $ e2)                   = do
      e2' <- eval_step_expr e2
      return $ E_Apply e1 e2'
  eval_step_expr (E_Apply (E_Function s e) e2)               = do
    extend_eval_env s e2
    return e
  eval_step_expr (E_Apply (E_Apply (E_Prim p) e1) e2)
    | is_binary p                                            = eval_prim p [e1, e2]
  eval_step_expr (E_Apply (E_Prim p) e)
    | not . is_binary $ p                                    = eval_prim p [e]
  eval_step_expr E_MatchFailure                              = throwError match_failure


  eval_expr :: (MonadError String m, MonadState InterpreterState m) => Expr -> m Expr
  eval_expr e
    | is_value e = return e
    | otherwise  = do 
      e' <- eval_step_expr e
      eval_expr e'

    