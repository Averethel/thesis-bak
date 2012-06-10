{-# LANGUAGE
  FlexibleContexts
  #-}
module Languages.EnrichedLambda.Interpreter where
  import Languages.EnrichedLambda.Errors
  import Languages.EnrichedLambda.Eval
  import Languages.EnrichedLambda.Parser
  import Languages.EnrichedLambda.State
  import Languages.EnrichedLambda.Syntax
  import Languages.EnrichedLambda.Typing
  import Languages.EnrichedLambda.Unification
  
  import Control.Monad.Error
  import Control.Monad.State
  import Data.Array
  import System.Console.Haskeline
  
  get_mem :: MonadState InterpreterState m => m [(Integer, Expr)]
  get_mem = do
    (M m _ _) <- get_memory
    return $ assocs m
  
  show_mem :: (MonadIO m, MonadException m) => [(Integer, Expr)] -> InputT m ()
  show_mem []             = outputStrLn ""
  show_mem ((_, Null):ms) = show_mem ms
  show_mem ((n, e):ms)    = do
    outputStrLn $ show n ++ ":\t\t" ++ show e
    show_mem ms
  
  get_type :: (MonadState InterpreterState m, MonadError String m) => Expr -> m String
  get_type expr = 
    do {
      t <- type_of_expression expr;
      f <- unify;
      check_compare;
      return . show $ f t }
    `catchError`
      (\e -> return $ typing_error e expr)
  
  evaluate :: (MonadState InterpreterState m, MonadError String m) => Expr -> m String
  evaluate expr = do {
      t <- type_of_expression expr;
      f <- unify;
      check_compare;
      v <- eval_expr expr;
      add_to_typing_env "it" t;
      add_to_eval_env "it" v;
      return $ show v ++ " : " ++ show (f t) }
    `catchError`
      (\e -> return $ eval_error e expr)
  
  eval_loop :: MonadException m => InterpreterState -> InputT m ()
  eval_loop state = do
    c <- getInputLine ":> "
    case c of
      Nothing                 -> eval_loop state
      Just ":q"               -> return ()
      Just ":c"               -> eval_loop empty_state
      Just ":mem"             -> do
        (m, s) <- runStateT get_mem state
        show_mem m
        eval_loop s
      Just (':':'t':' ':rest) -> do
        case expressionParser rest of
          Left err   -> do
            outputStrLn $ parse_error err rest
            eval_loop state
          Right expr -> do
            (Right (m, s)) <- runErrorT $ runStateT (get_type expr) state
            outputStrLn m
            eval_loop s
      Just expr               -> do
        case expressionParser expr of
          Left err   -> do
            outputStrLn $ parse_error err expr
            eval_loop state
          Right expr -> do
            (Right (m, s)) <- runErrorT $ runStateT (evaluate expr) state
            outputStrLn m
            eval_loop s

  interpreter :: IO ()
  interpreter = runInputT defaultSettings (eval_loop empty_state)
