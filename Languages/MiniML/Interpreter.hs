{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Interpreter where
  import Languages.MiniML.Errors
  import Languages.MiniML.Eval
  import Languages.MiniML.Parser
  import Languages.MiniML.Syntax
  import Languages.MiniML.State
  import Languages.MiniML.Typing
  import Languages.MiniML.Unification
  
  import Control.Monad.Error
  import Control.Monad.State
  import Data.Array
  import System.Console.Haskeline
  import System.IO.Error
  import Text.Parsec.String (parseFromFile)

  get_mem :: MonadState InterpreterState m => m [(Integer, Expr)]
  get_mem = do
    (M m _ _) <- get_memory
    return $ assocs m

  show_memory :: (MonadIO m, MonadException m) => [(Integer, Expr)] -> InputT m ()
  show_memory []             = outputStrLn ""
  show_memory ((_, Null):ms) = show_memory ms
  show_memory ((n, e):ms)    = do
    outputStrLn $ show n ++ ":\t\t" ++ show e
    show_memory ms

  get_type :: (MonadState InterpreterState m, MonadError String m) => Expr -> m String
  get_type expr = 
    do {
      t <- type_of_expression expr;
      f <- unify;
      check_compare;
      return . show $ f t }
    `catchError`
      (\e -> return $ typing_error e expr)

  evaluate :: (MonadState InterpreterState m, MonadError String m) => Instruction -> m String
  evaluate (IEX ex) = do {
      t <- type_of_expression ex;
      f <- unify;
      check_compare;
      v <- eval_expr ex;
      extend_typing_env "it" t;
      extend_eval_env "it" v;
      return $ show v ++ " : " ++ show (f t) }
    `catchError`
      (\e -> return $ eval_error e ex)
  evaluate (IDF df) = do {
      type_of_definition df;
      f <- unify;
      check_compare;
      eval_definition df;
      return "Defined." }
    `catchError`
      (\e -> return $ eval_error e df)

  evaluate_program :: (MonadState InterpreterState m, MonadError String m) => Program -> m String
  evaluate_program prog = do {
      type_of_program prog;
      f <- unify;
      check_compare;
      v <- eval_program prog;
      t <- type_of_expression v;
      return $ show v ++ " : " ++ show (f t) }
    `catchError`
      (\e -> return $ eval_error e prog)

  eval_loop :: MonadException m => InterpreterState -> InputT m ()
  eval_loop state = do
    c <- getInputLine ":> "
    case c of
      Nothing                 -> eval_loop state
      Just ":q"               -> return ()
      Just ":c"               -> eval_loop empty_state
      Just ":mem"             -> do
        (m, s) <- runStateT get_mem state
        show_memory m
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
      Just (':':'l':' ':file) -> do
        res <- liftIO $ try $ parseFromFile program file
        case res of
          Left err           -> do
            outputStrLn $ show err
            eval_loop state
          Right (Left err)   -> do
            outputStrLn $ parse_error err file
            eval_loop state
          Right (Right prog) -> do
            (Right (m, s)) <- runErrorT $ runStateT (evaluate_program prog) state
            outputStrLn m
            eval_loop s  
      Just instr             -> do
        case inputParser instr of
          Left err   -> do
            outputStrLn $ parse_error err instr
            eval_loop state
          Right instr -> do
            (Right (m, s)) <- runErrorT $ runStateT (evaluate instr) state
            outputStrLn m
            eval_loop s

  interpreter :: IO ()
  interpreter = runInputT defaultSettings (eval_loop empty_state)