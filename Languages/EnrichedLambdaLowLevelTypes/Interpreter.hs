{-# LANGUAGE
  FlexibleContexts
  #-}
module Languages.EnrichedLambdaLowLevelTypes.Interpreter where
  import Languages.EnrichedLambdaLowLevelTypes.Errors
  import Languages.EnrichedLambdaLowLevelTypes.Eval
  import Languages.EnrichedLambdaLowLevelTypes.Parser
  import Languages.EnrichedLambdaLowLevelTypes.State
  import Languages.EnrichedLambdaLowLevelTypes.Syntax
  import Languages.EnrichedLambdaLowLevelTypes.Typing
  import Languages.EnrichedLambdaLowLevelTypes.Unification
  
  import Control.Monad.Error
  import Control.Monad.State
  import Data.Array
  import Data.Maybe
  import System.Console.Haskeline
  import System.IO.Error
  import Text.Parsec.String (parseFromFile)
  
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
      s <- get;
      ex <- show_expression v s;
      return $  ex ++ " : " ++ show (f t) }
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
  
  show_expression :: (MonadState InterpreterState m, MonadError String m) => Expr -> InterpreterState -> m String
  show_expression (E_Val e) s = show_expression (fromJust $ eval_env s $ e) s
  show_expression e s = do
    r <- runErrorT $ runStateT (do { t <- type_of_expression e; f <- unify; check_compare; return $ f t}) s
    case r of
      Left e          -> return $ show e
      (Right (tp, s)) ->
        case tp of
          T_List _   -> show_list e s
          T_Pair _ _ -> show_pair e s
          _          -> return . show $ e
  
  show_pair :: (MonadState InterpreterState m, MonadError String m) => Expr -> InterpreterState -> m String
  show_pair (E_Struct (S_Str Tg_Pair _ [S_Ptr a, S_Ptr b]))             s = do
    let mem = dynamic_memory s
    let e1  = mem `at` a
    let e2  = mem `at` b
    e1_s <- show_expression e1 s
    e2_s <- show_expression e2 s
    return $ "( " ++ e1_s ++ ", " ++ e2_s ++ " )"
  show_pair (E_Struct (S_Str Tg_Pair _ [S_Ptr a, S_StaticPtr b]))       s = do
    let mem = dynamic_memory s
    let sm  = static_memory s
    let e1  = mem `at` a
    let e2  = sm `at` b
    e1_s <- show_expression e1 s
    e2_s <- show_expression e2 s
    return $ "( " ++ e1_s ++ ", " ++ e2_s ++ " )"
  show_pair (E_Struct (S_Str Tg_Pair _ [S_StaticPtr a, S_Ptr b]))       s = do
    let mem = dynamic_memory s
    let sm  = static_memory s
    let e1  = sm `at` a
    let e2  = mem `at` b
    e1_s <- show_expression e1 s
    e2_s <- show_expression e2 s
    return $ "( " ++ e1_s ++ ", " ++ e2_s ++ " )"
  show_pair (E_Struct (S_Str Tg_Pair _ [S_StaticPtr a, S_StaticPtr b])) s = do
    let sm  = static_memory s
    let e1  = sm `at` a
    let e2  = sm `at` b
    e1_s <- show_expression e1 s
    e2_s <- show_expression e2 s
    return $ "( " ++ e1_s ++ ", " ++ e2_s ++ " )"
  
  show_list :: (MonadState InterpreterState m, MonadError String m) => Expr -> InterpreterState -> m String
  show_list (E_Struct (S_Str Tg_Nil _ _))                   _ = return "[]"
  show_list (E_Struct (S_Str Tg_Cons _ [a, S_Ptr b]))       s = do
    let mem = dynamic_memory s
    let e   = mem `at` b
    e1_s <- show_expression (E_Struct a) s
    e2_s <- show_expression e s
    return $ e1_s ++ " :: (" ++ e2_s ++ " )"
  show_list (E_Struct (S_Str Tg_Cons _ [a, S_StaticPtr b])) s = do
    let mem = static_memory s
    let e   = mem `at` b
    e1_s <- show_expression (E_Struct a) s
    e2_s <- show_expression e s
    return $ e1_s ++ " :: (" ++ e2_s ++ " )"
  
  eval_loop :: MonadException m => InterpreterState -> InputT m ()
  eval_loop state = do
    c <- getInputLine ":> "
    case c of
      Nothing                       -> eval_loop state
      Just ":q"                     -> return ()
      Just ":c"                     -> eval_loop empty_state
      Just (':':'m':'e':'m':' ':'d':rest) -> do
        case naturalParser rest of
          Left err   -> outputStrLn $ parse_error err rest
          Right addr -> outputStrLn $ show $ (dynamic_memory state) `at` addr
      Just (':':'m':'e':'m':' ':'s':rest) -> do
        case naturalParser rest of
          Left err   -> outputStrLn $ parse_error err rest
          Right addr -> outputStrLn $ show $ (static_memory state) `at` addr
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
      Just instr               -> do
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
