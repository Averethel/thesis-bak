{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Interpreter where
  import Utils.Errors

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

  getMem :: MonadState InterpreterState m => m [(Integer, Expr)]
  getMem = do
    (M m _ _) <- getMemory
    return $ assocs m

  showMemory :: (MonadIO m, MonadException m) => [(Integer, Expr)] -> InputT m ()
  showMemory []             = outputStrLn ""
  showMemory ((_, Null):ms) = showMemory ms
  showMemory ((n, e):ms)    = do
    outputStrLn $ show n ++ ":\t\t" ++ show e
    showMemory ms

  getType :: (MonadState InterpreterState m, MonadError String m) => Expr -> m String
  getType expr = 
    do {
      t <- typeOfExpression expr;
      f <- unify;
      checkCompare;
      return . show $ f t }
    `catchError`
      (\e -> return $ typingError e expr)

  evaluate :: (MonadState InterpreterState m, MonadError String m) => Instruction -> m String
  evaluate (IEX ex) = do {
      t <- typeOfExpression ex;
      f <- unify;
      checkCompare;
      v <- evalExpr ex;
      extendTypingEnv "it" t;
      extendEvalEnv "it" v;
      return $ show v ++ " : " ++ show (f t) }
    `catchError`
      (\e -> return $ evalError e ex)
  evaluate (IDF df) = do {
      typeOfDefinition df;
      f <- unify;
      checkCompare;
      evalDefinition df;
      return "Defined." }
    `catchError`
      (\e -> return $ evalError e df)

  evaluateProgram :: (MonadState InterpreterState m, MonadError String m) => Program -> m String
  evaluateProgram prog = do {
      typeOfProgram prog;
      f <- unify;
      checkCompare;
      v <- evalProgram prog;
      t <- typeOfExpression v;
      return $ show v ++ " : " ++ show (f t) }
    `catchError`
      (\e -> return $ evalError e prog)

  evalLoop :: MonadException m => InterpreterState -> InputT m ()
  evalLoop state = do
    c <- getInputLine ":> "
    case c of
      Nothing                 -> evalLoop state
      Just ":q"               -> return ()
      Just ":c"               -> evalLoop emptyState
      Just ":mem"             -> do
        (m, s) <- runStateT getMem state
        showMemory m
        evalLoop s
      Just (':':'t':' ':rest) -> do
        case expressionParser rest of
          Left err   -> do
            outputStrLn $ parseError err rest
            evalLoop state
          Right expr -> do
            (Right (m, s)) <- runErrorT $ runStateT (getType expr) state
            outputStrLn m
            evalLoop s
      Just (':':'l':' ':file) -> do
        res <- liftIO $ try $ parseFromFile program file
        case res of
          Left err           -> do
            outputStrLn $ show err
            evalLoop state
          Right (Left err)   -> do
            outputStrLn $ parseError err file
            evalLoop state
          Right (Right prog) -> do
            (Right (m, s)) <- runErrorT $ runStateT (evaluateProgram prog) state
            outputStrLn m
            evalLoop s  
      Just instr             -> do
        case inputParser instr of
          Left err   -> do
            outputStrLn $ parseError err instr
            evalLoop state
          Right instr -> do
            (Right (m, s)) <- runErrorT $ runStateT (evaluate instr) state
            outputStrLn m
            evalLoop s

  interpreter :: IO ()
  interpreter = runInputT defaultSettings (evalLoop emptyState)