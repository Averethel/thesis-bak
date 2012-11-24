{-# LANGUAGE
  TypeFamilies
  #-}

module Utils.Interpreter where
  import Utils.Classes.Clojure
  import Utils.Classes.Expression
  import Utils.Classes.Language
  import Utils.Classes.Type
  import Utils.Classes.Value

  import Utils.Errors
  import qualified Utils.EvalEnv   as EE
  import qualified Utils.TypingEnv as TE
  import Utils.Memory

  import Control.Monad.IO.Class
  import System.Console.Haskeline

  clear :: (MonadException m, Language n p tp e i v) => n -> Integer -> InputT m ()
  clear n size = evalLoop n TE.emptyEnv EE.emptyEnv $ emptyMem n size

  printHelp :: (MonadException m, Language n p tp e i v) => n -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  printHelp langName typingEnv evalEnv memory = do
    outputStrLn "HERE WOULD BE HELP"
    evalLoop langName typingEnv evalEnv memory

  checkType :: (MonadException m, Language n p tp e i v) => String -> n -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  checkType exprS langName typingEnv evalEnv memory = do
    case parseExpression langName exprS of
      Left err   -> do
        outputStrLn $ parseError err exprS
        evalLoop langName typingEnv evalEnv memory
      Right expr -> do
        case typeOfExpression typingEnv expr of
          Left err -> outputStrLn $ typingError err expr
          Right tp -> outputStrLn $ show tp
        evalLoop langName typingEnv evalEnv memory

  typeAndEvalProgram :: (MonadException m, Language n p tp e i v) => String -> n -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  typeAndEvalProgram path langName typingEnv evalEnv memory = do
    res <- liftIO $ parseProgramFromFile langName path
    case res of
      Left err   -> do
        outputStrLn $ parseErrorFile err path
        evalLoop langName typingEnv evalEnv memory
      Right prog ->
        case typeOfProgram typingEnv prog of
          Left err       -> do
            outputStrLn $ typingErrorFile err path
            evalLoop langName typingEnv evalEnv memory
          Right (tp, te) ->
            case evalProgram memory evalEnv prog of
              Left err           -> do
                outputStrLn $ evalErrorFile err path
                evalLoop langName typingEnv evalEnv memory
              Right (v, ee, mem) -> do
                outputStrLn $ show v ++ " : " ++ show tp
                evalLoop langName te ee mem

  handleException :: (MonadException m, Language n p tp e i v) => n -> TE.Env tp -> EE.Env v e -> Memory v -> IOException -> InputT m ()
  handleException langName typingEnv evalEnv memory err = do
    outputStrLn $ show err
    evalLoop langName typingEnv evalEnv memory

  secureTypeAndEvalProgram :: (Language n p tp e i v, MonadException m) => String -> n -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  secureTypeAndEvalProgram path langName typingEnv evalEnv memory =
    (typeAndEvalProgram path langName typingEnv evalEnv memory)
    `System.Console.Haskeline.catch`
    (handleException langName typingEnv evalEnv memory)

  typeAndEvalInstruction :: (MonadException m, Language n p tp e i v) => String -> n -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  typeAndEvalInstruction instrS langName typingEnv evalEnv memory = do
    case parseInstruction langName instrS of
      Left err    -> do
        outputStrLn $ parseError err instrS
        evalLoop langName typingEnv evalEnv memory
      Right instr -> do
        case typeOfInstruction typingEnv instr of
          Left err       -> do
            outputStrLn $ typingError err instr
            evalLoop langName typingEnv evalEnv memory
          Right (tp, te) -> do
            case evalInstruction memory evalEnv instr of
              Left err           -> do
                outputStrLn $ evalError err instr
                evalLoop langName typingEnv evalEnv memory
              Right (v, ee, mem) -> do
                case (v, tp) of
                  (Just val, Just typ) -> outputStrLn $ "it = " ++ show val ++ " : " ++ show tp
                  (Nothing,  Nothing)  -> outputStrLn "Defined."
                evalLoop langName te ee mem

  evalLoop :: (MonadException m, Language n p tp e i v) => n -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  evalLoop langName typingEnv evalEnv memory = do
    c <- getInputLine $ show langName ++ " :>"
    case c of
      Nothing               -> evalLoop langName typingEnv evalEnv memory

      Just ":h"             -> printHelp langName typingEnv evalEnv memory

      Just ":q"             -> return ()

      Just ":c"             -> clear langName $ size memory

      Just (':':'t':exprS)  -> checkType exprS langName typingEnv evalEnv memory

      Just (':':'l':path)   -> secureTypeAndEvalProgram path langName typingEnv evalEnv memory

      Just instrS           -> typeAndEvalInstruction instrS langName typingEnv evalEnv memory
