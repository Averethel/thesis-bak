module Utils.Interpreter (interpreter) where
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
  clear n size = evalLoop n 0 TE.emptyEnv EE.emptyEnv $ emptyMem n size

  printHelp :: (MonadException m, Language n p tp e i v) => n -> Integer -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  printHelp langName varCounter typingEnv evalEnv memory = do
    outputStrLn "HERE WOULD BE HELP"
    evalLoop langName varCounter typingEnv evalEnv memory

  checkType :: (MonadException m, Language n p tp e i v) => String -> n -> Integer -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  checkType exprS langName varCounter typingEnv evalEnv memory = do
    case parseExpression langName exprS of
      Left err   -> outputStrLn $ parseError err exprS
      Right expr ->
        case typeOfExpression varCounter typingEnv expr of
          Left err -> outputStrLn $ typingError err expr
          Right tp -> outputStrLn $ show tp
    evalLoop langName varCounter typingEnv evalEnv memory

  typeAndEvalProgram :: (MonadException m, Language n p tp e i v) => String -> n -> Integer -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  typeAndEvalProgram path langName varCounter typingEnv evalEnv memory = do
    res <- liftIO $ parseProgramFromFile langName path
    case res of
      Left err   -> do
        outputStrLn $ parseErrorFile err path
        evalLoop langName varCounter typingEnv evalEnv memory
      Right prog ->
        case typeOfProgram varCounter typingEnv prog of
          Left err           -> do
            outputStrLn $ typingErrorFile err path
            evalLoop langName varCounter typingEnv evalEnv memory
          Right (tp, te, vc) ->
            case evalProgram memory evalEnv prog of
              Left err           -> do
                outputStrLn $ evalErrorFile err path
                evalLoop langName varCounter typingEnv evalEnv memory
              Right (v, ee, mem) -> do
                outputStrLn $ show v ++ " : " ++ show tp
                evalLoop langName vc te ee mem

  handleException :: (MonadException m, Language n p tp e i v) => n -> Integer -> TE.Env tp -> EE.Env v e -> Memory v -> IOException -> InputT m ()
  handleException langName varCounter typingEnv evalEnv memory err = do
    outputStrLn $ show err
    evalLoop langName varCounter typingEnv evalEnv memory

  secureTypeAndEvalProgram :: (Language n p tp e i v, MonadException m) => String -> n -> Integer -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  secureTypeAndEvalProgram path langName varCounter typingEnv evalEnv memory =
    (typeAndEvalProgram path langName varCounter typingEnv evalEnv memory)
    `System.Console.Haskeline.catch`
    (handleException langName varCounter typingEnv evalEnv memory)

  typeAndEvalInstruction :: (MonadException m, Language n p tp e i v) => String -> n -> Integer -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  typeAndEvalInstruction instrS langName varCounter typingEnv evalEnv memory = do
    case parseInstruction langName instrS of
      Left err    -> do
        outputStrLn $ parseError err instrS
        evalLoop langName varCounter typingEnv evalEnv memory
      Right instr -> do
        case typeOfInstruction varCounter typingEnv instr of
          Left err           -> do
            outputStrLn $ typingError err instr
            evalLoop langName varCounter typingEnv evalEnv memory
          Right (tp, te, vc) -> do
            case evalInstruction memory evalEnv instr of
              Left err           -> do
                outputStrLn $ evalError err instr
                evalLoop langName varCounter typingEnv evalEnv memory
              Right (v, ee, mem) -> do
                case (v, tp) of
                  (Just val, Just typ) -> outputStrLn $ "it = " ++ show val ++ " : " ++ show typ
                  (Nothing,  Nothing)  -> outputStrLn "Defined."
                evalLoop langName vc te ee mem

  evalLoop :: (MonadException m, Language n p tp e i v) => n -> Integer -> TE.Env tp -> EE.Env v e -> Memory v -> InputT m ()
  evalLoop langName varCounter typingEnv evalEnv memory = do
    c <- getInputLine $ show langName ++ " :> "
    case c of
      Nothing               -> evalLoop langName varCounter typingEnv evalEnv memory

      Just ":h"             -> printHelp langName varCounter typingEnv evalEnv memory

      Just ":q"             -> return ()

      Just ":c"             -> clear langName $ size memory

      Just (':':'t':exprS)  -> checkType exprS langName varCounter typingEnv evalEnv memory

      Just (':':'l':path)   -> secureTypeAndEvalProgram path langName varCounter typingEnv evalEnv memory

      Just instrS           -> typeAndEvalInstruction instrS langName varCounter typingEnv evalEnv memory

  interpreter :: Language n p tp e i v => n -> Integer -> IO ()
  interpreter langName memSize = runInputT defaultSettings $ clear langName memSize
