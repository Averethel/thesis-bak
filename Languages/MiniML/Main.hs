module Languages.MiniML.Main (main) where
  import Languages.MiniML.Errors
  import Languages.MiniML.Syntax
  import Languages.MiniML.Parser
  import Languages.MiniML.Typing
  import Languages.MiniML.Unification
  import Languages.MiniML.Eval

  import Control.Monad.Reader
  import Data.Maybe
  import System.Console.Haskeline
  import System.IO.Error
  import Text.Parsec.String (parseFromFile)

  show_type :: Expr -> TypeExpr -> String
  show_type ex tp = "it = " ++ show ex ++ " : " ++ show tp 

  show_values []     = outputStr ""
  show_values (v:vs) = do
    outputStrLn $ show v
    show_values vs

  show_memory = show_mem 0 where
    show_mem _ []     = outputStr ""
    show_mem n (e:es) = do
      outputStrLn $ show n ++ ": " ++ show e
      show_mem (n+1) es

  main :: IO ()
  main = runInputT defaultSettings $ loop empty_state where
    loop :: InterpreterState -> InputT IO ()
    loop state = do
      inp <- getInputLine ":> "
      case inp of
        Nothing                  -> return ()
        Just ":q"                -> return ()
        Just ":clear"            -> loop empty_state
        Just ":env v"            -> do
          let (_, (env, _)) = state
          show_values env
          loop state
        Just ":env t"            -> do
          let ((env, _, _, _), _) = state
          show_values env
          loop state
        Just ":mem"              -> do
          let (_, (_, mem)) = state
          show_memory mem
          loop state
        Just ":cs"               -> do
          let ((_, cs, _, _), _) = state
          show_values cs
          loop state
        Just (':':'l':' ':file)  -> do 
          res <- liftIO $ try $ parseFromFile program file
          case res of
            Left err           -> do
              outputStrLn $ show err
              loop state
            Right (Left err)   -> do
              outputStrLn $ parse_error_file err file
              loop state
            Right (Right prog) -> do
              let ((env, cs, scs, tvs), (envV, mem)) = state
              case type_of_program tvs env prog of
                Left err                        -> do
                  outputStrLn $ typing_error_file err file
                  loop state
                Right ((tvs', cs', scs'), env') -> case unify (cs' ++ cs) (scs' ++ scs) (env' ++ env) of -- czy na pewno ostatni ++
                  Left err                      -> do
                    outputStrLn $ typing_error_file err file
                    loop state
                  Right (scs'', env'')          -> case check_types_simple scs'' env'' of
                    Left err                    -> do
                      outputStrLn $ typing_error_file err file
                      loop state
                    Right env'''                -> case eval_program envV mem prog of
                      Left err                  -> do
                        outputStrLn $ eval_error_file err file
                        loop state
                      Right (vs, envV', mem')   -> do
                        show_values vs
                        loop ((env' ++ env, cs' ++ cs, scs' ++ scs, tvs'), (envV', mem'))
        Just (':':'c':' ':instr) -> do
          case expressionParser instr of
            Left err -> do
              outputStrLn $ parse_error err instr
            Right ex  -> do
              let ((env, cs, scs, tvs), _) = state
              case type_of_expression tvs env ex of
                Left err                      -> outputStrLn $ typing_error err ex
                Right ((tvs', cs', scs'), te) -> do
                  show_values cs'
                  loop state
        Just (':':'t':' ':instr) -> do
          case expressionParser instr of
            Left err -> do
              outputStrLn $ parse_error err instr
            Right ex  -> do
              let ((env, cs, scs, tvs), _) = state
              case type_of_expression tvs env ex of
                Left err                      -> outputStrLn $ typing_error err ex
                Right ((tvs', cs', scs'), te) -> case unify (cs' ++ cs) (scs' ++ scs) (("it", te):env) of
                  Left err                    -> outputStrLn $ typing_error err ex
                  Right (scs'', env')         -> case check_types_simple scs'' env' of
                    Left err                  -> outputStrLn $ typing_error err ex
                    Right env                 -> outputStrLn $ show_type ex (fromJust ("it" `lookup` env))
          loop state
        Just instruction         -> do
          case inputParser instruction of
            Left err -> do
              outputStrLn $ parse_error err instruction
              loop state
            Right i  -> do
              let ((env, cs, scs, tvs), (envV, mem)) = state
              case type_of_instruction tvs env i of
                Left err                        -> do
                  outputStrLn $ typing_error err i
                  loop state
                Right ((tvs', cs', scs'), env') -> case unify (cs' ++ cs) (scs' ++ scs) (env' ++ env) of
                  Left err                      -> do
                    outputStrLn $ typing_error err i
                    loop state
                  Right (scs'', env'')          -> case check_types_simple scs'' env'' of
                    Left err                    -> do
                      outputStrLn $ typing_error err i
                      loop state
                    Right env'''                -> case eval_instruction envV mem i of
                      Left err                  -> do
                        outputStrLn $ eval_error err i
                        loop state
                      Right (vs, envV', mem')   -> do
                        show_values vs
                        loop ((env' ++ env, cs' ++ cs, scs' ++ scs, tvs'), (envV' ++ envV, mem'))
