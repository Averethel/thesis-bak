{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Passes.EnforceUniqueNames (enforceUniqueNames) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  type Counter = ValueName -> Integer

  emptyCounter :: Counter
  emptyCounter _ = 0

  emptyMax :: Counter
  emptyMax _ = 1

  incBy :: Integer -> Counter -> ValueName -> Counter
  incBy i c m = \x -> if x == m then c x + i else c x

  inc :: Counter -> ValueName -> Counter
  inc = incBy 1

  set :: Counter -> ValueName -> Integer -> Counter
  set c m v = \x -> if x == m then v else c x

  data EnforcerState = 
    S {
        counter :: Counter,
        history :: [Counter],
        maxs    :: Counter
      }

  emptyState :: EnforcerState
  emptyState = S { counter = emptyCounter, history = [], maxs = emptyMax }

  backup :: (MonadState EnforcerState m) => m ()
  backup = do
    s <- get
    put $ s { history = counter s : history s }

  restore :: (MonadState EnforcerState m) => m ()
  restore = do
    s <- get
    let (c:cs) = history s
    put $ s { counter = c, history = cs }

  increment :: (MonadState EnforcerState m) => ValueName -> m ()
  increment v = do
    s <- get
    let c = counter s
    let m = maxs s
    put $ s { counter = set c v $ m v, maxs = m `inc` v }

  value :: (MonadState EnforcerState m) => ValueName -> m ValueName
  value v = do
    s <- get
    return $ ('_':v) ++ show (counter s $ v)

  renameToUniquePatternList :: (MonadState EnforcerState m) => [Pattern] -> m [Pattern]
  renameToUniquePatternList []     = return []
  renameToUniquePatternList (p:ps) = do
    p'  <- renameToUniquePattern p
    ps' <- renameToUniquePatternList ps
    return $ p':ps'

  renameToUniquePattern :: (MonadState EnforcerState m) => Pattern -> m Pattern
  renameToUniquePattern (P_Val vn) = do
    increment vn
    vn' <- value vn
    return $ P_Val vn'
  renameToUniquePattern (P_Tuple ps) = do
    ps' <- renameToUniquePatternList ps
    return $ P_Tuple ps'
  renameToUniquePattern (P_Cons p1 p2) = do
    p1' <- renameToUniquePattern p1
    p2' <- renameToUniquePattern p2
    return $ P_Cons p1' p2'
  renameToUniquePattern p = return p

  renameToUniqueFunction :: (MonadState EnforcerState m) => [FunBinding] -> m [FunBinding]
  renameToUniqueFunction []          = return []
  renameToUniqueFunction ((p, e, g):bs) = do
    backup
    p'  <- renameToUniquePattern p
    e'  <- renameToUniqueExpression e
    g'  <- renameToUniqueExpression g
    restore
    bs' <- renameToUniqueFunction bs
    return $ (p', e', g'):bs'

  renameToUniqueLetBindings :: (MonadState EnforcerState m) => [Binding] -> m [Binding]
  renameToUniqueLetBindings []          = return []
  renameToUniqueLetBindings ((p, e):bs) = do
    p'  <- renameToUniquePattern p
    e'  <- renameToUniqueExpression e
    bs' <- renameToUniqueLetBindings bs
    return $ (p', e'):bs'  

  renameToUniqueBindings :: (MonadState EnforcerState m) => [LetRecBinding] -> m [LetRecBinding]
  renameToUniqueBindings []            = return []
  renameToUniqueBindings ((v, e):lrbs) = do
    increment v
    v'    <- value v
    bs'   <- renameToUniqueExpression e
    lrbs' <- renameToUniqueBindings lrbs
    return $ (v',bs'):lrbs'

  renameToUniqueExpressionList :: (MonadState EnforcerState m) => [Expr] -> m [Expr]
  renameToUniqueExpressionList []     = return []
  renameToUniqueExpressionList (e:es) = do
    e'  <- renameToUniqueExpression e
    es' <- renameToUniqueExpressionList es
    return $ e':es' 

  renameToUniqueExpression :: (MonadState EnforcerState m) => Expr -> m Expr
  renameToUniqueExpression (E_Val v) = do
    v' <- value v
    return $ E_Val v'
  renameToUniqueExpression (E_Apply e1 e2) = do
    e1' <- renameToUniqueExpression e1
    e2' <- renameToUniqueExpression e2
    return $ E_Apply e1' e2'
  renameToUniqueExpression (E_Cons e1 e2) = do
    e1' <- renameToUniqueExpression e1
    e2' <- renameToUniqueExpression e2
    return $ E_Cons e1' e2'
  renameToUniqueExpression (E_Tuple es) = do
    es' <- renameToUniqueExpressionList es
    return $ E_Tuple es'
  renameToUniqueExpression (E_And e1 e2) = do
    e1' <- renameToUniqueExpression e1
    e2' <- renameToUniqueExpression e2
    return $ E_And e1' e2'
  renameToUniqueExpression (E_Or e1 e2) = do
    e1' <- renameToUniqueExpression e1
    e2' <- renameToUniqueExpression e2
    return $ E_And e1' e2'
  renameToUniqueExpression (E_ITE e1 e2 e3) = do
    e1' <- renameToUniqueExpression e1
    e2' <- renameToUniqueExpression e2
    e3' <- renameToUniqueExpression e3
    return $ E_ITE e1' e2' e3'
  renameToUniqueExpression (E_Seq e1 e2) = do
    e1' <- renameToUniqueExpression e1
    e2' <- renameToUniqueExpression e2
    return $ E_Seq e1' e2'
  renameToUniqueExpression (E_Function bs) = do
    bs' <- renameToUniqueFunction bs
    return $ E_Function bs'
  renameToUniqueExpression (E_Let bs e2) = do
    bs' <- renameToUniqueLetBindings bs
    backup
    e2' <- renameToUniqueExpression e2
    restore
    return $ E_Let bs' e2'
  renameToUniqueExpression (E_LetRec lrbs e) = do
    backup
    lrbs' <- renameToUniqueBindings lrbs
    e'    <- renameToUniqueExpression e
    restore
    return $ E_LetRec lrbs' e'
  renameToUniqueExpression e = return e

  renameToUniqueDefinition :: (MonadState EnforcerState m) => Definition -> m Definition
  renameToUniqueDefinition (D_Let bs)      = do
    bs' <- renameToUniqueLetBindings bs
    return $ D_Let bs'
  renameToUniqueDefinition (D_LetRec lrbs) = do
    lrbs' <- renameToUniqueBindings lrbs
    return $ D_LetRec lrbs'

  renameToUniqueInstruction :: (MonadState EnforcerState m) => Instruction -> m Instruction
  renameToUniqueInstruction (IDF df) = do
    df' <- renameToUniqueDefinition df
    return $ IDF df'
  renameToUniqueInstruction (IEX ex) = do
    ex' <- renameToUniqueExpression ex
    return $ IEX ex'

  renameToUnique :: (MonadState EnforcerState m) => Program -> m Program
  renameToUnique []     = return []
  renameToUnique (i:is) = do
    i'  <- renameToUniqueInstruction i
    is' <- renameToUnique is
    return $ i':is'

  enforceUniqueNames :: Program -> Program
  enforceUniqueNames prog = fst $ runState (renameToUnique prog) Compiler.Passes.EnforceUniqueNames.emptyState
