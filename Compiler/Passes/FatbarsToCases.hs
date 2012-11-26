{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Passes.FatbarsToCases (fatbarsToCases) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  renameBinding :: ValueName -> ValueName -> Binding -> Binding
  renameBinding s t (p, e) = 
    (p, rename s t e)

  renameFunBinding :: ValueName -> ValueName -> FunBinding -> FunBinding
  renameFunBinding s t (p, e, g) =
    (p, rename s t e, rename s t g)

  renameLetRecBinding :: ValueName -> ValueName -> LetRecBinding -> LetRecBinding
  renameLetRecBinding s t (v, e) =
    (v, rename s t e)

  rename :: ValueName -> ValueName -> Expr -> Expr
  rename s t (E_Val vn)      
    | vn == s                  =
      E_Val t
    | otherwise                =
      E_Val vn
  rename s t (E_Apply e1 e2)   =
    E_Apply (rename s t e1) (rename s t e2)
  rename s t (E_Cons e1 e2)    =
    E_Cons (rename s t e1) (rename s t e2)
  rename s t (E_Tuple es)      =
    E_Tuple $ map (rename s t) es
  rename s t (E_And e1 e2)     =
    E_And (rename s t e1) (rename s t e2)
  rename s t (E_Or e1 e2)      =
    E_Or (rename s t e1) (rename s t e2)
  rename s t (E_ITE e1 e2 e3)  =
    E_ITE (rename s t e1) (rename s t e2) (rename s t e3)
  rename s t (E_Case e bs)     =
    E_Case (rename s t e) $ map (renameBinding s t) bs
  rename s t (E_Seq e1 e2)     =
    E_Seq (rename s t e1) (rename s t e2)
  rename s t (E_Function fbs)  =
    E_Function $ map (renameFunBinding s t) fbs
  rename s t (E_Let bs e)      =
    E_Let (map (renameBinding s t) bs) (rename s t e)
  rename s t (E_LetRec lrbs e) =
    E_LetRec (map (renameLetRecBinding s t) lrbs) (rename s t e)
  rename s t (E_FatBar e1 e2)  =
    E_FatBar (rename s t e1) (rename s t e2)
  rename _ _ e                 =
    e

  type Equation = ([Pattern], Expr)

  data Constructor = 
      Nil
    | Cons
    | Pair
    | CTrue
    | CFalse
    | CUnit
    deriving Eq

  arity :: Constructor -> Integer
  arity Cons = 2
  arity Pair = 2
  arity _    = 0

  constructors :: Constructor -> [Constructor]
  constructors Nil    = [Nil, Cons]
  constructors Cons   = [Nil, Cons]
  constructors Pair   = [Pair]
  constructors CTrue  = [CTrue, CFalse]
  constructors CFalse = [CTrue, CFalse]
  constructors CUnit  = [CUnit]

  isVar :: Equation -> Bool
  isVar ((P_Val _):_, _) = True
  isVar _                = False

  isCon :: Equation -> Bool
  isCon = not . isVar

  getCon :: Equation -> Constructor
  getCon ((P_Const C_Nil):_, _)   = Nil
  getCon ((P_Cons _ _):_, _)      = Cons
  getCon ((P_Tuple _):_, _)       = Pair
  getCon ((P_Const C_True):_, _)  = CTrue
  getCon ((P_Const C_False):_, _) = CFalse
  getCon ((P_Const C_Unit):_, _)  = CUnit

  toPattern :: Constructor -> [ValueName] -> Pattern
  toPattern Nil    []      = P_Const C_Nil
  toPattern Cons   [x, xs] = P_Cons (P_Val x) $ P_Val xs
  toPattern Pair   [a, b]  = P_Tuple [P_Val a, P_Val b]
  toPattern CTrue  []      = P_Const C_True
  toPattern CFalse []      = P_Const C_False
  toPattern CUnit  []      = P_Const C_Unit

  subpaterns :: Pattern -> [Pattern]
  subpaterns (P_Const C_Nil)   = []
  subpaterns (P_Cons p1 p2)    = [p1, p2]
  subpaterns (P_Tuple ps)      = ps
  subpaterns (P_Const C_True)  = []
  subpaterns (P_Const C_False) = []
  subpaterns (P_Const C_Unit)  = []

  type NamerState = Integer

  emptyState :: NamerState
  emptyState = 0

  newVar :: MonadState NamerState m => m ValueName
  newVar = do
    s <- get
    put $ s + 1
    return $ "U_" ++ show s

  partition :: (a -> Bool) -> [a] -> [[a]]
  partition f []       = []
  partition f [x]      = [[x]]
  partition f (x:y:xs)
    | f x == f y       = 
      tack x $ partition f (y:xs)
    | otherwise        =
      [x] : partition f (y:xs)

  tack :: a -> [[a]] -> [[a]]
  tack x xss = (x : head xss) : tail xss

  matchVar :: MonadState NamerState m => [ValueName] -> [([Pattern], Expr)] -> Expr -> m Expr
  matchVar (u:us) qs def = 
    match us [(ps, rename v u e) | ((P_Val v) : ps, e) <- qs] def

  choose :: Constructor -> [Equation] -> [Equation]
  choose c qs = [q | q <- qs, getCon q == c]

  matchClause :: MonadState NamerState m => Constructor -> [ValueName] -> [Equation] -> Expr -> m Binding
  matchClause c (u:us) qs def = do
    let k  = arity c
    us' <- mapM (\_ -> newVar) [1..k]
    e'  <- match (us' ++ us) [(subpaterns p ++ ps, e) | (p : ps, e) <- qs] def
    return (toPattern c us', e')

  matchCon :: MonadState NamerState m => [ValueName] -> [Equation] -> Expr -> m Expr
  matchCon (u:us) qs def = do
    let cs = constructors $ getCon $ head qs
    ms' <- mapM (\c -> matchClause c (u:us) (choose c qs) def) cs
    return $ E_Case (E_Val u) ms'
        
  matchVarCon :: MonadState NamerState m => [ValueName] -> [Equation] -> Expr -> m Expr
  matchVarCon us qs def
    | isVar . head $ qs =
      matchVar us qs def
    | isCon . head $ qs = 
      matchCon us qs def

  foldrM :: Monad m => (b -> a -> m a) -> a -> [b] -> m a
  foldrM f a []     = return a
  foldrM f a (x:xs) = do
    acc' <- foldrM f a xs
    f x acc'

  match :: MonadState NamerState m => [ValueName] -> [Equation] -> Expr -> m Expr
  match []     qs def = 
    return $ foldr E_FatBar def [e | ([], e) <- qs ]
  match (u:us) qs def =
    foldrM (matchVarCon (u:us)) def $ partition isVar qs

  decompose :: Expr -> [Equation]
  decompose E_MatchFailure                                      =
    []
  decompose (E_FatBar (E_Apply (E_Function [(p, e, E_Const C_True)]) (E_Val _)) e2) =
    ([p], e) : decompose e2
  decompose (E_FatBar (E_Apply (E_Function [(p, e, g)]) (E_Val _)) e2)              =
    ([p], E_ITE g e E_MatchFailure) : decompose e2

  fatbarsToCasesBinding :: MonadState NamerState m => Binding -> m Binding
  fatbarsToCasesBinding (p, e) = do
    e' <- fatbarsToCasesExpr e
    return (p, e')

  fatbarsToCasesLetrecBinding :: MonadState NamerState m => LetRecBinding -> m LetRecBinding
  fatbarsToCasesLetrecBinding (v, e) = do
    e' <- fatbarsToCasesExpr e
    return (v, e')

  fatbarsToCasesExpr :: MonadState NamerState m => Expr -> m Expr
  fatbarsToCasesExpr (E_Apply e1 e2)                  = do
    e1' <- fatbarsToCasesExpr e1
    e2' <- fatbarsToCasesExpr e2
    return $ E_Apply e1' e2'
  fatbarsToCasesExpr (E_Cons e1 e2)                   = do
    e1' <- fatbarsToCasesExpr e1
    e2' <- fatbarsToCasesExpr e2
    return $ E_Cons e1' e2'
  fatbarsToCasesExpr (E_Tuple es)                     = do
    es' <- mapM fatbarsToCasesExpr es
    return $ E_Tuple es'
  fatbarsToCasesExpr (E_And e1 e2)                    = do
    e1' <- fatbarsToCasesExpr e1
    e2' <- fatbarsToCasesExpr e2
    return $ E_And e1' e2'
  fatbarsToCasesExpr (E_Or e1 e2)                     = do
    e1' <- fatbarsToCasesExpr e1
    e2' <- fatbarsToCasesExpr e2
    return $ E_Or e1' e2'
  fatbarsToCasesExpr (E_ITE e1 e2 e3)                 = do
    e1' <- fatbarsToCasesExpr e1
    e2' <- fatbarsToCasesExpr e2
    e3' <- fatbarsToCasesExpr e3
    return $ E_ITE e1' e2' e3'
  fatbarsToCasesExpr (E_Seq e1 e2)                    = do
    e1' <- fatbarsToCasesExpr e1
    e2' <- fatbarsToCasesExpr e2
    return $ E_Seq e1' e2'
  fatbarsToCasesExpr (E_Function
    [(P_Val v, e@(E_FatBar e1 e2), g@(E_Const C_True))]) = do
    cs <- match [v] (decompose e) E_MatchFailure
    return $ E_Function [(P_Val v, cs, g)]
  fatbarsToCasesExpr (E_Let bs e)                     = do
    bs' <- mapM fatbarsToCasesBinding bs
    e'  <- fatbarsToCasesExpr e
    return $ E_Let bs' e'
  fatbarsToCasesExpr (E_LetRec lrbs e)                = do
    lrbs' <- mapM fatbarsToCasesLetrecBinding lrbs
    e'    <- fatbarsToCasesExpr e
    return $ E_LetRec lrbs' e'
  fatbarsToCasesExpr e                                =
    return e

  fatbarsToCasesDefinition :: MonadState NamerState m => Definition -> m Definition
  fatbarsToCasesDefinition (D_Let bs)      = do
    bs' <- mapM fatbarsToCasesBinding bs
    return $ D_Let bs'
  fatbarsToCasesDefinition (D_LetRec lrbs) = do
    lrbs' <- mapM fatbarsToCasesLetrecBinding lrbs
    return $ D_LetRec lrbs'

  fatbarsToCasesInstruction :: MonadState NamerState m => Instruction -> m Instruction
  fatbarsToCasesInstruction (IDF df) = do
    df' <- fatbarsToCasesDefinition df
    return $ IDF df'
  fatbarsToCasesInstruction (IEX ex) = do
    ex' <- fatbarsToCasesExpr ex
    return $ IEX ex'

  fatbarsToCases :: Program -> Program
  fatbarsToCases p = 
    fst $ runState (mapM fatbarsToCasesInstruction p) emptyState
