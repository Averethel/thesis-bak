{-# LANGUAGE
  FlexibleContexts
  #-}

module Compiler.Passes.FatbarsToCases (fatbars_to_cases) where
  import Languages.MiniML.Syntax

  import Control.Monad.State

  rename_binding :: ValueName -> ValueName -> Binding -> Binding
  rename_binding s t (p, e) = 
    (p, rename s t e)

  rename_fun_binding :: ValueName -> ValueName -> FunBinding -> FunBinding
  rename_fun_binding s t (p, e, g) =
    (p, rename s t e, rename s t g)

  rename_let_rec_binding :: ValueName -> ValueName -> LetRecBinding -> LetRecBinding
  rename_let_rec_binding s t (v, e) =
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
    E_Case (rename s t e) $ map (rename_binding s t) bs
  rename s t (E_Seq e1 e2)     =
    E_Seq (rename s t e1) (rename s t e2)
  rename s t (E_Function fbs)  =
    E_Function $ map (rename_fun_binding s t) fbs
  rename s t (E_Let bs e)      =
    E_Let (map (rename_binding s t) bs) (rename s t e)
  rename s t (E_LetRec lrbs e) =
    E_LetRec (map (rename_let_rec_binding s t) lrbs) (rename s t e)
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

  is_var :: Equation -> Bool
  is_var ((P_Val _):_, _) = True
  is_var _                = False

  is_con :: Equation -> Bool
  is_con = not . is_var

  get_con :: Equation -> Constructor
  get_con ((P_Const C_Nil):_, _)   = Nil
  get_con ((P_Cons _ _):_, _)      = Cons
  get_con ((P_Tuple _):_, _)       = Pair
  get_con ((P_Const C_True):_, _)  = CTrue
  get_con ((P_Const C_False):_, _) = CFalse
  get_con ((P_Const C_Unit):_, _)  = CUnit

  to_pattern :: Constructor -> [ValueName] -> Pattern
  to_pattern Nil    []      = P_Const C_Nil
  to_pattern Cons   [x, xs] = P_Cons (P_Val x) $ P_Val xs
  to_pattern Pair   [a, b]  = P_Tuple [P_Val a, P_Val b]
  to_pattern CTrue  []      = P_Const C_True
  to_pattern CFalse []      = P_Const C_False
  to_pattern CUnit  []      = P_Const C_Unit

  subpaterns :: Pattern -> [Pattern]
  subpaterns (P_Const C_Nil)   = []
  subpaterns (P_Cons p1 p2)    = [p1, p2]
  subpaterns (P_Tuple ps)      = ps
  subpaterns (P_Const C_True)  = []
  subpaterns (P_Const C_False) = []
  subpaterns (P_Const C_Unit)  = []

  type NamerState = Integer

  empty_state :: NamerState
  empty_state = 0

  new_var :: MonadState NamerState m => m ValueName
  new_var = do
    s <- get
    put $ s + 1
    return $ "_u_" ++ show s

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

  match_var :: MonadState NamerState m => [ValueName] -> [([Pattern], Expr)] -> Expr -> m Expr
  match_var (u:us) qs def = 
    match us [(ps, rename v u e) | ((P_Val v) : ps, e) <- qs] def

  choose :: Constructor -> [Equation] -> [Equation]
  choose c qs = [q | q <- qs, get_con q == c]

  match_clause :: MonadState NamerState m => Constructor -> [ValueName] -> [Equation] -> Expr -> m Binding
  match_clause c (u:us) qs def = do
    let k  = arity c
    us' <- mapM (\_ -> new_var) [1..k]
    e'  <- match (us' ++ us) [(subpaterns p ++ ps, e) | (p : ps, e) <- qs] def
    return (to_pattern c us', e')

  match_con :: MonadState NamerState m => [ValueName] -> [Equation] -> Expr -> m Expr
  match_con (u:us) qs def = do
    let cs = constructors $ get_con $ head qs
    ms' <- mapM (\c -> match_clause c (u:us) (choose c qs) def) cs
    return $ E_Case (E_Val u) ms'
        
  match_var_con :: MonadState NamerState m => [ValueName] -> [Equation] -> Expr -> m Expr
  match_var_con us qs def
    | is_var . head $ qs =
      match_var us qs def
    | is_con . head $ qs = 
      match_con us qs def

  foldrM :: Monad m => (b -> a -> m a) -> a -> [b] -> m a
  foldrM f a []     = return a
  foldrM f a (x:xs) = do
    acc' <- foldrM f a xs
    f x acc'

  match :: MonadState NamerState m => [ValueName] -> [Equation] -> Expr -> m Expr
  match []     qs def = 
    return $ foldr E_FatBar def [e | ([], e) <- qs ]
  match (u:us) qs def =
    foldrM (match_var_con (u:us)) def $ partition is_var qs

  decompose :: Expr -> [Equation]
  decompose E_MatchFailure                                      =
    []
  decompose (E_FatBar (E_Apply (E_Function [(p, e, E_Const C_True)]) (E_Val _)) e2) =
    ([p], e) : decompose e2
  decompose (E_FatBar (E_Apply (E_Function [(p, e, g)]) (E_Val _)) e2)              =
    ([p], E_ITE g e E_MatchFailure) : decompose e2

  fatbars_to_cases_binding :: MonadState NamerState m => Binding -> m Binding
  fatbars_to_cases_binding (p, e) = do
    e' <- fatbars_to_cases_expr e
    return (p, e')

  fatbars_to_cases_letrec_binding :: MonadState NamerState m => LetRecBinding -> m LetRecBinding
  fatbars_to_cases_letrec_binding (v, e) = do
    e' <- fatbars_to_cases_expr e
    return (v, e')

  fatbars_to_cases_expr :: MonadState NamerState m => Expr -> m Expr
  fatbars_to_cases_expr (E_Apply e1 e2)                  = do
    e1' <- fatbars_to_cases_expr e1
    e2' <- fatbars_to_cases_expr e2
    return $ E_Apply e1' e2'
  fatbars_to_cases_expr (E_Cons e1 e2)                   = do
    e1' <- fatbars_to_cases_expr e1
    e2' <- fatbars_to_cases_expr e2
    return $ E_Cons e1' e2'
  fatbars_to_cases_expr (E_Tuple es)                     = do
    es' <- mapM fatbars_to_cases_expr es
    return $ E_Tuple es'
  fatbars_to_cases_expr (E_And e1 e2)                    = do
    e1' <- fatbars_to_cases_expr e1
    e2' <- fatbars_to_cases_expr e2
    return $ E_And e1' e2'
  fatbars_to_cases_expr (E_Or e1 e2)                     = do
    e1' <- fatbars_to_cases_expr e1
    e2' <- fatbars_to_cases_expr e2
    return $ E_Or e1' e2'
  fatbars_to_cases_expr (E_ITE e1 e2 e3)                 = do
    e1' <- fatbars_to_cases_expr e1
    e2' <- fatbars_to_cases_expr e2
    e3' <- fatbars_to_cases_expr e3
    return $ E_ITE e1' e2' e3'
  fatbars_to_cases_expr (E_Seq e1 e2)                    = do
    e1' <- fatbars_to_cases_expr e1
    e2' <- fatbars_to_cases_expr e2
    return $ E_Seq e1' e2'
  fatbars_to_cases_expr (E_Function
    [(P_Val v, e@(E_FatBar e1 e2), g@(E_Const C_True))]) = do
    cs <- match [v] (decompose e) E_MatchFailure
    return $ E_Function [(P_Val v, cs, g)]
  fatbars_to_cases_expr (E_Let bs e)                     = do
    bs' <- mapM fatbars_to_cases_binding bs
    e'  <- fatbars_to_cases_expr e
    return $ E_Let bs' e'
  fatbars_to_cases_expr (E_LetRec lrbs e)                = do
    lrbs' <- mapM fatbars_to_cases_letrec_binding lrbs
    e'    <- fatbars_to_cases_expr e
    return $ E_LetRec lrbs' e'
  fatbars_to_cases_expr e                                =
    return e

  fatbars_to_cases_definition :: MonadState NamerState m => Definition -> m Definition
  fatbars_to_cases_definition (D_Let bs)      = do
    bs' <- mapM fatbars_to_cases_binding bs
    return $ D_Let bs'
  fatbars_to_cases_definition (D_LetRec lrbs) = do
    lrbs' <- mapM fatbars_to_cases_letrec_binding lrbs
    return $ D_LetRec lrbs'

  fatbars_to_cases_instruction :: MonadState NamerState m => Instruction -> m Instruction
  fatbars_to_cases_instruction (IDF df) = do
    df' <- fatbars_to_cases_definition df
    return $ IDF df'
  fatbars_to_cases_instruction (IEX ex) = do
    ex' <- fatbars_to_cases_expr ex
    return $ IEX ex'

  fatbars_to_cases :: Program -> Program
  fatbars_to_cases p = 
    fst $ runState (mapM fatbars_to_cases_instruction p) empty_state
