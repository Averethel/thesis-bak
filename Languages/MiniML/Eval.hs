module Languages.MiniML.Eval (eval_program, eval_instruction, matches) where
  import Languages.MiniML.Errors
  import Languages.MiniML.Syntax
  import Languages.MiniML.Typing

  import Data.Maybe

  is_value :: Expr -> Bool
  is_value (E_UPrim _)             = True
  is_value (E_BPrim _)             = True
  -- is_value (E_Val _)               = True
  is_value (E_Location _)          = True
  is_value (E_Const _)             = True
  is_value (E_Function _)          = True
  is_value (E_Apply (E_BPrim _) e) = is_value e
  is_value (E_Cons e1 e2)          = is_value e1 && is_value e2
  is_value (E_Tuple es)            = all is_value es
  is_value _                       = False

  matches :: Expr -> Pattern -> Bool
  e              `matches` (P_Val x)      = True
  e              `matches` P_Wildcard     = True
  (E_Const c1)   `matches` (P_Const c2)   = c1 == c2
  (E_Tuple es)   `matches` (P_Tuple ps)
    | length es == length ps              = and $ zipWith matches es ps
  (E_Cons e1 e2) `matches` (P_Cons p1 p2) = e1 `matches` p1 && e2 `matches` p2
  _              `matches` _              = False

  gen_subst :: Expr -> Pattern -> EvalEnv
  gen_subst e              (P_Val x)      = [(x, e)]
  gen_subst e              P_Wildcard     = []
  gen_subst (E_Const _)    (P_Const _)    = []
  gen_subst (E_Tuple es)   (P_Tuple ps)   = concat $ zipWith gen_subst es ps
  gen_subst (E_Cons e1 e2) (P_Cons p1 p2) = gen_subst e1 p1 ++ gen_subst e2 p2

  match_clause :: EvalEnv -> Mem -> [(Pattern, Expr)] -> Expr -> Either String (Expr, EvalEnv, Mem)
  match_clause _   _   []          _  = Left $ match_failure
  match_clause env mem ((p, e):ms) ex
    | ex `matches` p                  = Right (e, gen_subst ex p ++ env, mem)
    | otherwise                       = match_clause env mem ms ex

  gen_location :: Mem -> Integer
  gen_location = toInteger.length

  get_reference :: Mem -> Expr -> Expr
  get_reference (m:ms) (E_Location 0) = m
  get_reference (m:ms) (E_Location n) = get_reference ms $ E_Location $ n - 1

  store_reference :: Mem -> Expr -> Expr -> Mem
  store_reference []     (E_Location 0) v = [v]
  store_reference (m:ms) (E_Location 0) v = v:ms
  store_reference (m:ms) (E_Location n) v = m:store_reference ms (E_Location $ n-1) v

  get_value :: EvalEnv -> Expr -> Expr
  get_value e (E_Val n) = fromJust $ n `lookup` e

  eval_unary_primitive :: EvalEnv -> Mem -> UnaryPrim -> Expr -> (Expr, EvalEnv, Mem)
  eval_unary_primitive env mem (U_Not)     (E_Const C_True)    = (E_Const C_False, env, mem)
  eval_unary_primitive env mem (U_Not)     (E_Const C_False)   = (E_Const C_True, env, mem)
  eval_unary_primitive env mem (U_I_Minus) (E_Const (C_Int n)) = (E_Const $ C_Int $ 0 - n, env, mem)
  eval_unary_primitive env mem (U_Ref)     e                   = (l, env, store_reference mem l e) where
    l = E_Location $ gen_location mem
  eval_unary_primitive env mem (U_Deref)   e@(E_Location l)    = (get_reference mem e, env, mem)

  eval_binary_primitive :: EvalEnv -> Mem -> Expr -> BinaryPrim -> Expr -> Either String (Expr, EvalEnv, Mem)
  eval_binary_primitive env mem (E_Const c1) B_Eq (E_Const c2)
    | c1 == c2                                                                      = Right (E_Const C_True, env, mem)
    | otherwise                                                                     = Right (E_Const C_False, env, mem)
  eval_binary_primitive env mem e1@(E_Location l1) B_Eq e2@(E_Location l2)          = Right (E_Apply (E_Apply (E_BPrim B_Eq) (E_Apply (E_UPrim U_Deref) e1)) (E_Apply (E_UPrim U_Deref) e2), env, mem)
  eval_binary_primitive env mem (E_Cons _ _) B_Eq (E_Const C_Nil)                   = Right (E_Const C_False, env, mem)
  eval_binary_primitive env mem (E_Const C_Nil) B_Eq (E_Cons _ _)                   = Right (E_Const C_False, env, mem)
  eval_binary_primitive env mem (E_Cons e1 e2) B_Eq (E_Cons e1' e2')                = Right (E_And (E_Apply (E_Apply (E_BPrim B_Eq) e1) e1') (E_Apply (E_Apply (E_BPrim B_Eq) e2) e2'), env, mem)
  eval_binary_primitive env mem (E_Tuple es1) B_Eq (E_Tuple es2)
    | length es1 == length es2                                                      = Right (foldl E_And (E_Const C_True) $ zipWith (\e1 e2 -> E_Apply (E_Apply (E_BPrim B_Eq) e1) e2) es1 es2, env, mem)
    | otherwise                                                                     = Right (E_Const C_False, env, mem)
  eval_binary_primitive env mem (E_Const (C_Int n1)) B_I_Plus (E_Const (C_Int n2))  = Right (E_Const $ C_Int $ n1 + n2, env, mem)
  eval_binary_primitive env mem (E_Const (C_Int n1)) B_I_Minus (E_Const (C_Int n2)) = Right (E_Const $ C_Int $ n1 - n2, env, mem)
  eval_binary_primitive env mem (E_Const (C_Int n1)) B_I_Mult (E_Const (C_Int n2))  = Right (E_Const $ C_Int $ n1 * n2, env, mem)
  eval_binary_primitive env mem (E_Const (C_Int n1)) B_I_Div (E_Const (C_Int n2))
    | n2 /= 0                                                                       = Right (E_Const $ C_Int $ n1 `div` n2, env, mem)
    | otherwise                                                                     = Left $ division_by_0
  eval_binary_primitive env mem e1@(E_Location l) B_Assign e2                       = Right (E_Const C_Unit, env, store_reference mem e1 e2)

  recfun :: [(ValueName, [(Pattern, Expr)])] -> [(Pattern, Expr)] -> Expr
  recfun lrbs pm = apply_substs subst $ apply_substs renaming_subst (E_Function pm) where
    pre_subst      = recfun' lrbs
    renaming_subst = map (\(a, _) -> (a, E_Val $ "'" ++ a)) pre_subst
    subst          = map (\(a, b) -> ("'" ++ a, b)) pre_subst
    recfun' :: [(ValueName, [(Pattern, Expr)])] -> EvalEnv
    recfun' []           = []
    recfun' ((vn, _):bs) = (vn, E_LetRec lrbs $ E_Val vn) : recfun' bs
    apply_subst :: ValueName -> Expr -> Expr -> Expr
    apply_subst vn e1 e2@(E_UPrim _)    = e2
    apply_subst vn e1 e2@(E_BPrim _)    = e2
    apply_subst vn e1 e2@(E_Val vn')
      | vn == vn'                       = e1
      | otherwise                       = e2
    apply_subst vn e1 e2@(E_Location _) = e2
    apply_subst vn e1 e2@(E_Const _)    = e2
    apply_subst vn e1 (E_Apply e e')    = E_Apply (apply_subst vn e1 e) (apply_subst vn e1 e')
    apply_subst vn e1 (E_Cons e e')     = E_Cons (apply_subst vn e1 e) (apply_subst vn e1 e')
    apply_subst vn e1 (E_Tuple es)      = E_Tuple $ map (apply_subst vn e1) es
    apply_subst vn e1 (E_And e e')      = E_And (apply_subst vn e1 e) (apply_subst vn e1 e')
    apply_subst vn e1 (E_Or e e')       = E_Or (apply_subst vn e1 e) (apply_subst vn e1 e')
    apply_subst vn e1 (E_ITE e e' e'')  = E_ITE (apply_subst vn e1 e) (apply_subst vn e1 e') (apply_subst vn e1 e'')
    apply_subst vn e1 (E_Seq e e')      = E_Seq (apply_subst vn e1 e) (apply_subst vn e1 e')
    apply_subst vn e1 (E_Function ps)   = E_Function $ map (\(p, e) -> if   p `binds` vn
                                                                       then (p, e)
                                                                       else (p, apply_subst vn e1 e)) ps
    apply_subst vn e1 (E_Let (p, e) e')
      | p `binds` vn                    = E_Let (p, apply_subst vn e1 e) e'
      | otherwise                       = E_Let (p, apply_subst vn e1 e) $ apply_subst vn e1 e'
    apply_subst vn e1 e2@(E_LetRec lrbs e)
      | vn `elem` (map fst lrbs)        = e2
      | otherwise                       = E_LetRec (map (\(v, bs) -> (v, map (\(p, e) -> if   p `binds` vn
                                                                                        then (p, e)
                                                                                        else (p, apply_subst vn e1 e)) bs)) lrbs) $ apply_subst vn e1 e
    apply_substs :: EvalEnv -> Expr -> Expr
    apply_substs []          e2 = e2
    apply_substs ((v,e1):ss) e2 = apply_substs ss $ apply_subst v e1 e2
    binds :: Pattern -> ValueName -> Bool
    (P_Val v)      `binds` vn = v == vn
    (P_Tuple ps)   `binds` vn = any (`binds` vn) ps
    (P_Cons p1 p2) `binds` vn = p1 `binds` vn || p2 `binds` vn
    _              `binds` _  = False

  eval_step_expr :: EvalEnv -> Mem -> Expr -> Either String (Expr, EvalEnv, Mem)
  eval_step_expr env mem e@(E_Val vn) = Right (get_value env e, env, mem)
  eval_step_expr env mem e@(E_Location l) = Right (get_reference mem e, env, mem)
  eval_step_expr env mem (E_Apply e1 e2)
    | not (is_value e1) = case eval_step_expr env mem e1 of
      Left err                -> Left err
      Right (e1', env', mem') -> Right (E_Apply e1' e2, env', mem')
    | is_value e1 && not (is_value e2) = case eval_step_expr env mem e2 of
      Left err                -> Left err
      Right (e2', env', mem') -> Right (E_Apply e1 e2', env', mem')
  eval_step_expr env mem (E_Apply (E_UPrim up) e1) = Right $ eval_unary_primitive env mem up e1
  eval_step_expr env mem (E_Apply (E_Apply (E_BPrim bp) e1) e2) = eval_binary_primitive env mem e1 bp e2
  eval_step_expr env mem (E_Apply (E_Function pm) e2) = match_clause env mem pm e2
  eval_step_expr env mem (E_Cons e1 e2)
    | not (is_value e1) = case eval_step_expr env mem e1 of
      Left err                -> Left err
      Right (e1', env', mem') -> Right (E_Cons e1' e2, env', mem')
    | is_value e1 && not (is_value e2) = case eval_step_expr env mem e2 of
      Left err                -> Left err
      Right (e2', env', mem') -> Right (E_Cons e1 e2', env', mem')
  eval_step_expr env mem (E_Tuple es) = eval_step_tuple env mem es [] where
    eval_step_tuple env mem []     acc = Right (E_Tuple acc, env, mem)
    eval_step_tuple env mem (e:es) acc
      | is_value e = eval_step_tuple env mem es (e:acc)
      | otherwise  = case eval_step_expr env mem e of
        Left err               -> Left err
        Right (e', env', mem') -> Right (E_Tuple $ (reverse acc) ++ (e':es), env', mem')
  eval_step_expr env mem (E_And e1 e2) = Right (E_ITE e1 e2 (E_Const C_False), env, mem)
  eval_step_expr env mem (E_Or e1 e2) = Right (E_ITE e1 (E_Const C_True) e2, env, mem)
  eval_step_expr env mem (E_ITE (E_Const C_True) e2 e3) = Right (e2, env, mem)
  eval_step_expr env mem (E_ITE (E_Const C_False) e2 e3) = Right (e3, env, mem)
  eval_step_expr env mem (E_ITE e1 e2 e3)
    | not (is_value e1) = case eval_step_expr env mem e1 of
      Left err               -> Left err
      Right (e', env', mem') -> Right (E_ITE e' e2 e3, env', mem')
  eval_step_expr env mem (E_Seq e1 e2)
    | not (is_value e1) = case eval_step_expr env mem e1 of
      Left err                -> Left err
      Right (e1', env', mem') -> Right (E_Seq e1' e2, env', mem')
    | is_value e1        = Right (e2, env, mem)
  eval_step_expr env mem (E_Let (p, e1) e2)
    | is_value e1 = Right (e2, gen_subst e1 p ++ env, mem)
    | otherwise   = case eval_step_expr env mem e1 of
      Left err                -> Left err
      Right (e1', env', mem') -> Right (E_Let (p, e1') e2, env', mem')
  eval_step_expr env mem (E_LetRec lrbs es) = Right (es, gen_env lrbs ++ env, mem) where
    gen_env []            = []
    gen_env ((vn, pm):bs) = (vn, recfun lrbs pm) : gen_env bs

  eval_expr :: EvalEnv -> Mem -> Expr -> Either String (Expr, Mem)
  eval_expr env mem e
    | is_value e = Right (e, mem)
    | otherwise  = case eval_step_expr env mem e of
      Left err               -> Left err
      Right (e', env', mem') -> eval_expr env' mem' e'

  eval_definition :: EvalEnv -> Mem -> Definition -> Either String (EvalEnv, Mem)
  eval_definition env mem (D_Let (p, e))
    | is_value e && e `matches` p       = Right (gen_subst e p, mem)
    | is_value e && not (e `matches` p) = Left match_failure
    | not (is_value e)                  =
      case eval_expr env mem e of
        Left err                       -> Left err
        Right (e', mem')               -> eval_definition env mem' (D_Let (p, e'))
  eval_definition env mem (D_LetRec lrbs)   = Right (gen_env lrbs, mem) where
    gen_env []                          = []
    gen_env ((vn, pm):bs)               = (vn, recfun lrbs pm) : gen_env bs

  eval_instruction :: EvalEnv -> Mem -> Instruction -> Either String ([Expr], EvalEnv, Mem)
  eval_instruction env mem (IDF df) =
    case eval_definition env mem df of
      Left err                 -> Left err
      Right (env', mem')       -> Right (map (\(vn, _) -> E_Val vn) env', env' ++ env, mem')
  eval_instruction env mem (IEX ex) =
    case eval_expr env mem ex of
      Left err                 -> Left err
      Right (ex', mem')        -> Right ([ex'], ("it", ex'):env, mem')

  eval_program :: EvalEnv -> Mem -> Program -> Either String ([Expr], EvalEnv, Mem)
  eval_program env mem = eval_program' env mem [] where
    eval_program' env mem exprs []     = Right (exprs, env, mem)
    eval_program' env mem exprs (i:is) =
      case eval_instruction env mem i of
        Left err                   -> Left err
        Right (exprs', env', mem') -> eval_program' env' mem' exprs' is
