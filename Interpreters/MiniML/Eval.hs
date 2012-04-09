module Interpreters.MiniML.Eval where
  import Interpreters.MiniML.Errors
  import Interpreters.MiniML.Syntax
  import Interpreters.MiniML.Typing

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

  match_clause :: EvalEnv -> [(Pattern, Expr)] -> Expr -> Either String (Expr, EvalEnv)
  match_clause _   []          _  = Left $ match_failure
  match_clause env ((p, e):ms) ex
    | ex `matches` p              = Right (e, gen_subst ex p ++ env)
    | otherwise                   = match_clause env ms ex

  is_location :: String -> Bool
  is_location ('0':_) = True
  is_location ('1':_) = True
  is_location ('2':_) = True
  is_location ('3':_) = True
  is_location ('4':_) = True
  is_location ('5':_) = True
  is_location ('6':_) = True
  is_location ('7':_) = True
  is_location ('8':_) = True
  is_location ('9':_) = True
  is_location _       = False

  count_locations :: EvalEnv -> Integer
  count_locations [] = 0
  count_locations ((v, _):es)
    | is_location v  = 1 + count_locations es
    | otherwise      = count_locations es

  gen_location :: EvalEnv -> String
  gen_location e = show $ (+) 1 $ count_locations e

  get_reference :: EvalEnv -> Expr -> Expr
  get_reference e (E_Location l) = fromJust $ l `lookup` e

  store_reference :: EvalEnv -> Expr -> Expr -> EvalEnv
  store_reference []             (E_Location l) v = [(l, v)]
  store_reference (e@(l',v'):es) (E_Location l) v
    | l == l'                                     = (l, v):es
    | otherwise                                   = e : store_reference es (E_Location l) v 

  get_value :: EvalEnv -> Expr -> Expr
  get_value e (E_Val n) = fromJust $ n `lookup` e

  eval_unary_primitive :: EvalEnv -> UnaryPrim -> Expr -> (Expr, EvalEnv)
  eval_unary_primitive env (U_Not)     (E_Const C_True)    = (E_Const C_False, env)
  eval_unary_primitive env (U_Not)     (E_Const C_False)   = (E_Const C_True, env)
  eval_unary_primitive env (U_I_Minus) (E_Const (C_Int n)) = (E_Const $ C_Int $ 0 - n, env)
  eval_unary_primitive env (U_Ref)     e                   = (l, store_reference env l e) where
    l = E_Location $ gen_location env
  eval_unary_primitive env (U_Deref)   e@(E_Location l)    = (get_reference env e, env)

  eval_binary_primitive :: EvalEnv -> Expr -> BinaryPrim -> Expr -> Either String (Expr, EvalEnv)
  eval_binary_primitive env (E_Const c1) B_Eq (E_Const c2)
    | c1 == c2                                                                  = Right (E_Const C_True, env)
    | otherwise                                                                 = Right (E_Const C_False, env)
  eval_binary_primitive env e1@(E_Location l1) B_Eq e2@(E_Location l2)          = Right (E_Apply (E_Apply (E_BPrim B_Eq) (E_Apply (E_UPrim U_Deref) e1)) (E_Apply (E_UPrim U_Deref) e2), env)
  eval_binary_primitive env (E_Cons _ _) B_Eq (E_Const C_Nil)                   = Right (E_Const C_False, env)
  eval_binary_primitive env (E_Const C_Nil) B_Eq (E_Cons _ _)                   = Right (E_Const C_False, env)
  eval_binary_primitive env (E_Cons e1 e2) B_Eq (E_Cons e1' e2')                = Right (E_And (E_Apply (E_Apply (E_BPrim B_Eq) e1) e1') (E_Apply (E_Apply (E_BPrim B_Eq) e2) e2'), env)
  eval_binary_primitive env (E_Tuple es1) B_Eq (E_Tuple es2)
    | length es1 == length es2                                                  = Right (foldl E_And (E_Const C_True) $ zipWith (\e1 e2 -> E_Apply (E_Apply (E_BPrim B_Eq) e1) e2) es1 es2, env)
    | otherwise                                                                 = Right (E_Const C_False, env)
  eval_binary_primitive env (E_Const (C_Int n1)) B_I_Plus (E_Const (C_Int n2))  = Right (E_Const $ C_Int $ n1 + n2, env)
  eval_binary_primitive env (E_Const (C_Int n1)) B_I_Minus (E_Const (C_Int n2)) = Right (E_Const $ C_Int $ n1 - n2, env)
  eval_binary_primitive env (E_Const (C_Int n1)) B_I_Mult (E_Const (C_Int n2))  = Right (E_Const $ C_Int $ n1 * n2, env)
  eval_binary_primitive env (E_Const (C_Int n1)) B_I_Div (E_Const (C_Int n2))
    | n2 /= 0                                                                   = Right (E_Const $ C_Int $ n1 `div` n2, env)
    | otherwise                                                                 = Left $ division_by_0
  eval_binary_primitive env e1@(E_Location l) B_Assign e2                       = Right (E_Const C_Unit, store_reference env e1 e2)

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

  eval_step_expr :: EvalEnv -> Expr -> Either String (Expr, EvalEnv)
  eval_step_expr env e@(E_Val vn) = Right (get_value env e, env)
  eval_step_expr env e@(E_Location l) = Right (get_reference env e, env)
  eval_step_expr env (E_Apply e1 e2)
    | not (is_value e1) = case eval_step_expr env e1 of
      Left err          -> Left err
      Right (e1', env') -> Right (E_Apply e1' e2, env')
    | is_value e1 && not (is_value e2) = case eval_step_expr env e2 of
      Left err          -> Left err
      Right (e2', env') -> Right (E_Apply e1 e2', env')
  eval_step_expr env (E_Apply (E_UPrim up) e1) = Right $ eval_unary_primitive env up e1
  eval_step_expr env (E_Apply (E_Apply (E_BPrim bp) e1) e2) = eval_binary_primitive env e1 bp e2
  eval_step_expr env (E_Apply (E_Function pm) e2) = match_clause env pm e2
  eval_step_expr env (E_Cons e1 e2)
    | not (is_value e1) = case eval_step_expr env e1 of
      Left err          -> Left err
      Right (e1', env') -> Right (E_Cons e1' e2, env')
    | is_value e1 && not (is_value e2) = case eval_step_expr env e2 of
      Left err          -> Left err
      Right (e2', env') -> Right (E_Cons e1 e2', env')
  eval_step_expr env (E_Tuple es) = eval_step_tuple env es [] where
    eval_step_tuple env []     acc = Right (E_Tuple acc, env)
    eval_step_tuple env (e:es) acc
      | is_value e = eval_step_tuple env es (e:acc)
      | otherwise  = case eval_step_expr env e of
        Left err         -> Left err
        Right (e', env') -> Right (E_Tuple $ (reverse acc) ++ (e':es), env')
  eval_step_expr env (E_And e1 e2) = Right (E_ITE e1 e2 (E_Const C_False), env)
  eval_step_expr env (E_Or e1 e2) = Right (E_ITE e1 (E_Const C_True) e2, env)
  eval_step_expr env (E_ITE (E_Const C_True) e2 e3) = Right (e2, env)
  eval_step_expr env (E_ITE (E_Const C_False) e2 e3) = Right (e3, env)
  eval_step_expr env (E_ITE e1 e2 e3)
    | not (is_value e1) = case eval_step_expr env e1 of
      Left err         -> Left err
      Right (e', env') -> Right (E_ITE e' e2 e3, env')
  eval_step_expr env (E_Seq e1 e2)
    | not (is_value e1) = case eval_step_expr env e1 of
      Left err          -> Left err
      Right (e1', env') -> Right (E_Seq e1' e2, env')
    | is_value e1        = Right (e2, env)
  eval_step_expr env (E_Let (p, e1) e2)
    | is_value e1 = Right (e2, gen_subst e1 p ++ env)
    | otherwise   = case eval_step_expr env e1 of
      Left err          -> Left err
      Right (e1', env') -> Right (E_Let (p, e1') e2, env')
  eval_step_expr env (E_LetRec lrbs es) = Right (es, gen_env lrbs ++ env) where
    gen_env []            = []
    gen_env ((vn, pm):bs) = (vn, recfun lrbs pm) : gen_env bs

  eval_expr :: EvalEnv -> Expr -> Either String Expr
  eval_expr env e = eval_expr' env e where
    eval_expr' env e
      | is_value e = Right e
      | otherwise  = case eval_step_expr env e of
        Left err         -> Left err
        Right (e', env') -> eval_expr' env' e'

  eval_definition :: EvalEnv -> Definition -> Either String EvalEnv
  eval_definition env (D_Let (p, e))
    | is_value e && e `matches` p       = Right $ gen_subst e p
    | is_value e && not (e `matches` p) = Left match_failure
    | not (is_value e)                  =
      case eval_expr env e of
        Left err                       -> Left err
        Right e'                       -> eval_definition env (D_Let (p, e'))
  eval_definition env (D_LetRec lrbs)   = Right $ gen_env lrbs where
    gen_env []                          = []
    gen_env ((vn, pm):bs)               = (vn, recfun lrbs pm) : gen_env bs

  eval_instruction :: EvalEnv -> Instruction -> Either String ([Expr], EvalEnv)
  eval_instruction env (IDF df) =
    case eval_definition env df of
      Left err                 -> Left err
      Right env'               -> Right (map (\(vn, _) -> E_Val vn) env', env' ++ env)
  eval_instruction env (IEX ex) =
    case eval_expr env ex of
      Left err                 -> Left err
      Right ex'                -> Right ([ex], ("it", ex):env)

  eval_program :: Program -> Either String ([Expr], EvalEnv)
  eval_program = eval_program' [] [] where
    eval_program' exprs env []     = Right (exprs, env)
    eval_program' exprs env (i:is) =
      case eval_instruction env i of
        Left err                  -> Left err
        Right (exprs', env')      -> eval_program' exprs' env' is
