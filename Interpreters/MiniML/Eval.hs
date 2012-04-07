module Interpreters.MiniML.Eval where
  import Interpreters.MiniML.Errors
  import Interpreters.MiniML.Syntax
  import Interpreters.MiniML.Typing

  import Data.Maybe

  is_value :: Expr -> Bool
  is_value (E_UPrim _)             = True
  is_value (E_BPrim _)             = True
  -- is_value (E_Val _)               = True --?
  -- is_value (E_Location _)          = True --?
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
