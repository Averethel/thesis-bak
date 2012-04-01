module Interpreters.MiniML.Typing (type_of_definitions) where
  import Interpreters.MiniML.Syntax
  import Interpreters.MiniML.PrettyPrint
  import Interpreters.MiniML.Kinding
  import Interpreters.MiniML.Errors
  import Interpreters.MiniML.Unification

  fresh_type_var :: [TypeVar] -> TypeVar
  fresh_type_var = ("a" ++).show.length

  names_distinct :: [ValueName] -> Bool
  names_distinct []     = True
  names_distinct (x:xs) = names_distinct' x xs xs where
    names_distinct' x []     []     = True
    names_distinct' x []     (y:ys) = names_distinct' y ys ys
    names_distinct' x (z:zs) ys
      | x /= z                      = names_distinct' x zs ys
      | otherwise                   = False

  type_of_constant :: [TypeVar] -> Constant -> (([TypeVar], Constraints, SimpleConstraints), TypeExpr)
  type_of_constant vs (C_Int n) = ((vs,   [], []), TE_Constr [] Int)
  type_of_constant vs C_False   = ((vs,   [], []), TE_Constr [] Bool)
  type_of_constant vs C_True    = ((vs,   [], []), TE_Constr [] Bool)
  type_of_constant vs C_Nil     = ((v:vs, [], []), TE_Constr [TE_Var v] List) where
    v = fresh_type_var vs
  type_of_constant vs C_Unit    = ((vs,   [], []), TE_Constr [] Unit)
  
  type_of_unary_primitive :: [TypeVar] -> UnaryPrim -> (([TypeVar], Constraints, SimpleConstraints), TypeExpr)
  type_of_unary_primitive vs U_Not     = ((vs,   [], []), TE_Arrow (TE_Constr [] Bool) (TE_Constr [] Bool))
  type_of_unary_primitive vs U_Ref     = ((v:vs, [], []), TE_Arrow (TE_Var v) (TE_Constr [TE_Var v] Ref)) where
    v = fresh_type_var vs
  type_of_unary_primitive vs U_Deref   = ((v:vs, [], []), TE_Arrow (TE_Constr [TE_Var v] Ref) (TE_Var v)) where
    v = fresh_type_var vs
  type_of_unary_primitive vs U_I_Minus = ((vs,   [], []), TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int))

  type_of_binary_primitive :: [TypeVar] -> BinaryPrim -> (([TypeVar], Constraints, SimpleConstraints), TypeExpr)
  type_of_binary_primitive vs B_Eq      = ((v:vs, [], [TE_Var v]), TE_Arrow (TE_Var v) (TE_Arrow (TE_Var v) (TE_Constr [] Bool))) where
    v = fresh_type_var vs
  type_of_binary_primitive vs B_I_Plus  = ((vs,   [], []),         TE_Arrow (TE_Constr [] Int) (TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)))
  type_of_binary_primitive vs B_I_Minus = ((vs,   [], []),         TE_Arrow (TE_Constr [] Int) (TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)))
  type_of_binary_primitive vs B_I_Mult  = ((vs,   [], []),         TE_Arrow (TE_Constr [] Int) (TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)))
  type_of_binary_primitive vs B_I_Div   = ((vs,   [], []),         TE_Arrow (TE_Constr [] Int) (TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)))
  type_of_binary_primitive vs B_Assign  = ((v:vs, [], []),         TE_Arrow (TE_Constr [TE_Var v] Ref) (TE_Arrow (TE_Var v) (TE_Constr [] Unit))) where
    v = fresh_type_var vs

  type_and_bindings_of_pattern :: [TypeVar] -> Pattern -> (([TypeVar], Constraints, SimpleConstraints), TypeExpr, Env)
  type_and_bindings_of_pattern vs (P_Val vn)         = ((v:vs, [], []), TE_Var v, [(vn, TE_Var v)]) where
    v = fresh_type_var vs
  type_and_bindings_of_pattern vs P_Wildcard         = ((v:vs, [], []), TE_Var v, []) where
    v = fresh_type_var vs
  type_and_bindings_of_pattern vs (P_Const c)        = ((vs', c', s'), tp, []) where
    ((vs', c', s'), tp) = type_of_constant vs c
  -- parser ensures that there are at least two elems in a tuple
  type_and_bindings_of_pattern vs (P_Tuple (p:ps))   = ((vs'', c'', s''), TE_Tuple $ reverse tes, e'') where
    ((vs', c', s'), tp, e')      = type_and_bindings_of_pattern vs p
    ((vs'', c'', s''), tes, e'') = foldl generate ((vs', c', s'), [tp], e') ps
    generate :: (([TypeVar], Constraints, SimpleConstraints), [TypeExpr], Env) -> Pattern -> (([TypeVar], Constraints, SimpleConstraints), [TypeExpr], Env)
    generate ((vs, cs, sc), ts, e) p = ((vs', cs' ++ cs, sc' ++ sc), tp:ts, e' ++ e) where
      ((vs', cs', sc'), tp, e') = type_and_bindings_of_pattern vs p
  type_and_bindings_of_pattern vs (P_Cons p1 p2)     = ((vs'', (te2, TE_Constr [te1] List):(cs1 ++ cs2), sc1 ++ sc2), te2, e1 ++ e2) where
    ((vs',  cs1, sc1), te1, e1) = type_and_bindings_of_pattern vs p1
    ((vs'', cs2, sc2), te2, e2) = type_and_bindings_of_pattern vs' p2

  type_of_function :: [TypeVar] -> Env -> [(Pattern, Expr)] -> Either String (([TypeVar], Constraints, SimpleConstraints), TypeExpr)
  -- parser ensures that there is at least one clause
  type_of_function vs env ((p,e):ps) =
    case type_of_expression vs' (env ++ envp) e of
      Left err                                       -> Left err
      Right ((vse', cse1, sce1), tee)                ->
        case foldl generate (Right ((vse', cse1 ++ csp1, sce1 ++ scp1), [TE_Arrow tep tee])) ps of
          Left err                                   -> Left err
          Right ((vsf, csf, scf), (te:tes))          -> Right ((vsf, csf ++ map ((,) te) tes, scf), te)
        where
          generate :: Either String (([TypeVar], Constraints, SimpleConstraints), [TypeExpr]) -> (Pattern, Expr) -> Either String (([TypeVar], Constraints, SimpleConstraints), [TypeExpr])
          generate (Left err)                 _      = Left err
          generate (Right ((vs, cs, sc), ts)) (p, e) =
            case type_of_expression vs' (env ++ env') e of
              Left err                        -> Left err
              Right ((vs'', cs'', sc''), tpe) -> Right ((vs'', cs'' ++ cs' ++ cs, sc'' ++ sc' ++ sc), (TE_Arrow tpp tpe):ts)
            where
              ((vs', cs', sc'), tpp, env') = type_and_bindings_of_pattern vs p
    where
      ((vs', csp1, scp1), tep, envp) = type_and_bindings_of_pattern vs p

  type_of_expression :: [TypeVar] -> Env -> Expr -> Either String (([TypeVar], Constraints, SimpleConstraints), TypeExpr)
  type_of_expression vs env (E_UPrim up)               = Right $ type_of_unary_primitive vs up
  type_of_expression vs env (E_BPrim bp)               = Right $ type_of_binary_primitive vs bp
  type_of_expression vs env (E_Val vn)                 =
    case vn `lookup` env of
      Nothing                                         -> Left $ unbound_variable vn
      Just tp                                         -> Right ((vs, [], []), tp)
  type_of_expression vs env (E_Const c)                = Right $ type_of_constant vs c
  type_of_expression vs env (E_Apply e1 e2)            =
    case type_of_expression vs env e1 of
      Left err                                        -> Left err
      Right ((vs', cs1, sc1), tp1)                    ->
        case type_of_expression vs' env e2 of
          Left err                                    -> Left err
          Right ((vs'', cs2, sc2), tp2)               -> Right ((v:vs'', (tp1, TE_Arrow tp2 (TE_Var v)):(cs1 ++ cs2), sc1 ++ sc2), TE_Var v) where
            v = fresh_type_var vs''
  type_of_expression vs env (E_Cons e1 e2)             =
    case type_of_expression vs env e1 of
      Left err                                        -> Left err
      Right ((vs', cs1, sc1), tp1)                    ->
        case type_of_expression vs' env e2 of
          Left err                                    -> Left err
          Right ((vs'', cs2, sc2), tp2)               -> Right ((vs'', (tp2, TE_Constr [tp1] List):(cs1 ++ cs2), sc1 ++ sc2), tp2)
  -- parser ensures that there are at least two elems in a tuple
  type_of_expression vs env (E_Tuple (e:es))          =
    case type_of_expression vs env e of
      Left err                                       -> Left err
      Right ((vs', c', s'), tp)                      ->
        case foldl generate (Right ((vs', c', s'), [tp])) es of
          Left err                                   -> Left err
          Right ((vs'', c'', s''), tes)              -> Right ((vs'', c'', s''), TE_Tuple $ reverse tes)
        where
          generate :: Either String (([TypeVar], Constraints, SimpleConstraints), [TypeExpr]) -> Expr -> Either String (([TypeVar], Constraints, SimpleConstraints), [TypeExpr])
          generate (Left err)                 _ = Left err
          generate (Right ((vs, cs, sc), ts)) e =
            case type_of_expression vs env e of
              Left err                    -> Left err
              Right ((vs', cs', sc'), tp) -> Right ((vs', cs' ++ cs, sc' ++ sc), tp:ts)
  type_of_expression vs env (E_And e1 e2)             =
    case type_of_expression vs env e1 of
      Left err                                       -> Left err
      Right ((vs', cs1, sc1), tp1)                   ->
        case type_of_expression vs' env e2 of
          Left err                                   -> Left err
          Right ((vs'', cs2, sc2), tp2)              -> Right ((vs'', (tp1, TE_Constr [] Bool):(tp2, TE_Constr [] Bool):(cs1 ++ cs2), sc1 ++ sc2), tp2)
  type_of_expression vs env (E_Or e1 e2)              =
    case type_of_expression vs env e1 of
      Left err                                       -> Left err
      Right ((vs', cs1, sc1), tp1)                   ->
        case type_of_expression vs' env e2 of
          Left err                                   -> Left err
          Right ((vs'', cs2, sc2), tp2)              -> Right ((vs'', (tp1, TE_Constr [] Bool):(tp2, TE_Constr [] Bool):(cs1 ++ cs2), sc1 ++ sc2), tp2)
  type_of_expression vs env (E_ITE e1 e2 e3)          =
    case type_of_expression vs env e1 of
      Left err                                       -> Left err
      Right ((vs', cs1, sc1), tp1)                   ->
        case type_of_expression vs' env e2 of
          Left err                                   -> Left err
          Right ((vs'', cs2, sc2), tp2)              ->
            case type_of_expression vs'' env e3 of
              Left err                               -> Left err
              Right ((vs''', cs3, sc3), tp3)         -> Right ((vs''', (tp1, TE_Constr [] Bool):(tp2, tp3):(cs1 ++ cs2 ++ cs3), sc1 ++ sc2 ++ sc3), tp2)
  type_of_expression vs env (E_Seq e1 e2)             =
    case type_of_expression vs env e1 of
      Left err                                       -> Left err
      Right ((vs', cs1, sc1), tp1)                   ->
        case type_of_expression vs' env e2 of
          Left err                                   -> Left err
          Right ((vs'', cs2, sc2), tp2)              -> Right ((vs'', (tp1, TE_Constr [] Unit):(cs1 ++ cs2), sc1 ++ sc2), tp2)
  type_of_expression vs env (E_Function ps)           = type_of_function vs env ps
  type_of_expression vs env (E_Let (p, e1) e2)        =
    case type_of_expression vs env e1 of
      Left err                                       -> Left err
      Right ((vs', cs1, sc1), tp1)                   ->
        case type_of_expression vs'' (env' ++ env) e2 of
          Left err                                   -> Left err
          Right ((vs''', cs2, sc2), tp2)             -> Right ((vs''', (tp1, tpp):(cs1 ++ cs2 ++ csp), sc1 ++ sc2 ++ scp), tp2)
        where
          ((vs'', csp, scp), tpp, env') = type_and_bindings_of_pattern vs' p
  -- parser ensures that there is at least one clause
  type_of_expression vs env expr@(E_LetRec bs@((vn, f):fs) e)
    | not (names_distinct $ map fst bs)               = Left $ non_distinct_names expr
    | otherwise                                       =
      case type_of_function vs' env' f of
        Left err                                     -> Left err
        Right ((vs'', cs'', sc''), te'')             ->
          case foldl generate (Right ((vs'', cs'', sc''), [te''])) fs of
            Left err                                 -> Left err
            Right ((vs''', cs''', sc'''), tes)       ->
              case type_of_expression vs''' env' e of
                Left err                             -> Left err
                Right ((vse, cse, sce), tee)         -> Right ((vse, (zip tes $ map TE_Var vs') ++ cse ++ cs''', sce ++ sc'''), tee)
          where
            generate :: Either String (([TypeVar], Constraints, SimpleConstraints), [TypeExpr]) -> (ValueName, [(Pattern, Expr)]) -> Either String (([TypeVar], Constraints, SimpleConstraints), [TypeExpr])
            generate (Left err)                 _      = Left err
            generate (Right ((vs, cs, sc), ts)) (v, f) =
              case type_of_function vs env' f of
                Left err                              -> Left err
                Right ((vs', cs', sc'), tp)           -> Right ((vs', cs' ++ cs, sc' ++ sc), tp:ts)
      where
        (env', vs') = foldl (\(e, v) (vn, _) -> let f = fresh_type_var v in ((vn, TE_Var f):e, f:v)) (env, vs) bs

  type_of_definition :: [TypeVar] -> Env -> Definition -> Either String (([TypeVar], Constraints, SimpleConstraints), Env)
  type_of_definition vs env (D_Let (p, e)) =
    case type_of_expression vs env e of
      Left err                                       -> Left err
      Right ((vs', cs1, sc1), tp1)                   -> Right ((vs'', (tp1, tpp):(cs1 ++ csp), sc1 ++ scp), env')
        where
          ((vs'', csp, scp), tpp, env') = type_and_bindings_of_pattern vs' p
  type_of_definition vs env def@(D_LetRec bs@((vn, f):fs))
    | not (names_distinct $ map fst bs)               = Left $ non_distinct_names def
    | otherwise                                       =
      case type_of_function vs' env' f of
        Left err                                     -> Left err
        Right ((vs'', cs'', sc''), te'')             ->
          case foldl generate (Right ((vs'', cs'', sc''), [te''])) fs of
            Left err                                 -> Left err
            Right ((vs''', cs''', sc'''), tes)       -> Right ((vs''', (zip tes $ map TE_Var vs') ++ cs''', sc'''), env')
          where
            generate :: Either String (([TypeVar], Constraints, SimpleConstraints), [TypeExpr]) -> (ValueName, [(Pattern, Expr)]) -> Either String (([TypeVar], Constraints, SimpleConstraints), [TypeExpr])
            generate (Left err)                 _      = Left err
            generate (Right ((vs, cs, sc), ts)) (v, f) =
              case type_of_function vs env' f of
                Left err                              -> Left err
                Right ((vs', cs', sc'), tp)           -> Right ((vs', cs' ++ cs, sc' ++ sc), tp:ts)
      where
        (env', vs') = foldl (\(e, v) (vn, _) -> let f = fresh_type_var v in ((vn, TE_Var f):e, f:v)) (env, vs) bs

  type_of_definitions :: Definitions -> Either String Env
  type_of_definitions = type_of_definitions' [] [] [] [] where
    type_of_definitions' :: [TypeVar] -> Constraints -> SimpleConstraints -> Env -> Definitions -> Either String Env
    type_of_definitions' vs cs scs env []     =
      case unify cs scs env of
        Left err                             -> Left err
        Right (scs', env')                   -> check_types_simple scs' env'
    type_of_definitions' vs cs scs env (d:ds) =
      case type_of_definition vs env d of
        Left err                             -> Left err
        Right ((vs', cs', scs'), env')       -> type_of_definitions' vs' (cs' ++ cs) (scs' ++ scs) (env' ++ env) ds
