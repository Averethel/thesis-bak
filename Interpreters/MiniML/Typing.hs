module Interpreters.MiniML.Typing where
  import Interpreters.MiniML.Syntax
  import Interpreters.MiniML.PrettyPrint
  import Interpreters.MiniML.Kinding
  import Interpreters.MiniML.Errors

  fresh_type_var :: [TypeVar] -> TypeVar
  fresh_type_var = ("a" ++).show.length

  type_of_constant :: [TypeVar] -> Constant -> (([TypeVar], Constraints, SimpleConstraints), TypeExpr)
  type_of_constant vs (C_Int n) = (([],  [], []), TE_Constr [] Int)
  type_of_constant vs C_False   = (([],  [], []), TE_Constr [] Bool)
  type_of_constant vs C_True    = (([],  [], []), TE_Constr [] Bool)
  type_of_constant vs C_Nil     = (([v], [], []), TE_Constr [TE_Var v] List) where
    v = fresh_type_var vs
  type_of_constant vs C_Unit    = (([],  [], []), TE_Constr [] Unit)
  
  type_of_unary_primitive :: [TypeVar] -> UnaryPrim -> (([TypeVar], Constraints, SimpleConstraints), TypeExpr)
  type_of_unary_primitive vs U_Not     = (([],  [], []), TE_Arrow (TE_Constr [] Bool) (TE_Constr [] Bool))
  type_of_unary_primitive vs U_Ref     = (([v], [], []), TE_Arrow (TE_Var v) (TE_Constr [TE_Var v] Ref)) where
    v = fresh_type_var vs
  type_of_unary_primitive vs U_Deref   = (([v], [], []), TE_Arrow (TE_Constr [TE_Var v] Ref) (TE_Var v)) where
    v = fresh_type_var vs
  type_of_unary_primitive vs U_I_Minus = (([],  [], []), TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int))

  type_of_binary_primitive :: [TypeVar] -> BinaryPrim -> (([TypeVar], Constraints, SimpleConstraints), TypeExpr)
  type_of_binary_primitive vs B_Eq      = (([v], [], [TE_Var v]), TE_Arrow (TE_Var v) (TE_Arrow (TE_Var v) (TE_Constr [] Bool))) where
    v = fresh_type_var vs
  type_of_binary_primitive vs B_I_Plus  = (([],  [], []),         TE_Arrow (TE_Constr [] Int) (TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)))
  type_of_binary_primitive vs B_I_Minus = (([],  [], []),         TE_Arrow (TE_Constr [] Int) (TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)))
  type_of_binary_primitive vs B_I_Mult  = (([],  [], []),         TE_Arrow (TE_Constr [] Int) (TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)))
  type_of_binary_primitive vs B_I_Div   = (([],  [], []),         TE_Arrow (TE_Constr [] Int) (TE_Arrow (TE_Constr [] Int) (TE_Constr [] Int)))
  type_of_binary_primitive vs B_Assign  = (([v], [], []),         TE_Arrow (TE_Constr [TE_Var v] Ref) (TE_Arrow (TE_Var v) (TE_Constr [] Unit))) where
    v = fresh_type_var vs

  type_and_bindings_of_pattern :: [TypeVar] -> Pattern -> (([TypeVar], Constraints, SimpleConstraints), TypeExpr, Env)
  type_and_bindings_of_pattern vs (P_Val vn)         = (([v], [], []), TE_Var v, [(vn, TE_Var v)]) where
    v = fresh_type_var vs
  type_and_bindings_of_pattern vs P_Wildcard         = (([v], [], []), TE_Var v, []) where
    v = fresh_type_var vs
  type_and_bindings_of_pattern vs (P_Const c)        = ((vs', c', s'), tp, []) where
    ((vs', c', s'), tp) = type_of_constant vs c
  -- parser ensures that there are at least two elems in a tuple
  type_and_bindings_of_pattern vs (P_Tuple (p:ps))   = ((vs'', c'', s''), TE_Tuple $ reverse tes, e'') where
    ((vs', c', s'), tp, e')      = type_and_bindings_of_pattern vs p
    ((vs'', c'', s''), tes, e'') = foldl generate ((vs', c', s'), [tp], e') ps
    generate :: (([TypeVar], Constraints, SimpleConstraints), [TypeExpr], Env) -> Pattern -> (([TypeVar], Constraints, SimpleConstraints), [TypeExpr], Env)
    generate ((vs, cs, sc), ts, e) p = ((vs' ++ vs, cs' ++ cs, sc' ++ sc), tp:ts, e' ++ e) where
      ((vs', cs', sc'), tp, e') = type_and_bindings_of_pattern vs p
  type_and_bindings_of_pattern vs (P_Cons p1 p2)     = ((vs'', (te2, TE_Constr [te1] List):(cs1 ++ cs2), sc1 ++ sc2), te2, e1 ++ e2) where
    ((vs',  cs1, sc1), te1, e1) = type_and_bindings_of_pattern vs p1
    ((vs'', cs2, sc2), te2, e2) = type_and_bindings_of_pattern vs' p1
