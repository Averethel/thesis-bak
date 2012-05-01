module Languages.EnrichedLambda.Syntax where
  
  data Constant =
      C_Int Integer
    | C_True
    | C_False
    | C_Nil
    | C_Unit
    deriving Eq
  
  data Expr =
      E_Var String
    | E_Const Constant
    | E_Location Integer
    | E_Not Expr
    | E_Ref Expr
    | E_Deref Expr
    | E_Eq Expr Expr
    | E_Plus Expr Expr
    | E_Minus Expr Expr
    | E_Div Expr Expr
    | E_Mult Expr Expr
    | E_Assign Expr Expr
    | E_Head Expr
    | E_Tail Expr
    | E_Cons Expr Expr
    | E_ITE Expr Expr Expr
    | E_Seq Expr Expr
    | E_Fst Expr
    | E_Snd Expr
    | E_Pair Expr Expr
    | E_Let String Expr Expr
    | E_Letrec String Expr Expr
    | E_Apply Expr Expr
    | E_Function String Expr
    | E_MatchFailure
    | Null -- for memory emptiness
    deriving Eq
  
  data Type =
      T_Var String
    | T_Int
    | T_Bool
    | T_Unit
    | T_Pair Type Type
    | T_List Type
    | T_Arrow Type Type
    | T_Ref Type
    deriving Eq
