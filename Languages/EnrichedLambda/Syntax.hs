module Languages.EnrichedLambda.Syntax where
  
  data Constant =
      C_Int Integer
    | C_True
    | C_False
    | C_Nil
    | C_Unit
    deriving Eq
  
  data UnaryPrim =
      U_Not
    | U_Ref
    | U_Deref
    | U_Fst
    | U_Snd
    | U_Head
    | U_Tail
    | U_Empty
    deriving Eq
  
  data BinaryPrim =
      B_Eq
    | B_Plus
    | B_Minus
    | B_Mult
    | B_Div
    | B_Assign
    deriving Eq
  
  data Expr =
      E_Var String
    | E_UPrim UnaryPrim
    | E_BPrim BinaryPrim
    | E_Const Constant
    | E_Location Integer
    | E_Cons Expr Expr
    | E_ITE Expr Expr Expr
    | E_Seq Expr Expr
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
