module Languages.EnrichedLambdaLowLevelTypes.Syntax where
  data Tag = 
      Tg_Nil
    | Tg_Cons
    | Tg_True
    | Tg_False
    | Tg_Unit
    | Tg_Pair
    deriving (Show, Eq)
  
  data Struct = 
      S_Ref Expr
    | S_Str Tag Integer [Struct]
    | S_Ptr Integer
    | S_StaticPtr Integer
    | S_Int Integer
    | Null
    deriving Eq
  
  data Prim = 
      P_AllocPair
    | P_AllocList
    | P_Not
    | P_Ref
    | P_Deref
    | P_Fst
    | P_Snd
    | P_Head
    | P_Tail
    | P_Empty
    | P_Eq
    | P_Plus
    | P_Minus
    | P_Mult
    | P_Div
    | P_Assign
    deriving Eq
  
  type LetRecBinding = (String, Expr)

  data Expr = 
      E_Prim Prim
    | E_Val String
    | E_Struct Struct
    | E_ITE Expr Expr Expr
    | E_Seq Expr Expr
    | E_Let String Expr Expr
    | E_LetRec [LetRecBinding] Expr
    | E_Apply Expr Expr
    | E_Function String Expr
    | E_MatchFailure
    deriving Eq
  
  data Type = 
      T_Int
    | T_Bool
    | T_Unit
    | T_Var String
    | T_List Type
    | T_Ref Type
    | T_Pair Type Type
    | T_Arrow Type Type
    deriving Eq
  
  is_binary :: Prim -> Bool
  is_binary P_AllocPair = True
  is_binary P_AllocList = True
  is_binary P_Eq        = True
  is_binary P_Plus      = True
  is_binary P_Minus     = True
  is_binary P_Mult      = True
  is_binary P_Div       = True
  is_binary P_Assign    = True
  is_binary _           = False
