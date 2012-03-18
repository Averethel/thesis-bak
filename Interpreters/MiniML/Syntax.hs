module Interpreters.MiniML.Syntax where
  type LowercaseIdent = String

  type ValueName = LowercaseIdent

  type TypeVar = String

  data TypeConstr =
      Int
    | Float
    | Bool
    | Unit
    | List
    | Ref
    deriving (Show, Eq)

  data TypeExpr =
      TE_Var      TypeVar
    | TE_Arrow    TypeExpr TypeExpr
    | TE_Tuple    [TypeExpr]
    | TE_Constr   [TypeExpr] TypeConstr
    deriving (Show, Eq)

  data Constant =
      C_Int Integer
    | C_Float Float
    | C_False
    | C_True
    | C_Nil
    | C_Unit
    deriving (Show, Eq)

  data UnaryPrim =
      U_Not
    | U_Ref
    | U_Deref
    | U_Minus
    deriving (Show, Eq)

  data BinaryPrim =
      B_Eq
    | B_I_Plus
    | B_I_Minus
    | B_I_Mult
    | B_I_Div
    | B_F_Plus
    | B_F_Minus
    | B_F_Mult
    | B_F_Div
    | B_Assign
    deriving (Show, Eq)

  data Expr =
      E_UPrim UnaryPrim Expr
    | E_BPrim Expr BinaryPrim Expr
    | E_Val ValueName
    | E_Const Constant
    | E_Typed Expr TypeExpr
    | E_Cons Expr Expr
    | E_List [Expr]
    | E_Tuple [Expr]
    | E_And Expr Expr
    | E_Or Expr Expr
    | E_ITE Expr Expr Expr
    | E_While Expr Expr
    | E_Seq Expr Expr
    | E_Fun [Pattern] Expr
    | E_Function PatternMatching
    | E_Let LetBinding Expr
    | E_LetRex LetRecBindings Expr
    deriving (Show, Eq)

  data Pattern =
      P_Val ValueName
    | P_Wildcard
    | P_Const Constant
    | P_List [Pattern]
    | P_Tuple [Pattern]
    | P_Cons Pattern Pattern
    deriving (Show, Eq)

  type LetBinding = (Pattern, Expr)

  type LetRecBindings = [LetRecBinding]

  data LetRecBinding =
      LB_Fun ValueName [Pattern] Expr
    | LB_Fuction ValueName PatternMatching
    deriving (Show, Eq)

  type PatExpr = (Pattern, Expr)

  type PatternMatching = [PatExpr]

  data Definition =
      D_Let LetBinding
    | D_LetRec LetRecBindings
    deriving (Show, Eq)

  type Definitions = [Definition]

  data Program =
      P Definitions Expr
