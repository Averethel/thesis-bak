module Languages.MiniML.Syntax where

  type LowercaseIdent = String

  type ValueName = LowercaseIdent

  data Constant =
      C_Int Integer
    | C_False
    | C_True
    | C_Nil
    | C_Unit
    deriving Eq

  data UnaryPrim =
      U_Not
    | U_Ref
    | U_Deref
    | U_I_Minus
    deriving Eq
  
  data BinaryPrim =
      B_Eq
    | B_I_Plus
    | B_I_Minus
    | B_I_Mult
    | B_I_Div
    | B_Assign
    deriving Eq

  data Pattern =
      P_Val ValueName
    | P_Wildcard
    | P_Const Constant
    | P_Tuple [Pattern]
    | P_Cons Pattern Pattern
    -- P_List [Pattern] - syntactic sugar for foldr P_Cons (P_Const C_Nil)
    deriving Eq

  type Binding = (Pattern, Expr)

  type LetRecBinding = (ValueName, [Binding])

  data Expr =
      E_UPrim UnaryPrim
    | E_BPrim BinaryPrim
    | E_Val ValueName
    | E_Location Integer -- for internal representation of references
    | E_Const Constant
    | E_Apply Expr Expr
    | E_Cons Expr Expr
    -- E_List [Expr] - syntactic sugar for foldr E_Cons (E_Const C_Nil)
    | E_Tuple [Expr]
    | E_And Expr Expr
    | E_Or Expr Expr
    | E_ITE Expr Expr Expr
    | E_Seq Expr Expr
    | E_Function [Binding]
    | E_Let Binding Expr
    | E_LetRec [LetRecBinding] Expr
    deriving Eq

  data Definition =
      D_Let (Pattern, Expr)
    | D_LetRec [LetRecBinding]
    deriving Eq

  data Instruction =
      IDF Definition
    | IEX Expr

  type Program = [Instruction]

  data Kind = 
      K_Type
    | K_Arrow Kind Kind
    deriving Eq

  data TypeConstr =
      Int
    | Bool
    | Unit
    | List
    | Ref
    deriving (Show, Eq)

  type TypeVar = String

  data TypeExpr =
      TE_Var      TypeVar
    | TE_Arrow    TypeExpr TypeExpr
    | TE_Tuple    [TypeExpr]
    | TE_Constr   [TypeExpr] TypeConstr
    deriving Eq

  type Env = [(ValueName, TypeExpr)]

  type EvalEnv = [(ValueName, Expr)]

  type Mem = [Expr]

  type Constraints = [(TypeExpr, TypeExpr)]

  type SimpleConstraints = [TypeExpr]

  type InterpreterState = ((Env, Constraints, SimpleConstraints, [TypeVar]), (EvalEnv, Mem))

  empty_state :: InterpreterState
  empty_state = (([], [], [], []), ([], []))
