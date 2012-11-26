module Languages.MiniML.Syntax where
  import Utils.EvalEnv

  data MiniMLName = MiniML

  type Name = String

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
    | U_Fst
    | U_Snd
    | U_Empty
    | U_Head
    | U_Tail
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
      P_Val Name
    | P_Wildcard
    | P_Const Constant
    | P_Tuple [Pattern]
    | P_Cons Pattern Pattern
    deriving Eq

  type Binding = (Pattern, Expr)

  type FunBinding = (Pattern, Expr, Expr)

  type LetRecBinding = (Name, Expr)

  data Expr =
      E_UPrim UnaryPrim
    | E_BPrim BinaryPrim
    | E_Val Name
    | E_Const Constant
    | E_Apply Expr Expr
    | E_Cons Expr Expr
    | E_Tuple [Expr]
    | E_And Expr Expr
    | E_Or Expr Expr
    | E_ITE Expr Expr Expr
    | E_Case Expr [Binding]
    | E_Seq Expr Expr
    | E_Function [FunBinding]
    | E_Let [Binding] Expr
    | E_LetRec [LetRecBinding] Expr
    | E_MatchFailure
    | E_FatBar Expr Expr
    deriving Eq

  data Value =
      V_UPrim UnaryPrim
    | V_BPrim BinaryPrim
    | V_Unit
    | V_Int Integer
    | V_Bool Bool
    | V_List [Value]
    | V_Tuple [Value]
    | V_Clo (Env Value Expr) [FunBinding]
    | V_Null
    | V_Pointer Integer
    | V_Error String
    deriving Eq

  data Definition =
      D_Let [Binding]
    | D_LetRec [LetRecBinding]
    deriving Eq

  data Instruction =
      IDF Definition
    | IEX Expr
    deriving Eq

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
    deriving Eq

  type TypeVar = String

  data TypeExpr =
      TE_Var      TypeVar
    | TE_Arrow    TypeExpr TypeExpr
    | TE_Tuple    [TypeExpr]
    | TE_Constr   [TypeExpr] TypeConstr
    deriving Eq
