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

  isInfix :: BinaryPrim -> Bool
  isInfix _ = True

  data Pattern =
      P_Val ValueName
    | P_Wildcard
    | P_Const Constant
    | P_Tuple [Pattern]
    | P_Cons Pattern Pattern
    -- P_List [Pattern] - syntactic sugar for foldr P_Cons (P_Const C_Nil)
    deriving Eq

  isAtomicPattern :: Pattern -> Bool
  isAtomicPattern P_Wildcard     = True
  isAtomicPattern (P_Val _)      = True
  isAtomicPattern (P_Const _)    = True
  isAtomicPattern _              = False

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
    | E_Case Expr [Binding]
    | E_Seq Expr Expr
    | E_Function [Binding]
    | E_Let [Binding] Expr
    | E_LetRec [LetRecBinding] Expr
    | Null -- for memory emptiness
    deriving Eq

  isAtomicExpr :: Expr -> Bool
  isAtomicExpr (E_Val _)      = True
  isAtomicExpr (E_Location _) = True
  isAtomicExpr (E_Const _)    = True
  isAtomicExpr _              = False

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

  isAtomicKind :: Kind -> Bool
  isAtomicKind K_Type = True
  isAtomicKind _      = False

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

  isAtomicTypeExpr :: TypeExpr -> Bool
  isAtomicTypeExpr (TE_Arrow  _     _) = False
  isAtomicTypeExpr (TE_Constr (_:_) _) = False
  isAtomicTypeExpr _                   = True
