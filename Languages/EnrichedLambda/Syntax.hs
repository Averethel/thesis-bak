 module Languages.EnrichedLambda.Syntax where
  
  type Name       = String
  type Constr_Tag = Int
  type Type_Tag   = Int
  type Arity      = Int

  data UnaryPrim =
      U_Not
    | U_Ref
    | U_Deref
    | U_Head
    | U_Tail
    | U_Fst
    | U_Snd
    deriving Eq
  
  data BinaryPrim =
      B_Eq
    | B_Plus
    | B_Minus
    | B_Mult
    | B_Div
    | B_Assign
    | B_And
    | B_Or
    deriving Eq

  type Clause = (Int, [Name], Expr)
  type LetBinding = (Name, Expr)

  data Expr = 
      E_UPrim UnaryPrim
    | E_BPrim BinaryPrim
    | E_Val Name
    | E_Num Integer
    | E_Location Integer
    | E_Constr Type_Tag Constr_Tag Arity
    | E_Seq Expr Expr
    | E_Apply Expr Expr
    | E_Let [LetBinding] Expr
    | E_LetRec [LetBinding] Expr
    | E_Case Expr [Clause]
    | E_Function Name Expr
    | Null
    deriving Eq

  isInfix :: BinaryPrim -> Bool
  isInfix _ = True

  isAtomicExpr :: Expr -> Bool
  isAtomicExpr (E_Val _)      = True
  isAtomicExpr (E_Num _)      = True
  isAtomicExpr (E_Location _) = True
  isAtomicExpr _              = False

  type Binding = (Name, [Name], Expr)

  data Definition = 
      D_Let [Binding]
    | D_LetRec [Binding]
    deriving Eq

  type Program = ([Definition], Expr)

  data Type =
      T_Var String
    | T_Arrow Type Type
    | T_Ref Type
    | T_Defined Type_Tag [Type]
    deriving Eq
