 module Languages.EnrichedLambda.Syntax where
  import Utils.EvalEnv
  import Utils.Memory

  type Name       = String
  type ConstrTag  = Int
  type TypeTag    = Int
  type Arity      = Int

  boolTag :: TypeTag
  boolTag = 0

  falseTag :: ConstrTag
  falseTag = 0

  trueTag :: ConstrTag
  trueTag = 1

  unitTag :: TypeTag
  unitTag = 1

  unitTagC :: ConstrTag
  unitTagC = 0

  listTag :: TypeTag
  listTag = 2

  nilTag :: ConstrTag
  nilTag = 0

  consTag :: ConstrTag
  consTag = 1

  pairTag :: TypeTag
  pairTag = 3

  pairTagC :: ConstrTag
  pairTagC = 0

  data UnaryPrim =
      U_Not
    | U_Ref
    | U_Deref
    | U_Head
    | U_Tail
    | U_Empty
    | U_Fst
    | U_Snd
    | U_PartBin BinaryPrim Value
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
    | B_Cons
    | B_Pair
    deriving Eq

  type Clause = (TypeTag, ConstrTag, [Name], Expr)
  type Binding = (Name, Expr)

  data Expr =
      E_UPrim UnaryPrim
    | E_BPrim BinaryPrim
    | E_Val Name
    | E_Num Integer
    | E_Location Integer
    | E_Constr TypeTag ConstrTag Arity
    | E_Seq Expr Expr
    | E_Apply Expr Expr
    | E_Rescue Expr Expr
    | E_Let [Binding] Expr
    | E_LetRec [Binding] Expr
    | E_Case Expr [Clause]
    | E_Function Name Expr
    | E_MatchFailure
    deriving Eq

  data Value =
      V_UPrim UnaryPrim
    | V_BPrim BinaryPrim
    | V_Clo (Env Value Expr) Name Expr
    | V_Error String
    | V_Pointer Integer
    | V_Int Integer
    | V_Cell TypeTag ConstrTag [Value]
    | V_Null
    deriving Eq

  data Definition =
      D_Let [Binding]
    | D_LetRec [Binding]
    deriving Eq

  type Program = ([Definition], Expr)

  data Type =
      T_Var String
    | T_Arrow Type Type
    | T_Ref Type
    | T_Int
    | T_Defined TypeTag [Type]
    deriving Eq
