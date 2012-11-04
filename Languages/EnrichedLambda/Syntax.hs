 module Languages.EnrichedLambda.Syntax where
  
  type Name       = String
  type ConstrTag  = Int
  type TypeTag    = Int
  type Arity      = Int

  bool_tag :: TypeTag
  bool_tag = 0

  false_tag :: ConstrTag
  false_tag = 0

  true_tag :: ConstrTag
  true_tag = 1

  unit_tag :: TypeTag
  unit_tag = 1

  unit_tag_c :: ConstrTag
  unit_tag_c = 0

  list_tag :: TypeTag
  list_tag = 2

  nil_tag :: ConstrTag
  nil_tag = 0

  cons_tag :: ConstrTag
  cons_tag = 1

  pair_tag :: TypeTag
  pair_tag = 3

  pair_tag_c :: ConstrTag
  pair_tag_c = 0

  data UnaryPrim =
      U_Not
    | U_Ref
    | U_Deref
    | U_Head
    | U_Tail
    | U_Empty
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

  type Clause = (TypeTag, ConstrTag, [Name], Expr)
  type LetBinding = (Name, Expr)

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
    | E_Let [LetBinding] Expr
    | E_LetRec [LetBinding] Expr
    | E_Case Expr [Clause]
    | E_Function Name Expr
    | E_MatchFailure
    | Null
    deriving Eq

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
    | T_Defined TypeTag [Type]
    deriving Eq
