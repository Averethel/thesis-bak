module Interpreters.MiniML.Syntax where
  import Interpreters.MiniML.Errors

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

  data TypeScheme =
    TS_Forall TypeExpr
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
    | E_UPrimFree UnaryPrim
    | E_BPrim Expr BinaryPrim Expr
    | E_BPrimFree BinaryPrim
    | E_BPrimPart BinaryPrim Expr
    | E_Val ValueName
    | E_Const Constant
    | E_Typed Expr TypeExpr
    | E_Apply Expr Expr
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
    | E_LetRec LetRecBindings Expr
    deriving (Show, Eq)

  data Pattern =
      P_Val ValueName
    | P_Wildcard
    | P_Const Constant
    | P_List [Pattern]
    | P_Tuple [Pattern]
    | P_Cons Pattern Pattern
    -- być może trzeba dodać tutaj opcję dla konstruktora typu!!!
    deriving (Show, Eq)

  type LetBinding = (Pattern, Expr)

  type LetRecBindings = [LetRecBinding]

  data LetRecBinding =
      -- LB_Fun ValueName [Pattern] Expr
    LB_Function ValueName PatternMatching
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

  data Kind = 
      K_Type
    | K_Arrow Integer -- Type^n -> Type
    deriving (Show, Eq)

  data EnvBinding =
      EB_TV TypeVar
    | EB_TE ValueName TypeExpr
    | EB_TS ValueName TypeScheme
    deriving (Show, Eq)

  type Env = [EnvBinding]

  fresh_type_var :: Env -> TypeVar
  fresh_type_var xs = "a" ++ (show $ length xs)

  is_bound_in :: TypeVar -> Env -> Bool
  is_bound_in _ []                = False
  is_bound_in v ((EB_TV tv):ebs)
    | v == tv                     = True
    | otherwise                   = v `is_bound_in` ebs
  is_bound_in v (_:ebs)           = v `is_bound_in` ebs

  type_lookup :: ValueName -> Env -> Either String TypeExpr
  type_lookup v []                = Left $ unbound_variable v
  type_lookup v ((EB_TV _):es)    = v `type_lookup` es
  type_lookup v ((EB_TE v' t):es)
    | v == v'                     = Right t
    | otherwise                   = v `type_lookup` es

  get_names :: Env -> [ValueName]
  get_names []               = []
  get_names ((EB_TE v _):xs) = v : get_names xs
  get_names ((EB_TS v _):xs) = v : get_names xs
  get_names (_:xs)           = get_names xs

  names_distinct :: Eq a => [a] -> Bool
  names_distinct []     = True
  names_distinct (x:xs) = not(x `elem` xs) && names_distinct xs

  is_non_expansive :: Expr -> Bool
  is_non_expansive (E_UPrim up ex)      = True
  is_non_expansive (E_UPrimFree up)     = True
  is_non_expansive (E_BPrim ex1 bp ex2) = True
  is_non_expansive (E_BPrimFree bp)     = True
  is_non_expansive (E_BPrimPart bp ex1) = is_non_expansive ex1
  is_non_expansive (E_Val v)            = True
  is_non_expansive (E_Const c)          = True
  is_non_expansive (E_Typed ex t)       = is_non_expansive ex
  is_non_expansive (E_Cons ex1 ex2)     = is_non_expansive ex1 && is_non_expansive ex2
  is_non_expansive (E_List exs)         = all is_non_expansive exs
  is_non_expansive (E_Tuple exs)        = all is_non_expansive exs
  is_non_expansive (E_Fun ps ex)        = True
  is_non_expansive (E_Function pm)      = True
  is_non_expansive (E_LetRec lrb ex)    = is_non_expansive ex
