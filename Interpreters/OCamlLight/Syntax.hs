module Interpreters.OCamlLight.Syntax where
  import Data.Natural

  type Index = Natural
  type Ident = Natural

  type IntegerLiteral = Integer
  type FloatLiteral   = Float
  type CharLiteral    = Char
  type StringLiteral  = String

  type InfixSymbol  = String
  type PrefixSymbol = String
  type Location     = Natural

  type LowercaseIdent   = String
  type CapitalizedIdent = String

  data TypeconstrName = 
      TCN_Id LowercaseIdent
    deriving (Show, Eq)

  data Typeconstr =
      TC_Name TypeconstrName
    | TC_Int
    | TC_Char
    | TC_String
    | TC_Float
    | TC_Bool
    | TC_Unit
    | TC_Exn
    | TC_List
    | TC_Option
    | TC_Ref
    deriving (Show, Eq)

  type Idx = Natural

  data TypeVar =
      TV_Ident Ident
    deriving (Show, Eq)

  data ConstrName =
      CN_Id CapitalizedIdent
    deriving (Show, Eq)

  data TypeExpr = 
      TE_Var TypeVar
    | TE_IdxVar Idx Idx
    | TE_Any
    | TE_Arrow TypeExpr TypeExpr
    | TE_Tuple [TypeExpr]
    | TE_Constr [TypeExpr] Typeconstr
    deriving (Show, Eq)

  data FieldName =
      FN_Id LowercaseIdent
    deriving (Show, Eq)

  data InfixOp =
      IO_Symbol InfixSymbol
    | IO_Star
    | IO_Equal
    | IO_ColonEqual
    deriving (Show, Eq)

  data ConstrDecl =
      CD_NullAry ConstrName
    | CD_NAry ConstrName [TypeExpr]
    deriving (Show, Eq)

  data FieldDecl =
      FD_Immutable FieldName TypeExpr
    deriving (Show, Eq)

  data OperatorName =
      ON_Symbol PrefixSymbol
    | ON_Infix InfixOp
    deriving (Show, Eq)

  data Constr =
      C_Name ConstrName
    | C_InvalidArgument
    | C_NotFound
    | C_AssertFailure
    | C_Div_By_0
    | C_None
    | C_Some
    deriving (Show, Eq)

  type Intn = IntegerLiteral

  data TypeEquation =
      TE_Te TypeExpr
    deriving (Show, Eq)

  data TypeRepresentation =
      TR_Variant [ConstrDecl]
    | TR_Record [FieldDecl]
    deriving (Show, Eq)

  data TypeParam =
      TP_Var TypeVar
    deriving (Show, Eq)

  data ValueName =
      VN_Id LowercaseIdent
    | VN_Op OperatorName
    deriving (Show, Eq)

  data Field =
      F_Name FieldName
    deriving (Show, Eq)

  data Constant =
      CONST_Int Intn
    | CONST_Float FloatLiteral
    | CONST_Char CharLiteral
    | CONST_String StringLiteral
    | CONST_Constr Constr
    | CONST_False
    | CONST_True
    | CONST_Nil
    | CONST_Unit
    deriving (Show, Eq)

  data TypeInformation = 
      TI_Eq TypeEquation
    | TI_Def TypeRepresentation
    deriving (Show, Eq)

  type TypeParamsOpt = [TypeParam]

  data Pattern =
      P_Var ValueName
    | P_Any
    | P_Constant Constant
    | P_Alias Pattern ValueName
    | P_Typed Pattern TypeExpr
    | P_Or Pattern Pattern
    | P_Construct Constr [Pattern]
    | P_ConstructorAny Constr
    | P_Tuple [Pattern]
    | P_Record [(Field, Pattern)]
    | P_Cons Pattern Pattern
    deriving (Show, Eq)

  data UnaryPrim =
      UPrim_Raise
    | UPrim_Not
    | UPrim_Minus
    | UPrim_Ref
    | UPrim_Deref
    deriving (Show, Eq)

  data BinaryPrim =
      BPrim_Equal
    | BPrim_Plus
    | BPrim_Minus
    | BPrim_Times
    | BPrim_Div
    | BPrim_Assign
    deriving (Show, Eq)

  data ForDirn =
      FD_Upto
    | FD_Downto
    deriving (Show, Eq)

  data Typedef =
      TD_Td TypeParamsOpt TypeconstrName TypeInformation
    deriving (Show, Eq)

  data LetrecBinding =
      LRB_Simple ValueName PatternMatching
    deriving (Show, Eq)

  type LetrecBindings = [LetrecBinding]

  data LetBinding =
      LB_Simple Pattern Expr
    deriving (Show, Eq)

  data PatExp =
      PE_Inj Pattern Expr
    deriving (Show, Eq)

  data PatternMatching = 
      PM_Pm [PatExp]
    deriving (Show, Eq)

  data Expr =
      Expr_Uprim UnaryPrim
    | Expr_Bprim BinaryPrim
    | Expr_Ident ValueName
    | Expr_Constant Constant
    | Expr_Typed Expr TypeExpr
    | Expr_Tuple [Expr]
    | Expr_Construct Constr [Expr]
    | Expr_Cons Expr Expr
    | Expr_Record [(Field, Expr)]
    | Expr_Override Expr [(Field, Expr)]
    | Expr_Apply Expr Expr
    | Expr_And Expr Expr
    | Expr_Or Expr Expr
    | Expr_Field Expr Field
    | Expr_IfThenElse Expr Expr Expr
    | Expr_While Expr Expr
    | Expr_For ValueName Expr ForDirn Expr Expr
    | Expr_Sequence Expr Expr
    | Expr_Match Expr PatternMatching
    | Expr_Function PatternMatching
    | Expr_Try Expr PatternMatching
    | Expr_Let LetBinding Expr
    | Expr_Letrec LetrecBindings Expr
    | Expr_Assert Expr
    | Expr_Location Location
    deriving (Show, Eq)

  type TypeDefinition = [Typedef]

  data ExceptionDefinition =
      ED_Def ConstrDecl
    deriving (Show, Eq)

  data Definition =
      D_Let LetBinding
    | D_Letrec LetrecBindings
    | D_Type TypeDefinition
    | D_Exception ExceptionDefinition
    deriving (Show, Eq)

  data TypeScheme =
      TS_Forall TypeExpr
    deriving (Show, Eq)

  type Kind = Natural

  type TypeExprs = [TypeExpr]

  data TransLabel =
      Lab_Nil
    | Lab_Alloc Expr Location
    | Lab_Deref Location Expr
    | Lab_Assign Location Expr
    deriving (Show, Eq)

  type Definitions = [Definition]

  data Name =
      Name_TV
    | Name_VN ValueName
    | Name_CN ConstrName
    | Name_FN FieldName
    | Name_L Location
    deriving (Show, Eq)

  data EnvironmentBinding =
      EB_TV
    | EB_VN ValueName TypeScheme
    | EB_CC ConstrName Typeconstr
    | EB_PC ConstrName TypeParamsOpt TypeExprs Typeconstr
    | EB_FN FieldName  TypeParamsOpt TypeExpr Typeconstr
    | EB_TD TypeconstrName Kind
    | EB_TR TypeconstrName Kind [FieldName]
    | EB_TA TypeParamsOpt TypeconstrName TypeExpr
    | EB_L Location TypeExpr
    deriving (Show, Eq)

  type LabelledArrow = TransLabel

  data Program = 
      Prog_Defs Definitions
    | Prog_Raise Expr
    deriving (Show, Eq)

  type Store = [(Location, Expr)]

  type Names = [Name]

  type Environment = [EnvironmentBinding]

  type TSigma = [(TypeVar, TypeExpr)]

  type SubstsX = [(ValueName, Expr)]

  data ValuePath =
      VP_Name ValueName
    deriving (Show, Eq)
