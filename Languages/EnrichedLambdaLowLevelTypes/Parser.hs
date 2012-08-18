module Languages.EnrichedLambdaLowLevelTypes.Parser where
  import Languages.EnrichedLambdaLowLevelTypes.Syntax
  import Languages.EnrichedLambdaLowLevelTypes.PrettyPrint
  
  import Control.Applicative hiding ((<|>), many)
  import Data.Functor
  import Data.Functor.Identity

  import Text.Parsec.Language
  import Text.Parsec.Prim
  import Text.Parsec.Char
  import Text.Parsec.Combinator
  import Text.Parsec.Expr
  import Text.Parsec.Error
  import qualified Text.Parsec.Token as PTok

  type Parser = ParsecT String () Identity

  langDef :: GenLanguageDef String u Identity
  langDef = haskellStyle {
    PTok.commentStart    = "(*",
    PTok.commentEnd      = "*)",
    PTok.commentLine     = "#",
    PTok.nestedComments  = True,
    PTok.identStart      = lower,
    PTok.identLetter     = alphaNum <|> char '_',
    PTok.opStart         = oneOf "[(!&-=+/*:|;~,",
    PTok.opLetter        = oneOf "])!&-=+/*:|;>",
    PTok.reservedNames   = [ "true", "false",
                             "not", "ref",
                             "and", "or",
                             "if", "then",
                             "else", "function",
                             "let", "letrec", "in", 
                             "MatchFailure",
                             "fst", "snd",
                             "head", "tail", "empty?",
                             "mkPair", "mkList"],
    PTok.reservedOpNames = [ "[]", "()", "!", "&",
                             "-", "==", "+", "/",
                             "*", ":=", "::", "&&",
                             "||", ";", "=", "@",
                             "|", "~", "->", ",",
                             "(", ")"],
    PTok.caseSensitive   = True }

  lang :: PTok.GenTokenParser String u Identity
  lang = PTok.makeTokenParser langDef

  identifier :: Parser String
  identifier = PTok.identifier lang

  reserved :: String -> Parser ()
  reserved = PTok.reserved lang

  reservedOp :: String -> Parser ()
  reservedOp = PTok.reservedOp lang

  parens :: Parser a -> Parser a
  parens = PTok.parens lang

  angles :: Parser a  -> Parser a
  angles = PTok.angles lang

  brackets :: Parser a -> Parser a
  brackets = PTok.brackets  lang

  commaSep :: Parser a -> Parser [a]
  commaSep = PTok.commaSep lang

  semiSep :: Parser a -> Parser [a]
  semiSep = PTok.semiSep lang

  natural :: Parser Integer
  natural = PTok.natural lang

  struct :: Parser Struct
  struct = choice [pInt, pFalse, pTrue, pNil, pUnit] where
    pInt   = S_Int <$> natural
    pFalse = const (S_Str Tg_False 0 []) <$> reserved "false"
    pTrue  = const (S_Str Tg_True 0 []) <$> reserved "true"
    pNil   = const (S_Str Tg_Nil 0 []) <$> reservedOp "[]"
    pUnit  = const (S_Str Tg_Unit 0 []) <$> reservedOp "()"

  prim :: Parser Prim
  prim = choice [uNot, uRef, uDeref, uFst, uSnd, uHead, uTail, bEq, bPlus, bMinus, bMult, bDiv, bAssgn, bMkList, bMkPair] where
    uNot    = const P_Not <$> reserved "not"
    uRef    = const P_Ref <$> (reserved "ref" <|> reservedOp "!")
    uDeref  = const P_Deref <$> reservedOp "&"
    uFst    = const P_Fst <$> reserved "fst"
    uSnd    = const P_Snd <$> reserved "snd"
    uHead   = const P_Head <$> reserved "head"
    uTail   = const P_Tail <$> reserved "tail"
    uEmpty  = const P_Empty <$> reserved "empty?"
    bEq     = const P_Eq <$> reservedOp "=="
    bPlus   = const P_Plus <$> reservedOp "+"
    bMinus  = const P_Minus <$> reservedOp "-"
    bMult   = const P_Mult <$> reservedOp "*"
    bDiv    = const P_Div <$> reservedOp "/"
    bAssgn  = const P_Assign <$> reservedOp ":="
    bMkList = const P_AllocPair <$> reserved "mkPair"
    bMkPair = const P_AllocList <$> reserved "mkList"
  
  preExpression :: Parser Expr
  preExpression = choice [try $ parens $ expression, pVar, pPrim, pStruct, pITE, pList, pPair, pLet, pLetRec, pFun, pMF] where
    pVar    = E_Val <$> identifier
    pPrim   = E_Prim <$> (angles $ prim)
    pStruct = E_Struct <$> struct
    pITE    = E_ITE <$> (reserved "if" *> expression) <*> (reserved "then" *> expression) <*> (reserved "else" *> expression)
    pList   = foldr (\a b -> E_Apply (E_Apply (E_Prim P_AllocList) a) b) (E_Struct (S_Str Tg_Nil 0 [])) <$> (brackets . commaSep $ expression)
    pPair   = (\a b -> E_Apply (E_Apply (E_Prim P_AllocPair) a) b) <$> (reservedOp "(" *> expression) <*> (reservedOp "," *> expression <* reservedOp ")")
    pLet    = E_Let <$> (reserved "let" *> identifier) <*> (reservedOp "=" *> expression) <*> (reserved "in" *> expression)
    pLetRec = E_LetRec <$> (reserved "letrec" *> identifier) <*> (reservedOp "=" *> expression) <*> (reserved "in" *> expression)
    pFun    = E_Function <$> (reserved "function" *> identifier) <*> (reservedOp "->" *> expression)
    pMF     = const E_MatchFailure <$> reserved "MatchFailure"

  expressionPartApp :: Parser Expr
  expressionPartApp = buildExpressionParser [
                [Postfix (reservedOp "*" *> pure (E_Apply (E_Prim P_Mult)))],
                [Postfix (reservedOp "/" *> pure (E_Apply (E_Prim P_Div)))],
                [Postfix (reservedOp "+" *> pure (E_Apply (E_Prim P_Plus)))],
                [Postfix (reservedOp "-" *> pure (E_Apply (E_Prim P_Minus)))],
                [Postfix (reservedOp "==" *> pure (E_Apply (E_Prim P_Eq)))],
                [Postfix (reservedOp ":=" *> pure (E_Apply (E_Prim P_Assign)))]
                ] preExpression

  expressionFullApp :: Parser Expr
  expressionFullApp = buildExpressionParser [
                [Prefix (reserved "not" *> pure (E_Apply (E_Prim P_Not)))],
                [Prefix ((reserved "ref" <|> reservedOp "!") *> pure (E_Apply (E_Prim P_Ref)))],
                [Prefix (reservedOp "&" *> pure (E_Apply (E_Prim P_Deref)))],
                [Prefix (reserved "head" *> pure (E_Apply (E_Prim P_Head)))],
                [Prefix (reserved "tail" *> pure (E_Apply (E_Prim P_Tail)))],
                [Prefix (reserved "fst" *> pure (E_Apply (E_Prim P_Fst)))],
                [Prefix (reserved "snd" *> pure (E_Apply (E_Prim P_Not)))],
                [Prefix (reserved "empty?" *> pure (E_Apply (E_Prim P_Empty)))],
                [Infix (reservedOp "*" *> pure (\a b -> E_Apply (E_Apply (E_Prim P_Mult) a) b)) AssocLeft],
                [Infix (reservedOp "/" *> pure (\a b -> E_Apply (E_Apply (E_Prim P_Div) a) b)) AssocLeft],
                [Infix (reservedOp "+" *> pure (\a b -> E_Apply (E_Apply (E_Prim P_Plus) a) b)) AssocLeft],
                [Infix (reservedOp "-" *> pure (\a b -> E_Apply (E_Apply (E_Prim P_Minus) a) b)) AssocLeft],
                [Infix (reservedOp "==" *> pure (\a b -> E_Apply (E_Apply (E_Prim P_Eq) a) b)) AssocNone],
                [Infix (reservedOp ":=" *> pure (\a b -> E_Apply (E_Apply (E_Prim P_Assign) a) b)) AssocNone],
                [Infix (reservedOp "@" *> pure E_Apply) AssocLeft],
                [Infix ((reservedOp "::" <|> reserved "mkList") *> pure (\a b -> E_Apply (E_Apply (E_Prim P_AllocList) a) b)) AssocRight],
                [Infix (reserved "mkPair" *> pure (\a b -> E_Apply (E_Apply (E_Prim P_AllocPair) a) b)) AssocLeft],
                [Infix (reservedOp ";" *> pure E_Seq) AssocRight]
                ] preExpression

  expression :: Parser Expr
  expression = choice [try expressionFullApp, expressionPartApp]
  
  runPp :: Parser a -> String -> Either ParseError a
  runPp p = parse (PTok.whiteSpace lang  >> p) ""
  
  expressionParser :: String -> Either ParseError Expr
  expressionParser = runPp (expression <* reservedOp ";;")
