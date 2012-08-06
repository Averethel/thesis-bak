module Languages.EnrichedLambda.Parser (expressionParser) where
  import Languages.EnrichedLambda.Syntax
  import Languages.EnrichedLambda.PrettyPrint
  
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
                             "head", "tail", "empty?"],
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

  constant :: Parser Constant
  constant = choice [pInt, pFalse, pTrue, pNil, pUnit] where
    pInt   = C_Int <$> natural
    pFalse = const C_False <$> reserved "false"
    pTrue  = const C_True <$> reserved "true"
    pNil   = const C_Nil <$> reservedOp "[]"
    pUnit  = const C_Unit <$> reservedOp "()"

  unaryPrim :: Parser UnaryPrim
  unaryPrim = choice [uNot, uRef, uDeref, uFst, uSnd, uHead, uTail] where
    uNot   = const U_Not <$> reserved "not"
    uRef   = const U_Ref <$> (reserved "ref" <|> reservedOp "!")
    uDeref = const U_Deref <$> reservedOp "&"
    uFst   = const U_Fst <$> reserved "fst"
    uSnd   = const U_Snd <$> reserved "snd"
    uHead  = const U_Head <$> reserved "head"
    uTail  = const U_Tail <$> reserved "tail"
    uEmpty = const U_Empty <$> reserved "empty?"

  binaryPrim :: Parser BinaryPrim
  binaryPrim = choice [bEq, bPlus, bMinus, bMult, bDiv, bAssgn] where
    bEq    = const B_Eq <$> reservedOp "=="
    bPlus  = const B_Plus <$> reservedOp "+"
    bMinus = const B_Minus <$> reservedOp "-"
    bMult  = const B_Mult <$> reservedOp "*"
    bDiv   = const B_Div <$> reservedOp "/"
    bAssgn = const B_Assign <$> reservedOp ":="

  preExpression :: Parser Expr
  preExpression = choice [try $ parens $ expression, pVal, pUprim, pBprim, pConst, pITE, pList, pPair, pLet, pLetrec, pFun, pMF] where
    pVal    = E_Val <$> identifier
    pUprim  = E_UPrim <$> (angles $ unaryPrim)
    pBprim  = E_BPrim <$> (angles $ binaryPrim)
    pConst  = E_Const <$> constant
    pITE    = E_ITE <$> (reserved "if" *> expression) <*> (reserved "then" *> expression) <*> (reserved "else" *> expression)
    pList   = foldr E_Cons (E_Const C_Nil) <$> (brackets . commaSep $ expression)
    pPair   = E_Pair <$> (reservedOp "(" *> expression) <*> (reservedOp "," *> expression <* reservedOp ")")
    pLet    = E_Let <$> (reserved "let" *> identifier) <*> (reservedOp "=" *> expression) <*> (reserved "in" *> expression)
    pLetrec = E_Letrec <$> (reserved "letrec" *> identifier) <*> (reservedOp "=" *> expression) <*> (reserved "in" *> expression)
    pFun    = E_Function <$> (reserved "function" *> identifier) <*> (reservedOp "->" *> expression)
    pMF     = const E_MatchFailure <$> reserved "MatchFailure"

  expressionPartApp :: Parser Expr
  expressionPartApp = buildExpressionParser [
                [Postfix (reservedOp "*" *> pure (E_Apply (E_BPrim B_Mult)))],
                [Postfix (reservedOp "/" *> pure (E_Apply (E_BPrim B_Div)))],
                [Postfix (reservedOp "+" *> pure (E_Apply (E_BPrim B_Plus)))],
                [Postfix (reservedOp "-" *> pure (E_Apply (E_BPrim B_Minus)))],
                [Postfix (reservedOp "==" *> pure (E_Apply (E_BPrim B_Eq)))],
                [Postfix (reservedOp ":=" *> pure (E_Apply (E_BPrim B_Assign)))]
                ] preExpression

  expressionFullApp :: Parser Expr
  expressionFullApp = buildExpressionParser [
                [Prefix (reserved "not" *> pure (E_Apply (E_UPrim U_Not)))],
                [Prefix ((reserved "ref" <|> reservedOp "!") *> pure (E_Apply (E_UPrim U_Ref)))],
                [Prefix (reservedOp "&" *> pure (E_Apply (E_UPrim U_Deref)))],
                [Prefix (reserved "head" *> pure (E_Apply (E_UPrim U_Head)))],
                [Prefix (reserved "tail" *> pure (E_Apply (E_UPrim U_Tail)))],
                [Prefix (reserved "fst" *> pure (E_Apply (E_UPrim U_Fst)))],
                [Prefix (reserved "snd" *> pure (E_Apply (E_UPrim U_Not)))],
                [Prefix (reserved "empty?" *> pure (E_Apply (E_UPrim U_Empty)))],
                [Infix (reservedOp "*" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Mult) a) b)) AssocLeft],
                [Infix (reservedOp "/" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Div) a) b)) AssocLeft],
                [Infix (reservedOp "+" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Plus) a) b)) AssocLeft],
                [Infix (reservedOp "-" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Minus) a) b)) AssocLeft],
                [Infix (reservedOp "==" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Eq) a) b)) AssocNone],
                [Infix (reservedOp ":=" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Assign) a) b)) AssocNone],
                [Infix (reservedOp "@" *> pure E_Apply) AssocLeft],
                [Infix (reservedOp "::" *> pure E_Cons) AssocRight],
                [Infix (reservedOp ";" *> pure E_Seq) AssocRight]
                ] preExpression

  expression :: Parser Expr
  expression = choice [try expressionFullApp, expressionPartApp]

  runPp :: Parser a -> String -> Either ParseError a
  runPp p = parse (PTok.whiteSpace lang  >> p) ""

  expressionParser :: String -> Either ParseError Expr
  expressionParser = runPp (expression <* reservedOp ";;")
