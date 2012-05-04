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
                             "head", "tail"],
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

  preExpression :: Parser Expr
  preExpression = choice [try $ parens $ expression, pVar, pConst, pITE, pList, pPair, pLet, pLetrec, pFun, pMF] where
    pVar    = E_Var <$> identifier
    pConst  = E_Const <$> constant
    pITE    = E_ITE <$> (reserved "if" *> expression) <*> (reserved "then" *> expression) <*> (reserved "else" *> expression)
    pList   = foldr E_Cons (E_Const C_Nil) <$> (brackets . commaSep $ expression)
    pPair   = E_Pair <$> (reservedOp "(" *> expression) <*> (reservedOp "," *> expression <* reservedOp ")")
    pLet    = E_Let <$> (reserved "let" *> identifier) <*> (reservedOp "=" *> expression) <*> (reserved "in" *> expression)
    pLetrec = E_Letrec <$> (reserved "letrec" *> identifier) <*> (reservedOp "=" *> expression) <*> (reserved "in" *> expression)
    pFun    = E_Function <$> (reserved "function" *> identifier) <*> (reservedOp "->" *> expression)
    pMF     = const E_MatchFailure <$> reserved "MatchFailure"

  expression :: Parser Expr
  expression = buildExpressionParser [
                [Prefix (reserved "not" *> pure E_Not)],
                [Prefix ((reserved "ref" <|> reservedOp "!") *> pure E_Ref)],
                [Prefix (reservedOp "&" *> pure E_Deref)],
                [Prefix (reserved "head" *> pure E_Head)],
                [Prefix (reserved "tail" *> pure E_Tail)],
                [Prefix (reserved "fst" *> pure E_Fst)],
                [Prefix (reserved "snd" *> pure E_Snd)],
                [Infix (reservedOp "==" *> pure E_Eq) AssocNone],
                [Infix (reservedOp ":=" *> pure E_Assign) AssocNone],
                [Infix (reservedOp "+" *> pure E_Plus) AssocLeft],
                [Infix (reservedOp "-" *> pure E_Minus) AssocLeft],
                [Infix (reservedOp "*" *> pure E_Mult) AssocLeft],
                [Infix (reservedOp "/" *> pure E_Div) AssocLeft],
                [Infix (reservedOp "@" *> pure E_Apply) AssocLeft],
                [Infix (reservedOp "::" *> pure E_Cons) AssocRight],
                [Infix (reservedOp ";" *> pure E_Seq) AssocRight]
                ] preExpression

  runPp :: Parser a -> String -> Either ParseError a
  runPp p = parse (PTok.whiteSpace lang  >> p) ""

  expressionParser :: String -> Either ParseError Expr
  expressionParser = runPp (expression <* reservedOp ";;")
