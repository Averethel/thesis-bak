module Languages.MiniML.Parser (inputParser, program, expressionParser) where
  import Languages.MiniML.Syntax
  import Languages.MiniML.PrettyPrint

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
    PTok.opStart         = oneOf "[(!&-=+/*:|;~",
    PTok.opLetter        = oneOf "])!&-=+/*:|;>",
    PTok.reservedNames   = [ "true", "false",
                             "not", "ref",
                             "and", "or",
                             "if", "then",
                             "else", "function",
                             "let", "letrec", "in"],
    PTok.reservedOpNames = [ "[]", "()", "!", "&",
                             "-", "==", "+", "/",
                             "*", ":=", "::", "&&",
                             "||", ";", "=", "@",
                             "|", "~", "->"],
    PTok.caseSensitive   = True }

  lang :: PTok.GenTokenParser String u Identity
  lang = PTok.makeTokenParser langDef

  identifier :: Parser LowercaseIdent
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

  prePattern :: Parser Pattern
  prePattern = choice [pVal, pWild, pConst, pTuple, pList, parens $ pattern] where
    pVal   = P_Val <$> identifier
    pWild  = const P_Wildcard <$> char '_'
    pConst = P_Const <$> constant
    pTuple = P_Tuple <$> (parens . commaSep $ pattern)
    pList  = foldr P_Cons (P_Const C_Nil) <$> (brackets . commaSep $ pattern)

  pattern :: Parser Pattern
  pattern = buildExpressionParser [[Infix (reservedOp "::" *> pure P_Cons) AssocRight]] prePattern

  prePatternMatching :: String -> Parser (Pattern, Expr)
  prePatternMatching s = do
                          p <- pattern
                          reserved s
                          e <- expression
                          return (p, e)

  patternMatching :: String -> Parser [(Pattern, Expr)]
  patternMatching s = do
                      first <- prePatternMatching s
                      rest  <- many (reservedOp "|" *> prePatternMatching s)
                      return $ first:rest

  preLetRec :: Parser (ValueName, [(Pattern, Expr)])
  preLetRec = do
                i <- identifier
                reservedOp "="
                reserved "function"
                pm <- patternMatching "->"
                return (i, pm)
  
  letrecBindings :: Parser [(ValueName, [(Pattern, Expr)])]
  letrecBindings = do 
                    d <- preLetRec
                    ds <- many (reserved "and" *> preLetRec)
                    return $ d:ds

  unaryPrim :: Parser UnaryPrim
  unaryPrim = choice [uNot, uRef, uDeref, uMinus] where
    uNot   = const U_Not <$> reserved "not"
    uRef   = const U_Ref <$> (reserved "ref" <|> reservedOp "!")
    uDeref = const U_Deref <$> reservedOp "&"
    uMinus = const U_I_Minus <$> reservedOp "~"

  binaryPrim :: Parser BinaryPrim
  binaryPrim = choice [bEq, bPlus, bMinus, bMult, bDiv, bAssgn] where
    bEq    = const B_Eq <$> reservedOp "=="
    bPlus  = const B_I_Plus <$> reservedOp "+"
    bMinus = const B_I_Minus <$> reservedOp "-"
    bMult  = const B_I_Mult <$> reservedOp "*"
    bDiv   = const B_I_Div <$> reservedOp "/"
    bAssgn = const B_Assign <$> reservedOp ":="

  preExpression :: Parser Expr
  preExpression = choice [try $ parens expression, eVal, eConst, eList, eTuple, eITE, eFunction, eLet, eLetRec, try eUprim, eBprim] where
    eVal       = E_Val <$> identifier
    eConst     = E_Const <$> constant
    eList      = foldr E_Cons (E_Const C_Nil) <$> (brackets . commaSep $ expression)
    eTuple     = E_Tuple <$> (parens . commaSep $ expression)
    eITE       = E_ITE <$> (reserved "if" *> expression) <*> (reserved "then" *> expression) <*> (reserved "else" *> expression)
    eFunction  = E_Function <$> (reserved "function" *> patternMatching "->")
    eLet       = E_Let <$> (reserved "let" *> prePatternMatching "=") <*> (reserved "in" *> expression)
    eLetRec    = E_LetRec <$> (reserved "letrec" *> letrecBindings) <*> (reserved "in" *> expression)
    eUprim     = E_UPrim <$> (angles $ unaryPrim)
    eBprim     = E_BPrim <$> (angles $ binaryPrim)

  expressionPartApp :: Parser Expr
  expressionPartApp = buildExpressionParser [
                [Postfix (reservedOp "*" *> pure (E_Apply (E_BPrim B_I_Mult)))],
                [Postfix (reservedOp "/" *> pure (E_Apply (E_BPrim B_I_Div)))],
                [Postfix (reservedOp "+" *> pure (E_Apply (E_BPrim B_I_Plus)))],
                [Postfix (reservedOp "-" *> pure (E_Apply (E_BPrim B_I_Minus)))],
                [Postfix (reservedOp "==" *> pure (E_Apply (E_BPrim B_Eq)))],
                [Postfix (reservedOp ":=" *> pure (E_Apply (E_BPrim B_Assign)))]
                ] preExpression

  expressionFullApp :: Parser Expr
  expressionFullApp = buildExpressionParser [
                [Prefix (reserved "not" *> pure (E_Apply (E_UPrim U_Not)))],
                [Prefix ((reserved "ref" <|> reservedOp "!") *> pure (E_Apply (E_UPrim U_Ref)))],
                [Prefix (reservedOp "&" *> pure (E_Apply (E_UPrim U_Deref)))],
                [Prefix (reservedOp "-" *> pure (E_Apply (E_UPrim U_I_Minus)))],
                [Infix (reservedOp "@" *> pure E_Apply) AssocLeft],
                [Infix (reservedOp "::" *> pure E_Cons) AssocRight],
                [Infix (reservedOp "&&" *> pure E_And) AssocLeft],
                [Infix (reservedOp "||" *> pure E_Or) AssocLeft],
                [Infix (reservedOp "*" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_I_Mult) a) b)) AssocLeft],
                [Infix (reservedOp "/" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_I_Div) a) b)) AssocLeft],
                [Infix (reservedOp "+" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_I_Plus) a) b)) AssocLeft],
                [Infix (reservedOp "-" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_I_Minus) a) b)) AssocLeft],
                [Infix (reservedOp "==" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Eq) a) b)) AssocNone],
                [Infix (reservedOp ":=" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Assign) a) b)) AssocNone],
                [Infix (reservedOp ";" *> pure E_Seq) AssocRight]
                ] preExpression

  expression :: Parser Expr
  expression = choice [try expressionFullApp, expressionPartApp]

  definition :: Parser Definition
  definition = choice [dLet, dLetRec] where
    dLet    = D_Let <$> (reserved "let" *> prePatternMatching "=")
    dLetRec = D_LetRec <$> (reserved "letrec" *> letrecBindings)

  instruction :: Parser Instruction
  instruction = choice [try iex, idf] where
    iex = IEX <$> expression <* reservedOp ";;"
    idf = IDF <$> definition <* reservedOp ";;"

  program :: Parser Program
  program = many instruction

  runPp :: Parser a -> String -> Either ParseError a
  runPp p = parse (PTok.whiteSpace lang  >> p) ""

  inputParser :: String -> Either ParseError Instruction
  inputParser = runPp instruction

  expressionParser :: String -> Either ParseError Expr
  expressionParser = runPp (expression <* reservedOp ";;")
