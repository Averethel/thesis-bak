module Languages.MiniML.Parser  where
  import Languages.MiniML.Syntax

  import Utils.RunParser

  import Control.Applicative hiding ((<|>), many)
  import Data.Functor
  import Data.Functor.Identity

  import Text.Parsec.Language
  import Text.Parsec.Prim hiding (runParser)
  import Text.Parsec.Char
  import Text.Parsec.Combinator
  import Text.Parsec.Expr
  import Text.Parsec.Error
  import Text.Parsec.String (parseFromFile)
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
                             "let", "letrec", "in",
                             "fst", "snd", "head",
                             "tail", "empty?"],
    PTok.reservedOpNames = [ "[]", "()", "!", "&",
                             "-", "==", "+", "/",
                             "*", ":=", "::", "&&",
                             "||", ";", "=", "@",
                             "|", "~", "->"],
    PTok.caseSensitive   = True }

  lang :: PTok.GenTokenParser String u Identity
  lang = PTok.makeTokenParser langDef

  identifier :: Parser Name
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

  commaSep1 :: Parser a -> Parser [a]
  commaSep1 = PTok.commaSep1 lang

  semiSep :: Parser a -> Parser [a]
  semiSep = PTok.semiSep lang

  natural :: Parser Integer
  natural = PTok.natural lang

  tuple :: Parser a -> Parser [a]
  tuple parser = do
    e <- parser
    reservedOp ","
    rest <- commaSep1 parser
    return $ e:rest

  constant :: Parser Constant
  constant = choice [pInt, pFalse, pTrue, pNil, pUnit] where
    pInt   = C_Int <$> natural
    pFalse = const C_False <$> reserved "false"
    pTrue  = const C_True <$> reserved "true"
    pNil   = const C_Nil <$> reservedOp "[]"
    pUnit  = const C_Unit <$> reservedOp "()"

  prePattern :: Parser Pattern
  prePattern = choice $ [pVal, pWild, try pConst, try pTuple, pList, parens $ pattern] where
    pVal   = P_Val <$> identifier
    pWild  = const P_Wildcard <$> char '_'
    pConst = P_Const <$> constant
    pTuple = P_Tuple <$> (parens . tuple $ pattern)
    pList  = foldr P_Cons (P_Const C_Nil) <$> (brackets . commaSep $ pattern)

  pattern :: Parser Pattern
  pattern = buildExpressionParser [[Infix (reservedOp "::" *> pure P_Cons) AssocRight]] prePattern

  prePatternMatching :: String -> Parser (Pattern, Expr)
  prePatternMatching s = do
                          p <- pattern
                          reserved s
                          e <- expression
                          return (p, e)

  patternMatching :: String -> Parser [Binding]
  patternMatching s = do
                      first <- prePatternMatching s
                      rest  <- many (reservedOp "|" *> prePatternMatching s)
                      return $ first:rest

  patternMatchingWithGuard :: String -> Parser [FunBinding]
  patternMatchingWithGuard s = do
    pss <- patternMatching s
    return $ map (\(p, e) -> (p, e, E_Const C_True)) pss

  letBindings :: Parser [Binding]
  letBindings = do
                  first <- prePatternMatching "="
                  rest  <- many (reserved "and" *> prePatternMatching "=")
                  return $ first:rest

  preLetRec :: Parser LetRecBinding
  preLetRec = do
                i <- identifier
                reservedOp "="
                e <- expression
                return (i, e)

  letrecBindings :: Parser [LetRecBinding]
  letrecBindings = do
                    d <- preLetRec
                    ds <- many (reserved "and" *> preLetRec)
                    return $ d:ds

  unaryPrim :: Parser UnaryPrim
  unaryPrim = choice [uNot, uFst, uSnd, uHead, uTail, uEmpty, uRef, uDeref, uMinus] where
    uNot   = const U_Not <$> reserved "not"
    uRef   = const U_Ref <$> (reserved "ref" <|> reservedOp "!")
    uDeref = const U_Deref <$> reservedOp "&"
    uMinus = const U_I_Minus <$> reservedOp "~"
    uFst   = const U_Fst <$> reserved "fst"
    uSnd   = const U_Snd <$> reserved "snd"
    uHead  = const U_Head <$> reserved "head"
    uTail  = const U_Tail <$> reserved "tail"
    uEmpty = const U_Empty <$> reserved "empty?"

  binaryPrim :: Parser BinaryPrim
  binaryPrim = choice [bEq, bPlus, bMinus, bMult, bDiv, bAssgn] where
    bEq    = const B_Eq <$> reservedOp "=="
    bPlus  = const B_I_Plus <$> reservedOp "+"
    bMinus = const B_I_Minus <$> reservedOp "-"
    bMult  = const B_I_Mult <$> reservedOp "*"
    bDiv   = const B_I_Div <$> reservedOp "/"
    bAssgn = const B_Assign <$> reservedOp ":="

  preExpression :: Parser Expr
  preExpression = choice $ map try [eUprim, eBprim, eVal, eConst, eList, eTuple, eITE, eFunction, eLet, eLetRec, parens expression] where
    eVal       = E_Val <$> identifier
    eConst     = E_Const <$> constant
    eList      = foldr E_Cons (E_Const C_Nil) <$> (brackets . commaSep $ expression)
    eTuple     = E_Tuple <$> (parens $ tuple expression)
    eITE       = E_ITE <$> (reserved "if" *> expression) <*> (reserved "then" *> expression) <*> (reserved "else" *> expression)
    eFunction  = E_Function <$> (reserved "function" *> patternMatchingWithGuard "->")
    eLet       = E_Let <$> (reserved "let" *> letBindings) <*> (reserved "in" *> expression)
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
                [Prefix (reserved "fst" *> pure (E_Apply (E_UPrim U_Fst)))],
                [Prefix (reserved "snd" *> pure (E_Apply (E_UPrim U_Snd)))],
                [Prefix (reserved "head" *> pure (E_Apply (E_UPrim U_Head)))],
                [Prefix (reserved "tail" *> pure (E_Apply (E_UPrim U_Tail)))],
                [Prefix (reserved "empty?" *> pure (E_Apply (E_UPrim U_Empty)))],
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
    dLet    = D_Let <$> (reserved "let" *> letBindings)
    dLetRec = D_LetRec <$> (reserved "letrec" *> letrecBindings)

  instruction :: Parser Instruction
  instruction = choice [try iex, idf] where
    iex = IEX <$> expression <* reservedOp ";;"
    idf = IDF <$> definition <* reservedOp ";;"

  program :: Parser Program
  program = many instruction

  parseExpression :: String -> Either ParseError Expr
  parseExpression = runParser lang expression

  parseInstruction :: String -> Either ParseError Instruction
  parseInstruction = runParser lang instruction

  parseProgramFromFile :: String -> IO (Either ParseError Program)
  parseProgramFromFile = parseFromFile program
