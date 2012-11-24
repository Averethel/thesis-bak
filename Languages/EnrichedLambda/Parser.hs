module Languages.EnrichedLambda.Parser where
  import Languages.EnrichedLambda.Syntax

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
    PTok.opStart         = oneOf "[(!&-=+/*:|;",
    PTok.opLetter        = oneOf "])!&-=+/*:|;>",
    PTok.reservedNames   = [ "true", "false",
                             "not", "ref",
                             "and", "or",
                             "if", "then", "case",
                             "else", "function",
                             "let", "letrec", "in",
                             "fst", "snd", "head",
                             "tail", "empty?"],
    PTok.reservedOpNames = [ "[]", "()", "!", "&",
                             "-", "==", "+", "/",
                             "*", ":=", "::", "&&",
                             "||", ";", "=", "@",
                             "|", "->", ","],
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

  typeTag :: Parser TypeTag
  typeTag = do { i <- many1 digit; return $ read i }

  constrTag :: Parser ConstrTag
  constrTag = do { i <- many1 digit; return $ read i }

  arity :: Parser Arity
  arity = do { i <- many1 digit; return $ read i }

  unaryPrim :: Parser UnaryPrim
  unaryPrim = choice [uNot, uFst, uSnd, uHead, uTail, uEmpty, uRef, uDeref] where
    uNot   = const U_Not <$> reserved "not"
    uRef   = const U_Ref <$> (reserved "ref" <|> reservedOp "!")
    uDeref = const U_Deref <$> reservedOp "&"
    uFst   = const U_Fst <$> reserved "fst"
    uSnd   = const U_Snd <$> reserved "snd"
    uHead  = const U_Head <$> reserved "head"
    uTail  = const U_Tail <$> reserved "tail"
    uEmpty = const U_Empty <$> reserved "empty?"

  binaryPrim :: Parser BinaryPrim
  binaryPrim = choice [bEq, bPlus, bMinus, bMult, bDiv, bAssgn, bAnd, bOr] where
    bEq    = const B_Eq <$> reservedOp "=="
    bPlus  = const B_Plus <$> reservedOp "+"
    bMinus = const B_Minus <$> reservedOp "-"
    bMult  = const B_Mult <$> reservedOp "*"
    bDiv   = const B_Div <$> reservedOp "/"
    bAssgn = const B_Assign <$> reservedOp ":="
    bAnd   = const B_And <$> reserved "and"
    bOr    = const B_Or <$> reserved "or"

  eFunction :: Parser Expr
  eFunction  = E_Function <$> (reserved "function" *> identifier <* reservedOp "=") <*> expression

  letBinding :: Parser Binding
  letBinding = do
    i <- identifier
    reservedOp "="
    e <- expression
    return (i, e)

  letBindings :: Parser [Binding]
  letBindings = do
    b  <- letBinding
    bs <- many (reserved "and" *> letBinding)
    return $ b:bs

  letrecBinding :: Parser Binding
  letrecBinding = do
    i <- identifier
    reservedOp "="
    f <- eFunction
    return (i, f)

  letrecBindings :: Parser [Binding]
  letrecBindings = do
    b <- letBinding
    bs <- many (reserved "and" *> letrecBinding)
    return $ b:bs

  clause :: Parser Clause
  clause = choice $ map try [cTrue, cFalse, cUnit, cNil, cCons, cPair, cCl] where
    cTrue  = (\e ->       (boolTag, trueTag,  [],    e)) <$> (reserved "true" *> reservedOp "->" *> expression)
    cFalse = (\e ->       (boolTag, falseTag, [],    e)) <$> (reserved "false" *> reservedOp "->" *> expression)
    cUnit  = (\e ->       (unitTag, unitTagC, [],    e)) <$> (reservedOp "()" *> reservedOp "->" *> expression)
    cNil   = (\e ->       (listTag, nilTag,   [],    e)) <$> (reservedOp "[]" *> reservedOp "->" *> expression)
    cCons  = (\(a,b,e) -> (listTag, consTag,  [a,b], e)) <$> do { i <- identifier; reservedOp "::"; i' <- identifier; reservedOp "->"; e <- expression; return  (i, i', e)}
    cPair  = (\(a,b,e) -> (pairTag, pairTagC, [a,b], e)) <$> do { reservedOp "("; i <- identifier; reservedOp ","; i' <- identifier; reservedOp ")"; reservedOp "->"; e <- expression; return (i, i', e)}
    cCl    = do
      reservedOp "<"
      tt <- typeTag
      reservedOp ","
      ct <- constrTag
      reservedOp ">"
      ns <- many identifier
      reservedOp "->"
      e <- expression
      return (tt, ct, ns, e)

  clauses :: Parser [Clause]
  clauses = do
    c <- clause
    cs <- many (reservedOp "|" *> clause)
    return $ c:cs

  preExpression :: Parser Expr
  preExpression = choice $ map try [eUprim, eBprim, eVal, eNum, eList, ePair, eConstr, eCase, eFunction, eLet, eLetRec, parens expression] where
    eUprim     = E_UPrim <$> (angles $ unaryPrim)
    eBprim     = E_BPrim <$> (angles $ binaryPrim)
    eVal       = E_Val <$> identifier
    eNum       = E_Num <$> natural
    eList      = foldr (\a -> \b -> E_Apply (E_Apply (E_Constr listTag consTag 2) a) b) (E_Constr listTag nilTag 0) <$> (brackets . commaSep $ expression)
    ePair      = parens $ do { a <- expression; reservedOp ","; e2 <- expression; return $ E_Apply (E_Apply (E_Constr pairTag pairTagC 2) a) e2 }
    eConstr    = choice $ map try [cTrue, cFalse, cUnit, cNil, cCons, cPair, cPack]
    cTrue      = const (E_Constr boolTag trueTag 0) <$> reserved "true"
    cFalse     = const (E_Constr boolTag falseTag 0) <$> reserved "false"
    cUnit      = const (E_Constr unitTag unitTagC 0) <$> reservedOp "()"
    cNil       = const (E_Constr listTag nilTag 0) <$> reservedOp "[]"
    cCons      = const (E_Constr listTag consTag 2) <$> (angles $ reservedOp "::")
    cPair      = const (E_Constr pairTag pairTagC 2) <$> (angles $ reservedOp ",")
    cPack      = (\(t, c, a) -> E_Constr t c a) <$> do { reserved "Pack"; reservedOp "{"; t <- typeTag; reservedOp ","; c <- constrTag; reservedOp ","; a <- arity; reservedOp "}"; return (t, c, a)}
    eCase      = E_Case <$> (reserved "case" *> expression) <*> (reserved "of" *> clauses)
    eLet       = E_Let <$> (reserved "let" *> letBindings) <*> (reserved "in" *> expression)
    eLetRec    = E_LetRec <$> (reserved "letrec" *> letrecBindings) <*> (reserved "in" *> expression)

  expressionPartApp :: Parser Expr
  expressionPartApp = buildExpressionParser [
                [Postfix (reservedOp "*" *> pure (E_Apply (E_BPrim B_Mult)))],
                [Postfix (reservedOp "/" *> pure (E_Apply (E_BPrim B_Div)))],
                [Postfix (reservedOp "+" *> pure (E_Apply (E_BPrim B_Plus)))],
                [Postfix (reservedOp "-" *> pure (E_Apply (E_BPrim B_Minus)))],
                [Postfix (reservedOp "==" *> pure (E_Apply (E_BPrim B_Eq)))],
                [Postfix (reservedOp ":=" *> pure (E_Apply (E_BPrim B_Assign)))],
                [Postfix (reservedOp "&&" *> pure (E_Apply (E_BPrim B_And)))],
                [Postfix (reservedOp "||" *> pure (E_Apply (E_BPrim B_Or)))]
                ] preExpression

  expressionFullApp :: Parser Expr
  expressionFullApp = buildExpressionParser [
                [Prefix (reserved "not" *> pure (E_Apply (E_UPrim U_Not)))],
                [Prefix ((reserved "ref" <|> reservedOp "!") *> pure (E_Apply (E_UPrim U_Ref)))],
                [Prefix (reservedOp "&" *> pure (E_Apply (E_UPrim U_Deref)))],
                [Prefix (reserved "fst" *> pure (E_Apply (E_UPrim U_Fst)))],
                [Prefix (reserved "snd" *> pure (E_Apply (E_UPrim U_Snd)))],
                [Prefix (reserved "head" *> pure (E_Apply (E_UPrim U_Head)))],
                [Prefix (reserved "tail" *> pure (E_Apply (E_UPrim U_Tail)))],
                [Prefix (reserved "empty?" *> pure (E_Apply (E_UPrim U_Empty)))],
                [Infix (reservedOp "@" *> pure E_Apply) AssocLeft],
                [Infix (reservedOp "::" *> pure (\a b -> E_Apply (E_Apply (E_Constr listTag consTag 2) a) b)) AssocRight],
                [Infix (reservedOp "&&" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_And) a) b)) AssocRight],
                [Infix (reservedOp "||" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Or) a) b)) AssocRight],
                [Infix (reservedOp "*" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Mult) a) b)) AssocLeft],
                [Infix (reservedOp "/" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Div) a) b)) AssocLeft],
                [Infix (reservedOp "+" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Plus) a) b)) AssocLeft],
                [Infix (reservedOp "-" *> pure (\a b -> E_Apply (E_Apply (E_BPrim B_Minus) a) b)) AssocLeft],
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
  instruction = choice $ map try [iDf, iEx] where
    iDf = IDF <$> definition
    iEx = IEX <$> expression

  program :: Parser Program
  program = do
    dfs <- many definition
    e   <- expression
    return (dfs, e)

  parseExpression :: String -> Either ParseError Expr
  parseExpression = runParser lang expression

  parseInstruction :: String -> Either ParseError Instruction
  parseInstruction = runParser lang instruction

  parseProgramFromFile :: String -> IO (Either ParseError Program)
  parseProgramFromFile = parseFromFile program



