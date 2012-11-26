{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.RunParser where
  import Data.Functor.Identity

  import Text.Parsec.Error
  import Text.Parsec.Prim
  import Text.Parsec.Token

  runParser :: Stream s Identity t => GenTokenParser s () Identity -> ParsecT s () Identity a -> s -> Either ParseError a
  runParser lang p = parse (whiteSpace lang  >> p) ""
