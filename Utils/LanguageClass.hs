{-# LANGUAGE
  FlexibleContexts,
  MultiParamTypeClasses
  #-}

module Utils.LanguageClass where
  import Utils.Env
  import Utils.Subst

  import Control.Monad.Error
  import Text.Parsec.Error

  class Program p where
  class Value v where
  class Show tp => Type tp where
    isVar          :: tp -> Bool
    canCompare     :: tp -> Bool
    getVar         :: tp -> String
    canUnify       :: tp -> tp -> Bool
    newConstraints :: tp -> tp -> [(tp, tp)]
    applySubst     :: tp -> Subst tp -> tp

  class (Program p, Type tp, Value v) => Language p tp v where
    typeOf  :: Env tp -> p -> Either String tp
    eval    :: Env v  -> p -> Either String  v
    parser  :: String -> Either ParseError p
