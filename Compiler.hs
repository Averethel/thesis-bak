module Compiler where
  import Compiler.Preprocess
  import Compiler.CompilePattetnMatching
  import Compiler.TranslationToEL

  compile = 
    programToEnrichedLambda .
    compilePatternMatching .
    preprocess
