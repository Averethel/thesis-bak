module Compiler where
  import Compiler.Preprocess
  import Compiler.CompilePattetnMatching
  import Compiler.TranslationToEL

  compile = 
    program_to_enriched_lambda .
    compile_pattern_matching .
    preprocess
