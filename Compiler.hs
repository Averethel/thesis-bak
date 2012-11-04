module Compiler where
  import Compiler.Preprocess
  import Compiler.CompilePattetnMatching

  compile = 
    compile_pattern_matching .
    preprocess
