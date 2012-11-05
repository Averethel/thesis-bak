module Compiler.Preprocess where
  import Compiler.Passes.EnforceUniqueNames
  import Compiler.Passes.EliminateWildcards
  import Compiler.Passes.FoldTuples
 
  preprocess = 
    foldTuples . 
    eliminateWildcards .
    enforceUniqueNames
