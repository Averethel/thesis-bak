module Compiler.Preprocess where
  import Compiler.Passes.EnforceUniqueNames
  import Compiler.Passes.EliminateWildcards
  import Compiler.Passes.FoldTuples
 
  preprocess = 
    fold_tuples . 
    eliminate_wildcards .
    enforce_unique_names
