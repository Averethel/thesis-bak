module Compiler.Compiler where
  import Compiler.Passes.EnforceUniqueNames
  import Compiler.Passes.EliminateWildcards
  import Compiler.Passes.FoldTuples
  import Compiler.Passes.SimplifyPatternMatchings
  import Compiler.Passes.EliminateRedundantIfs
  import Compiler.Passes.TranslationToEL

  import Languages.MiniML.PrettyPrint

  compile = 
    program_to_enriched_lambda .
    eliminate_redundant_ifs .
    simplify_pattern_matchings .
    fold_tuples . 
    eliminate_wildcards .
    enforce_unique_names
