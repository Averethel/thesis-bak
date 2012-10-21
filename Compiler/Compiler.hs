module Compiler.Compiler where
  import Compiler.Passes.EnforceUniqueNames
  import Compiler.Passes.ExpressionsToDefinitions
  import Compiler.Passes.EliminateWildcards
  import Compiler.Passes.FoldTuples
  import Compiler.Passes.SimplifyPatternMatchings
  --import Compiler.Passes.TranslationToEL

  import Languages.MiniML.PrettyPrint

  compile = 
    simplify_pattern_matchings .
    fold_tuples . 
    eliminate_wildcards .
    translate_expressions_to_definitions . 
    enforce_unique_names
