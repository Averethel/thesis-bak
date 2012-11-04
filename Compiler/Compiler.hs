module Compiler.Compiler where
  import Compiler.Passes.EnforceUniqueNames
  import Compiler.Passes.EliminateWildcards
  import Compiler.Passes.FoldTuples
  import Compiler.Passes.SimplifyLetBindings
  import Compiler.Passes.ConstantMatchingsToGuards
  import Compiler.Passes.FunctionsToFatbars

  import Languages.MiniML.Syntax
  import Languages.MiniML.PrettyPrint

  compile = 
    functions_to_fatbars .
    program_to_guarded_patterns .
    let_bindings_to_declarations .
    fold_tuples . 
    eliminate_wildcards .
    enforce_unique_names
