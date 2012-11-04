module Compiler.CompilePattetnMatching where
  import Compiler.Passes.SimplifyLetBindings
  import Compiler.Passes.ConstantMatchingsToGuards
  import Compiler.Passes.FunctionsToFatbars
  import Compiler.Passes.FatbarsToCases

  compile_pattern_matching =
    fatbars_to_cases .
    functions_to_fatbars .
    program_to_guarded_patterns .
    let_bindings_to_declarations
