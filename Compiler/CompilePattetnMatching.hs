module Compiler.CompilePattetnMatching where
  import Compiler.Passes.SimplifyLetBindings
  import Compiler.Passes.ConstantMatchingsToGuards
  import Compiler.Passes.FunctionsToFatbars
  import Compiler.Passes.FatbarsToCases

  compilePatternMatching =
    fatbarsToCases .
    functionsToFatbars .
    programToGuardedPatterns .
    letBindingsToDeclarations
