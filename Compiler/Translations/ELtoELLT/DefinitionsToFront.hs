module Compiler.Translations.ELtoELLT.DefinitionsToFront where
  import Languages.EnrichedLambda.Syntax

  definitions_to_front :: Program -> Program
  definitions_to_front prog = definitions_to_front' prog [] [] where
    definitions_to_front' :: Program -> Program -> Program -> Program
    definitions_to_front' []             dfs exs = reverse dfs ++ reverse exs
    definitions_to_front' (i@(IDF _):ps) dfs exs = definitions_to_front' ps (i:dfs) exs
    definitions_to_front' (i@(IEX _):ps) dfs exs = definitions_to_front' ps dfs $ i:exs
 