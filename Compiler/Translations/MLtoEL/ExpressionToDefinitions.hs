module Compiler.Translations.MLtoEl.ExpressionsToDefinitions where
  import Languages.MiniML.Syntax

  expressions_to_definitions :: Program -> Program
  expressions_to_definitions prog = reverse $ expressions_to_definitions_back $ reverse prog where
    expressions_to_definitions_back  []               = []
    expressions_to_definitions_back  ((IDF c) : rest) = (IDF c) : expressions_to_definitions_back rest
    expressions_to_definitions_back  ((IEX e) : rest) = (IEX c) : expressions_to_definitions_front rest
    expressions_to_definitions_front []               = []
    expressions_to_definitions_front ((IDF c) : rest) = (IDF c) : expressions_to_definitions_front rest
    expressions_to_definitions_front ((IEX e) : rest) = (IDF $ D_Let "it" e) : expressions_to_definitions_front rest
