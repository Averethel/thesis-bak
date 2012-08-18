module Compiler.Translations.ELtoELLT where
  import Compiler.Translations.ELtoELLT.DefinitionsToFront
  import Compiler.Translations.ELtoELLT.TranslationToELLT

  translate_to_low_level_lambda = program_to_low_level_lambda . definitions_to_front