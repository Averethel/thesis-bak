module Compiler.Translations.MLtoEL (translate_to_enriched_lambda) where
  import Compiler.Translations.MLtoEL.ExpressionsToDefinitions
  import Compiler.Translations.MLtoEL.UniqueNamesEnforcer
  import Compiler.Translations.MLtoEL.TranslationToEL

  translate_to_enriched_lambda = program_to_enriched_lambda . enforce_unique_names . expressions_to_definitions
