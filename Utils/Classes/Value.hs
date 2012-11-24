module Utils.Classes.Value where
  class Show v => Value v where
    nullValue :: v
