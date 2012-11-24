module Utils.Classes.Type where
  import Utils.Subst

  class Show tp => Type tp where
    isVar          :: tp -> Bool
    canCompare     :: tp -> Bool
    getVar         :: tp -> String
    canUnify       :: tp -> tp -> Bool
    newConstraints :: tp -> tp -> [(tp, tp)]
    applySubst     :: tp -> Subst tp -> tp