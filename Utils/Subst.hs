module Utils.Subst where
  type Subst t = [(String, t)]

  emptySubst :: Subst t
  emptySubst = []

  composeSubst :: (String, t) -> Subst t -> Subst t
  composeSubst n s = s ++ [n]