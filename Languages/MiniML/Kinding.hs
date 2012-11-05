{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Kinding (kindOfTypeExpr) where
  import Utils.Errors

  import Languages.MiniML.State
  import Languages.MiniML.Syntax
  import Languages.MiniML.PrettyPrint

  import Control.Monad.Error
  import Control.Monad.State

  kindOfTypeConstr :: (MonadState InterpreterState m, MonadError String m) => TypeConstr -> m Kind
  kindOfTypeConstr Int   =
    return K_Type
  kindOfTypeConstr Bool  =
    return K_Type
  kindOfTypeConstr Unit  =
    return K_Type
  kindOfTypeConstr List  =
    return $ K_Arrow K_Type K_Type
  kindOfTypeConstr Ref   =
    return $ K_Arrow K_Type K_Type

  kindOfTypeExpr :: (MonadState InterpreterState m, MonadError String m) => TypeExpr -> m Kind
  kindOfTypeExpr (TE_Var _)           = 
    return K_Type
  kindOfTypeExpr te@(TE_Arrow e1 e2)  = do
    k1 <- kindOfTypeExpr e1
    k2 <- kindOfTypeExpr e2
    case (k1, k2) of
      (K_Type, K_Type) -> return K_Type
      (K_Type, k2)     -> throwError $ invalid "kind" te K_Type (e2, k2)
      (k1,     _)      -> throwError $ invalid "kind" te K_Type (e1, k1)
  kindOfTypeExpr (TE_Tuple [])        = 
    return K_Type
  kindOfTypeExpr te@(TE_Tuple (e:es)) = do
    k <- kindOfTypeExpr e
    case k of
      K_Type -> kindOfTypeExpr $ TE_Tuple es
      k      -> throwError $ invalid "kind" te K_Type (e, k)
  kindOfTypeExpr te@(TE_Constr es tc) = do
    k <- kindOfTypeConstr tc
    apply es k
    where
      apply :: (MonadState InterpreterState m, MonadError String m) => [TypeExpr] -> Kind -> m Kind
      apply []     fk              = return fk
      apply (e:_)  K_Type          = throwError $ tooManyArguments "Type constructor" tc te
      apply (e:es) (K_Arrow k1 k2) = do
        k <- kindOfTypeExpr e
        if   k1 == k
        then apply es k2
        else throwError $ invalid "kind" te k1 (e, k)
