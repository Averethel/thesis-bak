{-# LANGUAGE
  FlexibleContexts
  #-}

module Languages.MiniML.Kinding (kind_of_type_expr) where
  import Utils.Errors

  import Languages.MiniML.State
  import Languages.MiniML.Syntax
  import Languages.MiniML.PrettyPrint

  import Control.Monad.Error
  import Control.Monad.State

  kind_of_type_constr :: (MonadState InterpreterState m, MonadError String m) => TypeConstr -> m Kind
  kind_of_type_constr Int   =
    return K_Type
  kind_of_type_constr Bool  =
    return K_Type
  kind_of_type_constr Unit  =
    return K_Type
  kind_of_type_constr List  =
    return $ K_Arrow K_Type K_Type
  kind_of_type_constr Ref   =
    return $ K_Arrow K_Type K_Type

  kind_of_type_expr :: (MonadState InterpreterState m, MonadError String m) => TypeExpr -> m Kind
  kind_of_type_expr (TE_Var _)           = 
    return K_Type
  kind_of_type_expr te@(TE_Arrow e1 e2)  = do
    k1 <- kind_of_type_expr e1
    k2 <- kind_of_type_expr e2
    case (k1, k2) of
      (K_Type, K_Type) -> return K_Type
      (K_Type, k2)     -> throwError $ invalid "kind" te K_Type (e2, k2)
      (k1,     _)      -> throwError $ invalid "kind" te K_Type (e1, k1)
  kind_of_type_expr (TE_Tuple [])        = 
    return K_Type
  kind_of_type_expr te@(TE_Tuple (e:es)) = do
    k <- kind_of_type_expr e
    case k of
      K_Type -> kind_of_type_expr $ TE_Tuple es
      k      -> throwError $ invalid "kind" te K_Type (e, k)
  kind_of_type_expr te@(TE_Constr es tc) = do
    k <- kind_of_type_constr tc
    apply es k
    where
      apply :: (MonadState InterpreterState m, MonadError String m) => [TypeExpr] -> Kind -> m Kind
      apply []     fk              = return fk
      apply (e:_)  K_Type          = throwError $ too_many_arguments "Type constructor" tc te
      apply (e:es) (K_Arrow k1 k2) = do
        k <- kind_of_type_expr e
        if   k1 == k
        then apply es k2
        else throwError $ invalid "kind" te k1 (e, k)
