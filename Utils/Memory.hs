{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.Memory where
  import Utils.Classes.Value
  import Utils.Errors

  import Control.Monad.Error
  import Data.Array

  data Memory v =
    M {
        mem      :: Array Integer v,
        freeCell :: [Integer],
        size   :: Integer
      }

  emptyMem :: Value v => n -> Integer -> Memory v
  emptyMem _ n =
    M {
        mem      = listArray (1, n) $ replicate (fromInteger n) nullValue,
        freeCell = [1..n],
        size     = n
      }

  getFreeAddr :: (MonadError String m, Value v) => Memory v -> m Integer
  getFreeAddr mem = do
    case freeCell mem of
      []  -> throwError $ memoryFull
      a:_ -> return a

  clearFreeAddr :: Value v => Memory v -> Memory v
  clearFreeAddr mem = mem {freeCell = tail . freeCell $ mem}

  update :: Value v => Memory v -> Integer -> v -> Memory v
  update m addr v = m {mem = (mem m)//[(addr, v)]}

  at :: Value v => Memory v -> Integer -> v
  m `at` addr = (mem m) ! addr
