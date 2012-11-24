{-# LANGUAGE
  FlexibleContexts
  #-}

module Utils.Memory where
  import Utils.Classes.Language
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

  emptyMem :: Language n p tp e i v => n -> Integer -> Memory v
  emptyMem _ n =
    M {
        mem      = listArray (1, n) $ replicate (fromInteger n) nullValue,
        freeCell = [1..n],
        size     = n
      }

  getFreeAddr :: (MonadError String m, Language n p tp e i v) => Memory v -> m Integer
  getFreeAddr mem = do
    case freeCell mem of
      []  -> throwError $ memoryFull
      a:_ -> return a

  clearFreeAddr :: Language n p tp e i v => Memory v -> Memory v
  clearFreeAddr mem = mem {freeCell = tail . freeCell $ mem}

  update :: Language n p tp e i v => Memory v -> Integer -> v -> Memory v
  update m addr v = m {mem = (mem m)//[(addr, v)]}

  at :: Language n p tp e i v => Memory v -> Integer -> v
  m `at` addr = (mem m) ! addr
