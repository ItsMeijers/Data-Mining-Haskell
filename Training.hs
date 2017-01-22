module Training where
  import System.Random
  import Data.List (partition)
  import Control.Monad  (join)
  import Data.Bifunctor (bimap)

  splitR :: Float -> [a] -> StdGen -> ([a], [a])
  splitR r xs = join bimap (fmap fst)
    . partition ((r >) . snd)
    . zip xs
    . randomRs (0, 1)
