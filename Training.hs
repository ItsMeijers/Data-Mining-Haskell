module Training where
  import System.Random
  import Data.List (partition, genericLength)
  import Control.Monad  (join)
  import Data.Bifunctor (bimap)
  import Data.List.Split (splitOn)

  splitRandom :: Float -> [a] -> StdGen -> ([a], [a])
  splitRandom ratio xs = join bimap (fmap fst)
    . partition ((ratio >) . snd)
    . zip xs
    . randomRs (0, 1)

  createRowsFrom :: String -> ([String], [[String]])
  createRowsFrom file = let (headers:rows) = fmap (splitOn ",") (lines file)
                        in  (headers, rows)

  toEither :: a -> Maybe b -> Either a b
  toEither a Nothing  = Left a
  toEither _ (Just b) = Right b

  singleRatio :: Eq a => a -> [a] -> Double
  singleRatio x xs = genericLength (filter (x ==) xs) / genericLength xs

  doubleRatio :: Eq a => a -> a -> [a] -> [a] -> Double
  doubleRatio x y xs ys = let zipped = zip xs ys
                              filteredOnX = filter ((==) x . fst) zipped
                              filteredOnY = filter ((==) y . snd) filteredOnX
                          in genericLength filteredOnY / genericLength filteredOnX

  notFound :: String -> Maybe a -> Either String a
  notFound header = toEither ("column " ++ header ++ " not found.")
