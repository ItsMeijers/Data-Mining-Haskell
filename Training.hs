module Training where
  import System.Random
  import Data.List (partition, genericLength)
  import Control.Monad  (join)
  import Data.Bifunctor (bimap)
  import Data.List.Split (splitOn)

  -- | Splits a data set based on a ratio for training and test set
  splitRandom :: Float -> [a] -> StdGen -> ([a], [a])
  splitRandom ratio xs = join bimap (fmap fst)
    . partition ((ratio >) . snd)
    . zip xs
    . randomRs (0, 1)

  -- | Creates headers and rows based on a CSV file
  createRowsFrom :: String -> ([String], [[String]])
  createRowsFrom file = let (headers:rows) = fmap (splitOn ",") (lines file)
                        in  (headers, rows)

  -- Transforms an Maybe b to Either a b based on standard value a when Nothing
  toEither :: a -> Maybe b -> Either a b
  toEither a Nothing  = Left a
  toEither _ (Just b) = Right b

  -- | Column not found specialization
  notFound :: String -> Maybe a -> Either String a
  notFound header = toEither ("column " ++ header ++ " not found.")

  -- | Calculates P(X) from xs
  singleRatio :: Eq a => a -> [a] -> Double
  singleRatio x xs = genericLength (filter (x ==) xs) / genericLength xs

  -- | Calculates P(X | Y) from xs and ys
  doubleRatio :: Eq a => a -> a -> [a] -> [a] -> Double
  doubleRatio x y xs ys = let zipped = zip xs ys
                              filteredOnX = filter ((==) x . fst) zipped
                              filteredOnY = filter ((==) y . snd) filteredOnX
                          in genericLength filteredOnY / genericLength filteredOnX

  -- | Helper that checks wether all the elements in a list are the same
  -- (empty is false)
  allTheSame :: Eq a => [a] -> Bool
  allTheSame [] = False
  allTheSame xs = all (== head xs) (tail xs)
