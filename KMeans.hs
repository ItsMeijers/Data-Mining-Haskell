module KMeans where
  import qualified Data.Text as T
  import System.Directory (getCurrentDirectory)
  import Data.List (sort)

  main :: IO ()
  main = do
    currentDir <- getCurrentDirectory
    file       <- readFile (currentDir ++ "/dogs.csv")
    print $ let (x:xs) = (fmap (T.splitOn $ T.pack ",") . T.lines) (T.pack file)
                rows   = readRows xs
            in rows

  data DataKMeans = DataKMeans
    { rows       :: ![(Integer, [Double])] -- index plus row
    , clusters   :: ![(Integer, [Integer])] -- Cluster + indices of members
    , centroids  :: ![(Double, Double)]
    , sse        :: !Double
    , change     :: !Double -- % of points that change cluster membership
    , iterations :: !Integer
    } deriving Show

  readRows :: [[T.Text]] -> [[Double]]
  readRows = fmap $ fmap (read . T.unpack) . tail

  median :: [Double] -> Double
  median xs = let size   = length xs
                  sorted = sort xs
                  split  = size `div` 2
              in if size `mod` 2 == 1
                   then sorted !! split
                   else ((sorted !! split) + (sorted !! (split - 1))) / 2

  normalizeColumn :: [Double] -> [Double]
  normalizeColumn xs = fmap normalize xs
    where median' = median xs
          asd     = sum (fmap (\x -> abs (x - median')) xs) / fromIntegral (length xs)
          normalize x = (x - median') / asd

  euclideanDistance :: [Double] -> [Double] -> Double
  euclideanDistance xs ys = sqrt $ sum $ zipWith (\p q -> (q - p) ** 2) xs ys

  -- randomCentroids :: [[Double]] -> []

-- K-means basic algo:
-- SELECT K Random instances to be the initial centroids
-- REPEAT
--   assign each instance to the nearest centroid (forming k clusters)
--   update centroids by computing the mean of each cluster
-- UNTIL centroids don't change (much) || max iterations has been reached

-- TO determine the quality of a set of cluster we can use the sum of squared error (SSE)
-- When K-mean run mutliple times, the set with the smaller SSE is the better of the two
