{-# LANGUAGE ExistentialQuantification #-}

-- | Implementation of the Naive Bayes Classification
module Bayes where
  import qualified Data.Text as T
  import System.Random
  import System.Directory (getCurrentDirectory)
  import Data.List (transpose)

  main :: IO ()
  main = do
    currentDir <- getCurrentDirectory
    file       <- readFile (currentDir ++ "/pima-indians-diabetes.csv")
    stdGen     <- getStdGen
    print $ let dataset = readDataSet $ readTable $ T.pack file
                (testing, train) = readTestingTraining 0.67 (fields dataset) stdGen
            in probabilityPerClass 22 dataset

  type Table = [[T.Text]]
  type Row   = [Double]
  type Class = T.Text
  type Mean = Double
  type StandardDeviation = Double
  type Probability = Double

  data DataSet = DataSet { header :: [Class] , fields  :: [Row] } deriving Show

  readTable :: T.Text -> Table
  readTable = fmap (T.splitOn (T.pack ",")) . T.lines

  readDataSet :: Table -> DataSet
  readDataSet (x:xs) = DataSet x (readValues xs)
  readDataSet _      = error "Data set requires atleast 2 rows"

  readValues :: [[T.Text]] -> [Row]
  readValues = fmap (fmap (read . T.unpack))

  -- | Results in (Testing, Train)
  -- | MAKE PRETIER
  readTestingTraining :: Double -> [Row] -> StdGen -> ([Row], [Row])
  readTestingTraining splitRatio rows stdg =
    let trainSize = round (realToFrac (length rows) * splitRatio) :: Int
    in getTrainSet trainSize rows [] stdg

  -- | MAKE PRETIER
  getTrainSet :: Int -> [Row] -> [Row] -> StdGen -> ([Row], [Row])
  getTrainSet n rows tRows stdg | length tRows < n =
    let (randomNumber, stdg') = randomR (0, n) stdg
    in getTrainSet (n-1) rows (tRows ++ [rows !! randomNumber]) stdg'
  getTrainSet _ rows tRows _ = (rows, tRows)

  mean :: [Double] -> Mean
  mean xs = sum xs / realToFrac (length xs)

  standardDeviation :: [Double] -> StandardDeviation
  standardDeviation xs = let average = mean xs
                             variance = sum (fmap (\x -> (x - average) ** 2) xs)
                                        / realToFrac (length xs - 1)
                         in sqrt variance

  summarizePerClass :: DataSet -> [(Class, (Mean, StandardDeviation))]
  summarizePerClass ds = fmap summarize (zip [0..] classes)
    where classes = header ds
          columns = transpose $ fields ds
          summarize (i, name) = let column = columns !! i
                                in (name,(mean column, standardDeviation column))

  calculateProbability :: Double -> Double -> Double -> Double
  calculateProbability x mean' stdev = (1 / (sqrt (2 * pi) * stdev)) * exponent'
    where exponent' = exp(- ((x - mean') ** 2) / (2 * (stdev ** 2)))

  -- | NO TIMES INCOPERATED BUGS!!!  
  probabilityPerClass :: Double -> DataSet -> [(Class, Probability)]
  probabilityPerClass x ds = fmap (\(c, (m, sd)) -> (c, calculateProbability x m sd)) sumarized
    where sumarized = summarizePerClass ds
