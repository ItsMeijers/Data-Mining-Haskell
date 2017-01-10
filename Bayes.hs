{-# LANGUAGE ExistentialQuantification #-}

-- | Implementation of the Naive Bayes Classification
module Bayes where
  import qualified Data.Text as T
  import System.Random
  import System.Directory (getCurrentDirectory)
  import Data.List (transpose, maximumBy, genericLength)
  import Data.Char (isDigit)
  import Data.Function (on)
  import Control.Arrow

  main :: IO ()
  main = do
    currentDir <- getCurrentDirectory
    file       <- readFile (currentDir ++ "/pima-indians-diabetes.csv")
    stdGen     <- getStdGen
    print $ let dataset = readDataSet 0.67 True (T.pack file) stdGen
                testSummary  = summarizeColumns $ testing dataset
                trainSummary = summarizeColumns $ training dataset
            in (testSummary, trainSummary)

  data Column = TextColumn [T.Text]
              | NumericColumn [Double]
              deriving Show

  -- | A data set for the Naive Bayes Classification
  data DataSet = DataSet
      { headers  :: Maybe [T.Text]  -- ^ Optional headers
      , testing  :: ![Column]      -- ^ Testing part of the data set
      , training :: ![Column]      -- ^ Training part of the data set
      } deriving Show

  -- | Reads the dataset based on the sr as split ratio for training, headers
  -- based on if the csv has headers and content as the Text type of reading the
  -- file and StdGen from the IO action to get a randomized value generator.
  readDataSet :: Double -> Bool -> T.Text -> StdGen -> DataSet
  readDataSet sr hasHeaders content stdGen =
    let (x:xs)    = (fmap (T.splitOn $ T.pack ",") . T.lines) content -- Read the csv in as a [[Text]]
        (h, tc)   = if hasHeaders -- Optional header plus columns in Text form
                      then (Just x, xs)
                      else (Nothing, x:xs)
        trainSize = round $ realToFrac (length tc) * sr
        trainSet  = createTrainingSet tc trainSize stdGen
        columns   = fmap createColumn (transpose tc) -- creates the Text Columns into Column
    in DataSet h columns trainSet

  -- | Creates the training set based on the rows from the csv and the training
  -- size results in StdGen -> [Column]
  createTrainingSet :: [[T.Text]] -> Int -> StdGen -> [Column]
  createTrainingSet rows size =
    fmap createColumn . transpose . randomRows size (length rows) rows

  -- | Creates a list of rows randomly picked from the current rows of the csv
  -- Flow: Creates a list of random numbers between 0 and total size of the csv
  -- takes from the infinite list an amount equal to the training size
  -- based on these numbers it will create a new rows by using addRow
  randomRows :: Int -> Int -> [[T.Text]] -> StdGen -> [[T.Text]]
  randomRows n total cr = foldl addRow [] . take n . randomRs (0, total - 1)
    where addRow nr i = (cr !! i) : nr

  -- | Reads a list of text into a single column, Naive Bayes Classification
  -- uses columns intensively so for efficiency the data will be transported
  -- directly to columns instead of per calculation
  createColumn :: [T.Text] -> Column
  createColumn cols@(x:_)
    | isNumber x = NumericColumn $ fmap (read . T.unpack) cols
    | otherwise  = TextColumn cols

  -- | Helper function to check wether a text value is a double
  -- Could fail on values that are numbers and have more dots, but that probably
  -- won't be the case in most csv files.
  isNumber :: T.Text -> Bool
  isNumber = T.all (\c -> isDigit c || c == '.')

  -- | Calculates the mean of a column
  mean :: Column -> Double
  mean (NumericColumn xs) = sum xs / realToFrac (length xs)
  mean (TextColumn xs)    = undefined

  -- add variance function:: Column -> Double and define standard deviation only
  -- on one function since you
  -- dont need to pattern match :D

  standardDeviation :: Column -> Double
  standardDeviation nc@(NumericColumn xs) =
    let average  = mean nc
        variance = sum (fmap (\x -> (x - average) ** 2) xs) / realToFrac (length xs - 1)
    in sqrt variance
  standardDeviation (TextColumn xs)   = undefined

  -- | Calculates density can be over >1 research a bit more...
  calculateProbability :: Double -> Double -> Double -> Double
  calculateProbability x avg stdev = (1 / (sqrt (2 * pi) * stdev)) * exponent'
    where exponent' = exp(- ((x - avg) ** 2) / (2 * (stdev ** 2)))

  summarizeColumns :: [Column] -> [(Double, Double)]
  summarizeColumns = fmap (mean &&& standardDeviation)

  classProbabilities :: [Double] -> [(Double, Double)] -> [Double]
  classProbabilities input = foldl prob []
    where prob acc (m, sd) = acc ++ [foldl (probClass m sd) 1 input]
          probClass m' sd' x i = x * calculateProbability i m' sd'

  -- | Calculates class probabilties resulting in the max probability of the class
  -- index
  predict :: [Double] -> [(Double, Double)] -> (Integer, Double)
  predict xs = maximumBy (compare `on` snd) . zip [1..] . classProbabilities xs

  predicts :: [[Double]] -> [(Double, Double)] -> [(Integer, Double)]
  predicts xss summaries = fmap (`predict` summaries) xss

  -- | Compares testsset values with the predicted set values
  accuracy :: Eq a => [a] -> [a] -> Double
  accuracy ts ps = (genericLength equalPredicts / genericLength ts) * 100.0
    where equalPredicts = filter (uncurry (==)) $ zip ts ps
