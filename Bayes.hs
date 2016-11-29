{-# LANGUAGE ExistentialQuantification #-}

-- | Implementation of the Naive Bayes Classification
module Bayes where
  import qualified Data.Text as T
  import System.Random
  import System.Directory (getCurrentDirectory)

  main :: IO ()
  main = do
    currentDir <- getCurrentDirectory
    file       <- readFile (currentDir ++ "/pima-indians-diabetes.csv")
    stdGen     <- getStdGen
    print $ let dataset = readDataSet $ readTable $ T.pack file
                (testing, train) = readTestingTraining 0.67 (fields dataset) stdGen
            in train

  type Table = [[T.Text]]
  type Row   = [Double]

  data DataSet = DataSet { header :: [T.Text] , fields  :: [Row] } deriving Show

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
