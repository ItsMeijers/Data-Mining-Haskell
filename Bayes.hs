-- | Implementation of the Naive Bayes Classification
module Bayes where
  import System.Random
  import System.Directory (getCurrentDirectory)
  import Data.List (transpose, maximumBy, genericLength, nub)
  import Data.List.Split (splitOn)
  import Data.Function (on)
  import Data.Foldable (foldl')
  import Training
  import qualified Data.Map.Strict as M
  import Control.Applicative ((<$>))

  main :: IO ()
  main = do
    _          <- putStrLn "Bayes Classification with Haskell"
    _          <- putStrLn "Definie the name of the test set:"
    currentDir <- getCurrentDirectory
    fileName   <- getLine
    file       <- readFile (currentDir ++ "/" ++ fileName)
    _          <- putStrLn "Enter a comma seperated entry to classify:"
    test       <- getLine
    stdGen     <- getStdGen
    putStrLn $ case classifyTestFile test file stdGen of
      Left errorMsg -> "Error: " ++ errorMsg
      Right (cl, p)  -> "Classfied on: " ++ fst cl ++ " with a accuracy of: " ++ show p ++ " percent."

  type Result a = Either String a
  type Table    = M.Map String [String]

  classifyTestFile :: String -> String -> StdGen -> Result ((String, Double), Double)
  classifyTestFile testClass file stdGen =
    let (headers, rows)   = createRowsFrom file
        headersT          = tail headers
        (train, test)     = splitRandom (1/3) rows stdGen
        (outcomes, tests) = unzip (fmap (splitAt 1) test)
        table             = M.fromList (zip headers (transpose train))
        testClass'        = zip headersT (splitOn "," testClass)
        outcomes'         = fmap head outcomes
        classes           = zip (repeat (head headers)) (nub outcomes')
    in do
      classifiedClass <- classify table classes testClass'
      testAccuracy    <- accuracy table classes (fmap (zip headersT) tests) outcomes'
      return (classifiedClass, testAccuracy)

  classify :: Table -> [(String, String)] -> [(String, String)] -> Result (String, Double)
  classify table classes testClass = maximumBy (compare `on` snd) <$> hmap
    where hmap = traverse (\(h, e) -> fmap (\r -> (e, r)) (bayes testClass table h e)) classes

  bayes :: [(String, String)] -> Table -> String -> String -> Result Double
  bayes events table header hypothesis = do
    probH <- probabilityH header hypothesis table
    probs <- traverse (\(h, e) -> probability header hypothesis e h table) events
    return $ foldl' (\acc c -> acc + log c) (log probH) probs

  probability :: String -> String -> String -> String -> Table -> Result Double
  probability header hypothesis event header' table = notFound header' $ do
    xs <- M.lookup header table
    ys <- M.lookup header' table
    return (doubleRatio hypothesis event xs ys)

  probabilityH :: String -> String -> Table -> Result Double
  probabilityH header hypothesis table =
    notFound header $ singleRatio hypothesis <$> M.lookup header table

  accuracy :: Table -> [(String, String)] -> [[(String, String)]] -> [String] -> Result Double
  accuracy table classes testSet outcomes = do
    results <- traverse (classify table classes) testSet
    return $ let successes = filter (uncurry (==)) . zip outcomes $ fmap fst results
             in (genericLength successes / genericLength outcomes) * 100.0
