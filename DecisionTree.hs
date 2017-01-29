module DecisionTree where
  import System.Directory (getCurrentDirectory)
  import qualified Data.Text as T
  import Data.List (nub, genericLength, transpose)
  import Data.List.Split (splitOn)
  import Data.Foldable hiding (sum)
  import Control.Arrow (second, (&&&))
  import Training (splitRandom, createRowsFrom)
  import System.Random (getStdGen, StdGen)
  import Data.Maybe (fromMaybe, catMaybes)

  test :: [T.Text]
  test = fmap T.pack ["x","y","n","t","l","f","c","b","p","e","r","s","y","w","w","p","w","o","p","n","y"]

  main :: IO ()
  main = do
    currentDir <- getCurrentDirectory
    _          <- putStrLn "Name of the CSV to classify:"
    fileName   <- getLine
    file       <- readFile (currentDir ++ "/" ++ fileName)
    _          <- putStrLn "Specify a comma seperated test set to classify:"
    testInput  <- getLine
    stdGen     <- getStdGen
    print (classifyTest file testInput stdGen)

  --classifyTest :: String -> String -> StdGen -> Result
  classifyTest file testInput stdGen =
    let (headers, rows)     = createRowsFrom file
        (trainSet, testSet) = splitRandom (1/3) rows stdGen
        testToClassify      = parseInput testInput
        --tree                = buildTreeWith headers testSet
        --classifiedTest      = classify tree (zip headers testToClassify)
        trainSet'           = fmap (head &&& tail) trainSet
        --trainingRatio       = accuracy undefined undefined tree
    in trainSet --maybe NoResult (Result trainingRatio testInput) classifiedTest

  parseInput :: String -> [String]
  parseInput = splitOn ","


  data Result = NoResult
              | Result
                { percentage :: Double
                , testSet    :: String
                , outCome    :: String
                }

  instance Show Result where
    show NoResult = "No Result due to error."
    show result   = "The classification of: " ++
                    testSet result            ++
                    " resulted in: "          ++
                    outCome result            ++
                    " with an accuracy of: "  ++
                    show (percentage result)  ++
                    " percent."

  -- | A DecisionTree consists of either a Node or Leaf
  -- A node has multiple branches and a Leaf has the classification value
  data DecisionTree a = Node a [Branch a]
                      | Leaf a
                      deriving Show

  -- | A branch has a tag a value and child DecisionTree attached to it
  data Branch a = Branch a (DecisionTree a) deriving Show

  -- | Getter of the tag of a Branch
  tag :: Branch a -> a
  tag (Branch a _) = a

  -- | Getter of the decision tree attached to a Branch
  cons :: Branch a -> DecisionTree a
  cons (Branch _ dt) = dt

  -- | Calculates the entropy for a List
  entropy :: Eq a => [a] -> Double
  entropy xs = sum $ (\c -> negate (p c) * logBase 2.0 (p c)) <$> nub xs
    where p c'     = genericLength (filter (c' ==) xs) / xsLength
          xsLength = genericLength xs

  -- | Calculates the gain for two lists
  gain :: Eq a => [a] -> [a] -> Double
  gain xs ys = entropy xs - sum (fmap (\t -> p t * entropy t) tss)
    where p t'     = genericLength t' / genericLength xs
          tss      = fmap subset (nub ys)
          subset x = fmap fst . filter ((==) x . snd) $ zip xs ys

  -- | Builds the tree based on ID3 algorithm
  -- cs is a list of class attributes that form the goal of the classifier
  -- tss is the table where the first element of the tuple is the header and
  -- the second element is a list that represents the column values
  buildTree :: Eq a => [a] -> [(a, [a])] -> DecisionTree a
  buildTree cs tss
    | allTheSame cs = Leaf (head cs)
    | otherwise     = uncurry Node $ buildBranches cs tss

  -- | Builds the branches based on Gain and Entropy
  -- Is recursively called from and to buildTree
  buildBranches :: Eq a => [a] -> [(a, [a])] -> (a, [Branch a])
  buildBranches cs tss = (s, fmap dBranch (nub fs))
    where withGain    = fmap (\(t, ts) -> (t, ts, gain cs ts)) tss
          (s, fs, ga) = maximumBy (\(_, _, g) (_, _, g') -> compare g g') withGain
          dBranch a   = if ga == 0 -- TODO ADD IF GA == 1 SINCE THE OUTCOME THEN IS EQUAL!!!!!!!!!!!!!!!!!!! TODO
              then Branch a . Leaf $ head cs
              else Branch a . uncurry buildTree $ extractSubset s a fs cs tss

  extractSubset :: Eq a => a -> a -> [a] -> [a] -> [(a, [a])] -> ([a], [(a, [a])])
  extractSubset extract f fs xs table =
    let fs'     = fmap (f ==) fs                     -- see where all the feature equals the element in the list
        xs'     = fmap snd (filter fst (zip fs' xs)) -- filter it out for the goal set
        table'  = filter ((/=) extract . fst) table  -- filter the table without the already selected column
        table'' = fmap (second $ fmap snd . filter fst . zip fs') table' -- filter out all the other columns on the selected feature
    in  (xs', table'')


  -- | Helper that checks wether all the elements in a list are the same
  -- (empty is false)
  allTheSame :: Eq a => [a] -> Bool
  allTheSame [] = False
  allTheSame xs = all (== head xs) (tail xs)

  classify :: Eq a => DecisionTree a -> [(a, a)] -> Maybe a
  classify (Node a bs) xs = do
      (_, feature) <- find ((==a) . fst) xs
      branch       <- find ((==feature) . tag) bs
      case cons branch of
          (Leaf o) -> Just o
          node     -> classify node (filter ((feature /=) . snd) xs)
  classify _ _ = Nothing

  accuracy :: Eq a => [[(a, a)]] -> [a] -> DecisionTree a -> Double
  accuracy ts expectedResults tree = (genericLength success / genericLength ts) * 100
    where classifiedResult    = zip (fmap (classify tree) ts) expectedResults
          success             = filter equals classifiedResult
          equals (Just a, a') = a == a'
          equals (Nothing, _) = False
