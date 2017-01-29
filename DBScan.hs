module DBScan where
  import System.Directory (getCurrentDirectory)
  import Vector
  import Data.List.Split (splitOn)
  import Data.List (union, foldl')
  import Data.Bool (bool)

  main :: IO ()
  main = do
    _          <- putStrLn "DBScan Haskell."
    _          <- putStrLn "Define the name of the test set:"
    currentDir <- getCurrentDirectory
    fileName   <- getLine
    file       <- readFile (currentDir ++ "/" ++ fileName)
    _          <- putStrLn "Define the mu: "
    mu         <- getLine
    _          <- putStrLn "Define epsilon: "
    epsilon    <- getLine
    print $ let points   = readData 3 file
                minPts   = read mu
                eps      = read epsilon
                (cs, ns) = dbscan eps minPts points
            in (cs, ns)

  data Point a = Visited    (Vector a)
               | NotVisited (Vector a)
               | Noise      (Vector a)
               deriving (Show)

  instance Eq a => Eq (Point a) where
    p1 == p2 = vector p1 == vector p2

  type Cluster a = (Int, [Point a])

  vector :: Point a -> Vector a
  vector (Visited va)    = va
  vector (NotVisited va) = va
  vector (Noise va)      = va

  visited :: Point a -> Bool
  visited (NotVisited _) = False
  visited (Noise _)      = False
  visited _              = True

  visit :: Point a -> Point a
  visit = Visited . vector

  noise :: Point a -> Point a
  noise = Noise . vector

  readData :: Int -> String -> [Point Double]
  readData dimensions = take 500 . fmap (toPoint . splitOn ",") . tail . lines
    where toPoint = NotVisited . take dimensions . fmap read

  dbscan :: (Ord a, Floating a) => a -> Int -> [Point a] -> ([Cluster a], [Point a])
  dbscan eps minPts points = let (cl, ns) = foldl constructClusers ([], []) points
                             in (zip [1..] cl, ns)
    where constructClusers (clusters, ns') c
            | or (fmap (c `elem`) clusters) = (clusters, ns')
            | otherwise  = case construction eps minPts points c of
              (Left n)           -> (clusters, n:ns')
              (Right newCluster) -> (clusters ++ [newCluster], ns')

  construction :: (Ord a, Floating a) => a -> Int -> [Point a] -> Point a -> Either (Point a) [Point a]
  construction eps minPts points p =
    let pVisited   = visit p
        neighbours = epsNeighbourhood eps pVisited points
    in if length neighbours < minPts
     then Left (noise pVisited)
     else Right (expansion [pVisited] [] eps minPts points)

  expansion :: (Ord a, Floating a) => [Point a] -> [Point a] -> a -> Int -> [Point a] -> [Point a]
  expansion [] pss _ _ _ = pss
  expansion (p:ps) pps eps minPts points =
       let neighbours = epsNeighbourhood eps p points
           ps'        = (ps `union` filter (`notElem` pps) neighbours)
           pps'       = bool pps (p:pps) (p `notElem` pps)
       in bool [] (expansion ps' pps' eps minPts points) (length neighbours >= minPts)

  epsNeighbourhood :: (Floating a, Ord a) => a -> Point a -> [Point a] -> [Point a]
  epsNeighbourhood eps point = filter ((>=) eps . distance (vector point) . vector)
