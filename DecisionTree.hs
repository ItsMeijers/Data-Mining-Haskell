module DecisionTree where
  import System.Directory (getCurrentDirectory)
  import qualified Data.Text as T
  import Data.List (nub, genericLength, transpose, sortBy)
  import Control.Applicative
  import Data.Traversable
  import Data.Foldable hiding (sum)
  import Data.Monoid (mappend)
  import Control.Monad (ap, liftM)
  import qualified Data.Foldable ()
  import qualified Data.Traversable ()
  import qualified Control.Applicative ()

  main :: IO ()
  main = do
    currentDir <- getCurrentDirectory
    file       <- readFile (currentDir ++ "/dtree.csv")
    print $ let (x:xs)  = fmap (T.splitOn $ T.pack ",") $ T.lines $ T.pack file
                columns = zip x (transpose xs)
            in build (snd (last columns)) (init columns)

  -- | A DecisionTree consists of either a Node or Leaf
  -- The Leaf contains a value for the classification
  -- A Node contains an attribute of the data (a) and a List of Tuple2
  -- where the first element is the predicate to that specific child, the
  -- second element
  data DecisionTree a = Node a [Branch a]
                      | Leaf a
                      deriving Show

  data Branch a = Branch a (DecisionTree a) deriving Show

  -- -- | Instance of the Foldable typeclass for DecisionTree
  -- -- Makes it possible to call fold functions
  -- instance Foldable DecisionTree where
  --     foldMap f (Node a ts) = f a `mappend` foldMap (foldMap f) ts
  --     foldMap f (Leaf a)    = f a
  --
  -- -- | Instance of the Traversable typeclass for DecisionTree
  -- -- Makes it possible to traverse over DecisionTrees
  -- instance Traversable DecisionTree where
  --     traverse f (Leaf a)    = Leaf <$> f a
  --     traverse f (Node a ts) = Node <$> f a <*> for ts (traverse f)
  --
  -- -- | Instance of the Functor typeclass for DecisionTree
  -- -- Makes it possible to map over DecisionTrees
  -- instance Functor DecisionTree where
  --     fmap = liftM
  --
  -- -- | Instance of the Applicative Functor typeclass for DecisionTree
  -- -- Makes it possible to have sequential application over DecisionTrees
  -- instance Applicative DecisionTree where
  --     pure  = Leaf
  --     (<*>) = ap
  --
  -- -- | Instance of the Monad typeclass for DecisionTree
  -- -- Makes it possible to dependent sequencing over DecisionTrees
  -- instance Monad DecisionTree where
  --     return = pure
  --     (Leaf a)    >>= f = f a
  --     (Node a ta) >>= f = case f a of
  --         (Leaf b)    -> Node b (fmap (>>= f) ta)
  --         (Node b tb) -> Node b (tb `mappend` fmap (>>= f) ta)

  -- | Calculates the entropy of a List
  -- applies the lambda over unique elements of xs (by using nub) and sums
  -- the results
  -- Note that <$> is a infix notation of fmap
  entropy :: Eq a => [a] -> Double
  entropy xs = sum $ (\c -> negate (p c) * logBase 2.0 (p c)) <$> nub xs
    where p c' = genericLength (filter (c' ==) xs) / genericLength xs

  -- | Calculates the gain of two lists
  gain :: Eq a => [a] -> [a] -> Double
  gain xs ys = entropy xs - sum (fmap (\t -> p t * entropy t) tss)
    where p t'     = genericLength t' / genericLength xs
          tss      = fmap subset (nub ys)
          subset x = fmap fst $ filter ((==) x . snd) $ zip xs ys

  build :: Eq a => [a] -> [(a, [a])] -> DecisionTree a
  build xs tss = Node n (fmap (\b -> Branch b (Leaf b)) (nub ns))
    where withGain   = fmap (\(t, ts) -> (t, ts, gain xs ts)) tss
          sorted     = sortBy (\(_, _, g) (_, _, g') -> compare g' g) withGain
          (n, ns, _) = head sorted
