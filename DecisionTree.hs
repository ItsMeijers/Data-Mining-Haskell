module DecisionTree where
  import Data.List (nub, genericLength)
  import Control.Applicative ((<$>))
  -- | A DecisionTree consists of either a Node or Leaf
  -- | The Leaf contains a value for the classification
  -- | A Node contains an attribute of the data (a) and a List of Tuple2
  -- | where the first element is the predicate to that specific child, the
  -- | second element
  data DecisionTree a b = Node a [(a -> Bool, DecisionTree a b)] | Leaf b

  -- | Calculates the entropy of a List
  -- | applies the lambda over unique elements of xs (by using nub) and sums
  -- | the results
  -- | Note that <$> is a infix notation of fmap
  entropy :: Eq a => [a] -> Double
  entropy xs = sum $ (\c -> negate (p c) * logBase 2.0 (p c)) <$> nub xs
    where p c' = genericLength (filter (c' ==) xs) / genericLength xs

  -- f is probably of different form
  gain :: Eq a => [a] -> a -> Double
  gain xs f = entropy xs - sum ((\t -> undefined) <$> xs)
