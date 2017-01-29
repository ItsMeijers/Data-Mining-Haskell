module Vector where

  type Vector a = [a]

  vector2 :: Floating a => a -> a -> Vector a
  vector2 x y = [x, y]

  vector3 :: Floating a => a -> a -> a -> Vector a
  vector3 x y z = [x, y, z]

  vectorN :: Floating a => [a] -> Vector a
  vectorN = id

  sumVector :: Floating a => Vector a -> Vector a -> Vector a
  sumVector = zipWith (+)

  difference :: Floating a => Vector a -> Vector a -> Vector a
  difference = zipWith (-)

  lengthVector :: (Floating a, Enum a) => Vector a -> a
  lengthVector = sqrt . sum . fmap f . zip [1..]
    where f (i, x) = x ** i

  distance :: Floating a => Vector a -> Vector a -> a
  distance v = sqrt . sum . fmap f . zip v
    where f (x1, x2) = (x1 ** 2 - x2 ** 2) ** 2
