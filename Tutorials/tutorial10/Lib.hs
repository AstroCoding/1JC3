module Lib where

import           Test.QuickCheck

myAbsolute :: (Num a, Ord a) => a -> a
myAbsolute x
  | x >= 0 = x
  | x < 0 = -x

absoluteProperties :: Integer -> Bool
absoluteProperties x = (myAbsolute x) >= 0

mySum :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs

sumProperties :: ([Integer], [Integer]) -> Bool
sumProperties (xs, ys) = (mySum xs) + (mySum ys) == mySum (xs ++ ys)

-- data Vector2D =
--   Vector2D (Float, Float)
--   deriving (Show)
-- vectorAdd :: Vector2D -> Vector2D -> Vector2D
-- vectorAdd (Vector2D (x0, y0)) (Vector2D (x1, y1)) =
--   Vector2D ((x0 + x1), (y0 + y1))
-- vecProp :: Float -> Float -> Bool
-- vecProp x y = do
--   let v = Vector2D (x, y)
--   in (vectorAdd v (Vector2D (0, 0))) == v

takeDropProp :: Int -> [Int] -> Bool
takeDropProp n l = reverse (take (length l - n) (reverse l)) == take n l
