module Main where

maxInList :: (Num a, Ord a) => [a] -> a
maxInList x = maxInListRecursive (tail x) (head x)

maxInListRecursive :: (Num a, Ord a) => [a] -> a -> a
maxInListRecursive x m
  | x == [] = m
  | head x <= m = maxInListRecursive (tail x) m
  | head x > m = maxInListRecursive (tail x) (head x)

minInList :: (Num a, Ord a) => [a] -> a
minInList x = minInListRecursive (tail x) (head x)

minInListRecursive :: (Num a, Ord a) => [a] -> a -> a
minInListRecursive x m
  | x == [] = m
  | head x >= m = minInListRecursive (tail x) m
  | head x < m = minInListRecursive (tail x) (head x)

minMaxInList :: (Num a, Ord a) => [a] -> (a, a)
minMaxInList x = minMaxInListRecursive (tail x) ((head x), (head x))

minMaxInListRecursive :: (Num a, Ord a) => [a] -> (a, a) -> (a, a)
minMaxInListRecursive x (max, min)
  | x == [] = (max, min)
  | head x <= min = minMaxInListRecursive (tail x) (max, (head x))
  | head x >= max = minMaxInListRecursive (tail x) ((head x), min)
  | otherwise = minMaxInListRecursive (tail x) (max, min)

main :: IO ()
main = do
  print (maxInList [1, 5, 4, 5, 7, 3, 2])
  print (minInList [1, 5, 4, 5, 7, 3, 2])
  print (minMaxInList [1, 5, 4, 5, 7, 3, 2])
