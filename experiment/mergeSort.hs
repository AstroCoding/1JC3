module Merge where

mergeSort :: [Int] -> [Int] -> [Int]
mergeSort a b
  | a == [] = b
  | b == [] = a
  | head a >= head b = [head b] ++ (mergeSort a (tail b))
  | otherwise = [head a] ++ (mergeSort (tail a) b)

factorial n
  | n == 0 = 1
  | n > 0 = n * factorial (n - 1)

sigma :: Num a => Int -> Int -> (Int -> a) -> a
sigma n i f
  | i == n = f i
  | i < n = f i + sigma n (i + 1) f

function :: Int -> Float
function n = 1 / ((fromIntegral n) ** 2)
