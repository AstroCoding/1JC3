module Vicky where

definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n
  | a == b = 0.0
  | otherwise = defInt a b g n (fromIntegral (n))

defInt a b g n 1 = ((g a + g (a + deltaX)) / 2) * deltaX
  where
    deltaX = (b - a) / (fromIntegral n)
defInt a b g n i =
  ((g (a + deltaX * (i - 1)) + g (a + i * deltaX)) / 2) * deltaX +
  defInt a b g n (i - 1)
  where
    deltaX = (b - a) / (fromIntegral n)
