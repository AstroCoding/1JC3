module Integral where

import           Test.QuickCheck

definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n
  | n < 0 = error "Number of Trapezoids must be positive."
  | a > b = (-1) * definiteIntegral b a g n
  | a == b || n == 0 = 0
  | otherwise =
    let deltaX = (b - a) / fromIntegral n
        nextA = a + deltaX
        fraction = ((g a) + (g nextA)) / 2
     in (fraction * deltaX) + definiteIntegral (nextA) b g (n - 1)

approximateIntegral ::
     Double -> Double -> (Double -> Double) -> Integer -> Double
approximateIntegral a b g n
  | n < 0 = error "Number of Trapezoids must be positive."
  | a > b = (-1) * definiteIntegral b a g n
  | a == b || n == 0 = 0
  | otherwise = calcSum a b g (generateXI a b n) * ((b - a) / fromIntegral n)

generateXI :: Double -> Double -> Integer -> [Double]
generateXI a b n =
  let deltaX = (b - a) / (fromIntegral n)
   in [a + (i * deltaX) | i <- [0 .. fromIntegral n]]

calcSum :: Double -> Double -> (Double -> Double) -> [Double] -> Double
calcSum a b g (x:[]) = 0
calcSum a b g (x0:x1:xs) =
  let numerator = g x0 + g x1
   in numerator / 2 + calcSum a b g (x1 : xs)

egG :: Double -> Double
egG v = v

prop :: Double -> Double -> Integer -> Property
prop a b n =
  n > 0 && a < b ==> 0.0001 > (definiteIntegral a b (\x -> x) n) -
  (approximateIntegral a b (\x -> x) n) &&
  (definiteIntegral a b (\x -> x) n) -
  (approximateIntegral a b (\x -> x) n) >
  (-0.0001)
