
module Assign_1_ExtraCredit where

import Data.Complex
-- see https://www.stackage.org/haddock/lts-8.24/base-4.9.1.0/Data-Complex.html

macid :: String
macid = "hutchm6"

{-
    - Assignment 1
    - Name: Mark Hutchison
    - Date: September 17th, 2019
 -}

{-
    Similar to the normal assignment, I will be using the 4 base equations that will be listed under this comment string. That being said, because the equations for Q R and Disc do not change, I will not be re showing the outputs for those above the functions.

    - cubicRealSolutions 1 (-6) 11 (-6) = should [1,2,3]
    - cubicRealSolutions 1 0 (-3) 0 = [0, sqrt 3, -sqrt 3]
    - cubicRealSolutions 1 (-3) 3 (-1) = should [1,1,1]
    - cubicRealSolutions 1 (-5) 8 (-4) = should [1,2,2]
-}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3 * a * c) - (b ** 2)) / (9 * (a ** 2))

cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9 * a * b * c) - (27 * (a ** 2) * d) - (2 * (b ** 3))) / (54 * (a ** 3))

cubicDisc :: Double -> Double -> Double
cubicDisc q r = (q ** 3) + (r ** 2)

{-
    Here is the point where everything changes and we need to begin thinking in complex number systems.

    We need to be able to get the square root of a negative number, so the S and T function needed to change.

    Additionally, x2 and x3 now need to change to include the final product: ((sqrt 3) / 2)i * (s - t)
    This was not in Assign_1 because we only dealt with cases where i = 0, and anything multiplied by 0 is 0.
    However, we can now work with imaginary numbers, which means i will not be equivalent to 0 in every case.
-}

complexSquareRoot :: Double -> Double
complexSquareRoot n = if n < 0 then sqrt (-n) else sqrt n

cubicComplexS :: Double -> Double -> Complex Double
cubicComplexS q r = do
    if cubicDisc q r < 0
        then
            let
                real = r
                imaginary = (complexSquareRoot (cubicDisc q r))
            in cubeRoot (real :+ imaginary)
        else
            let
                real = (r + sqrt (cubicDisc q r))
                imaginary = 0;
            in cubeRoot (real :+ imaginary)

cubicComplexT :: Double -> Double -> Complex Double
cubicComplexT q r = do
    if cubicDisc q r < 0
        then
            let
                real = r
                imaginary = -(complexSquareRoot (cubicDisc q r))
            in cubeRoot (real :+ imaginary)
        else
            let
                real = (r - sqrt (cubicDisc q r))
                imaginary = 0;
            in cubeRoot (real :+ imaginary)

cubicComplexSolutions :: Double -> Double -> Double -> Double -> [Complex Double]
cubicComplexSolutions a b c d = [x1, x2, x3] where
    aComplex = a :+ 0
    bComplex = b :+ 0
    q = cubicQ a b c
    r = cubicR a b c d
    s = cubicComplexS q r
    t = cubicComplexT q r
    x1 = (s + t - (bComplex / (3 * aComplex)))
    x2 = ((-1) * ((s + t) / 2) - (bComplex / (3 * aComplex))) + ((0 :+ ((sqrt 3) / 2)) * (s - t))
    x3 = ((-1) * ((s + t) / 2) - (bComplex / (3 * aComplex))) - ((0 :+ ((sqrt 3) / 2)) * (s - t))

cubeRoot :: Complex Double -> Complex Double
cubeRoot n = do
    if realPart n > 0
        then n ** (1/3)
        else
            if realPart n < 0
                then (-1) * ((-n) ** (1/3))
                else n ** (1/3)
