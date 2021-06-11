module Main where

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x2, x1) (y2, y1) = let
        deltaX = x2 - x1
        deltaY = y2 - y1
    in
        sqrt (deltaX ** 2 + deltaY ** 2)

polynomialSquared :: Double -> Double -> Double
polynomialSquared x y =
    let
        z = x + y
    in
        z * z

main :: IO ()
main = do
    print(distance (4.0, 5.0) (4.0, 4.0))
    print(polynomialSquared 4 4)

-- ! Lists - []:
-- ? Haskell can only type check a homogeneous list. You cannot have a list in which contains different types. Basically, lists are type locked.
-- * Note: 'Char' and "String" are different and the correct quotations should be used.
-- * Functions: head, tail, init, last

-- ! Tuples - ():
-- ? Tuples can have multiple types within them. They can also contain lists within them.
-- * Functions: fst, snd