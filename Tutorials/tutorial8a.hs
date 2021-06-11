module Tutorial where

import           Test.QuickCheck

add1 :: [Int] -> [Int]
add1 []     = []
add1 (x:xs) = (x + 1) : add1 xs

mult2 :: [Int] -> [Int]
mult2 []     = []
mult2 (x:xs) = (x * 2) : add1 xs

manipulate_list :: (a -> b) -> [a] -> [b]
manipulate_list _ []     = []
manipulate_list f (x:xs) = (f x) : manipulate_list f xs

-- * Definition of map function
mapPrime f list = [f head | head <- list]

alternateMapPrime f []          = []
alternateMapPrime f (head:tail) = (f head) : alternateMapPrime f tail

filter' p xs = [x | x <- xs, p x]

filterPrime :: (a -> Bool) -> [a] -> [a]
filterPrime p [] = []
filterPrime p (head:tail)
  | p head = head : filterPrime p tail
  | otherwise = filterPrime p tail

pairs :: [a] -> [(a, a)]
pairs list = zip list (tail list)

sorted ls = and [x <= y | (x, y) <- pairs ls]

positions value list = [y | (x, y) <- (zip list [0 .. len]), x == value]
  where
    len = (length list) - 1
