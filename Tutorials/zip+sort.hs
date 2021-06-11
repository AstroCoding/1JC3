module Lib where

productPrime :: [Int] -> Int
productPrime [] = 1
productPrime (h:t) = h * (productPrime t)
--productPrime nums = (head nums) * (productPrime $ tail nums)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h:t) = (reverseList t) ++ [h]

zipPrime :: [a] -> [b] -> [(a,b)]
zipPrime [] _ = []
zipPrime _ [] = []
zipPrime (h:t) (x:xs) = [(h,x)] ++ zipPrime (t) (xs)

dropPrime :: Int -> [a] -> [a]
dropPrime _ [] = []
dropPrime 0 list = list
dropPrime x (h:t)
    | x > 0 = dropPrime (x - 1) (t)
    | otherwise = error "First parameter of dropPrime cannot be a negative number"

partition :: (a -> Bool) -> [a] -> ([a],[a])
partition predicate list = (satisfaction,failure) where
    satisfaction = filter predicate list
    failure = filter (not . predicate) list

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (h:b:t)
    | h > b = b : bubbleSort (h:t)
    | h <= b = h : bubbleSort (b:t)

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort list = sort $ bubbleSort list