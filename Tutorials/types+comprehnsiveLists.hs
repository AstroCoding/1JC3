module Main where

factorial :: Integer -> Integer
factorial n
    | n == 0    = 1
    | n > 0     = n * factorial (n - 1)
    | otherwise = error "Factorials of negative numbers do not exist."

factorial' :: Integer -> Integer
factorial' n = product [1..n]

product' :: [Integer] -> Integer
product' []     = 1
product' (x:xs) = x * product' xs

-- * {x^2 | x in {1..5}} == map (^2) [1..5] == [x^2 | x <- [1..5]]
-- ? Use a comma to then filter the domain by a function
-- * [x^2 | x <- [1..5], even x]
-- * map (^2) (filter even [1..10]) == map (^2) $ filter even [1..10]

-- ! Comprehension
-- * [ ${Pattern} | ${Definitions} <- ${Application}]

type Point = (Int, Int)
type Point2 = Pair Int
type Pair a = (a, a)

data TrafficLight = Green | Yellow | Red
data Maybe a = Just a
             | Nothing

origin :: Point
origin = (0, 0)

left :: Point -> Point
left (x,y) = (x - 1, y)

factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], n `mod` x == 0]

main :: IO()
main = do
    print (factorial 5)
    print (factorial' 5)
    print (product' [1..5])
    print ([x^2 | x <- [1..5]])