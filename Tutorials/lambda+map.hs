module Lib where

head' :: [a] -> a
head' [] = error "Head isn't defined on empty lists."
head' (x : xs) = x

tail' :: [a] -> [a]
tail' [] = error "Tail isn't defined on empty lists."
tail' (x : xs) = xs

add :: Double -> Double -> Double
add x y = x + y

-- ! add in lambda
-- * \x -> x + \y -> y

printHello input
    | input == "Hello" = "Hello, Good Sir!"
    | input == "Screw You" = "Well screw you too."
    | otherwise = "Hello"

printHello' input = do
    if input == "Hello" then "Hello, Good Sir!"
    else if input == "Screw You" then "Well screw you too."
    else "Hello"

printHello'' input = case input of
    "Hello" -> "Hello, Goo Sir!"
    "Screw You" -> "Well screw you too."
    _ -> "Hello"


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' function (head : tail) = (function head) : (map' function tail)

init' :: [a] -> [a]
init' [] = error "Function cannot be run on an empty list."
init' [element] = []
init' (head:tail) = head : init' tail

main :: IO()
main = do
    let testing_list = [1,2,3,4,5,6,7,8,9]
    -- print((\(x : xs) -> x)([1,2,3,4]))
    -- print((\x y z -> x * y * z) 2 6 8)

    print(map' (+1) testing_list)
    print(init' testing_list)