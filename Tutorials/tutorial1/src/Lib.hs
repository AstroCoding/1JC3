module Lib
    (
    someString, double, quadruple
    ) where

someString :: String
someString = "Hello, My Name is Mark." ++ " How may I help you?"

double :: Int -> Int
double x = x + x

quadruple :: Int -> Int
quadruple y = 4 * y
