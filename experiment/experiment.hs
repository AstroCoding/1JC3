-- Comments like this
{- Or they are like this -}

{- Types -}
-- Int -2^63 -> 2^63
-- Integer -> No limits
-- Float or Double -> [bigFloat or Double]
-- Char -> Single Quotes
-- Bool -> True or False
-- Tuple -> List with many different data types

{- Functions for Math -}
-- Sum [1..1000]
-- +, -, *, /
-- Prefix: mod 5 4 || Infix: 5 `mod` 4
-- Negatives in Parenthesis
-- sqrt Float
-- fromIntegral && round
-- pi, exp, log, ** (^), truncate, ceiling, floor
-- sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
-- not(True)

-- Now for actual code
{-
Reverse Words:
import Data.Char

main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-}

{-
Name Teller:
main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    putStrLn $ "Hey " ++ firstName ++ " " ++ lastName ++ ", how are you?"
-}

