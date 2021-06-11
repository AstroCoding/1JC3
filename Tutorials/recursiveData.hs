module Main where

type Point = (Int, Int)

data Rainbow
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet

data Shape
  = Circle Int
  | Rectangle Int Int

-- data Maybe a
--   = Just a
--   | Nothing
safeIntegerDivision :: Int -> Int -> Maybe Int
safeIntegerDivision x 0 = Nothing
safeIntegerDivision x y = Just (x `div` y)

data NaturalNumbers
  = Zero
  | Successor NaturalNumbers
  deriving (Show)
  {-
    * 3 ==> (+1 (+1 (+1 0)))
    *   ==> (Successor (Successor (Successor Zero)))
    * Let's teach it to translate it back into 3
  -}

naturalNumbersToInt :: NaturalNumbers -> Int
naturalNumbersToInt Zero          = 0
naturalNumbersToInt (Successor x) = (naturalNumbersToInt x) + 1

intToNaturalNumbers :: Int -> NaturalNumbers
intToNaturalNumbers 0 = Zero
intToNaturalNumbers x = Successor (intToNaturalNumbers (x - 1))

sum' :: NaturalNumbers -> NaturalNumbers -> NaturalNumbers
sum' n Zero          = n
sum' Zero n          = n
sum' m (Successor n) = Successor (sum' m n)

data Expression
  = Val Int
  | Add Expression Expression
  | Multiply Expression Expression
  {-
    * 1 + (2 * 3) ==>
    * Add (Val 1) (Multiplication (Val 2) (Val 3))
  -}

expr = Add (Val 1) (Multiply (Val 2) (Val 3))

evaluate :: Expression -> Int
evaluate (Val x)        = x
evaluate (Add x y)      = (evaluate x) + (evaluate y)
evaluate (Multiply x y) = (evaluate x) * (evaluate y)

main :: IO ()
main = do
  print (safeIntegerDivision 4 2)
  print (naturalNumbersToInt (Successor (Successor (Successor Zero))))
  print (intToNaturalNumbers 3)
  print
    (naturalNumbersToInt (sum' (intToNaturalNumbers 3) (intToNaturalNumbers 3)))
