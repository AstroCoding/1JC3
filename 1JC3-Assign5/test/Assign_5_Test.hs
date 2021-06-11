{- Assignment 5 Tests
 - Name: TODO add full name
 - Date: TODO add of completion
 -}
import           Assign_5

import           Test.QuickCheck

{-
  ? Function: definiteIntegral
  ? Property: n > 0 ==> definiteIntegral 0 doubleN (\x -> doubleN) n == doubleN ** 2
  ? Result: Passed 100 Test Cases
  | Explanation: The area under a curve from a -> n of y = n is always n ^ 2 as long as n is a whole number
  | DoubleN: This represents the Integer as a Double since definiteIntegral's type is Double -> Double -> (Double -> Double) -> Integer -> Double
-}
dIProp1 :: Integer -> Property
dIProp1 n =
  let doubleN = fromIntegral n
   in n > 0 ==> definiteIntegral 0 doubleN (\x -> doubleN) n == doubleN ** 2

{-
  ? Function: definiteIntegral
  ? Property: (n > 0) ==> definiteIntegral a b (\x -> 3.0) n == (-1) * definiteIntegral b a (\x -> 3.0) n
  ? Result: Passed 100 Test Cases
  | Explanation: The area under the curve in one direction is the same as the negative area of the curve in the opposite direction
-}
dIProp2 :: Double -> Double -> Integer -> Property
dIProp2 a b n =
  (n > 0) ==> definiteIntegral a b (\x -> 3.0) n == (-1) *
  definiteIntegral b a (\x -> 3.0) n

{-
  ? Function: definiteIntegral
  ? Property: (n > 0) ==> (n > 0) ==> round (definiteIntegral a b (\x -> x + 3.0) n) == round (definiteIntegral a b (\x -> 3.0) n + definiteIntegral a b (\x -> x) n)
  ? Result: Passed 100 Test Cases
  | Explanation: The sum of the area of 2 functions is the area of the two functions combined
  | Round: We round because floating point addition is really shoty.
-}
dIProp3 :: Double -> Double -> Integer -> Property
dIProp3 a b n =
  (n > 0) ==> round (definiteIntegral a b (\x -> x + 3.0) n) ==
  round (definiteIntegral a b (\x -> 3.0) n + definiteIntegral a b (\x -> x) n)

{-
  ? Function: definiteIntegral
  ? Property: (n > 0) ==> 0.001 > u && (-0.001) < u where u = (definiteIntegral a b (\x -> x + 3.0) n) - (definiteIntegral a b (\x -> 3.0) n + definiteIntegral a b (\x -> x) n)
  ? Result: Passed 100 Test Cases
  | Explanation: This is a rewrite of the dIProp3 without the round or floor functions.
-}
dIProp3v2 :: Double -> Double -> Integer -> Property
dIProp3v2 a b n = (n > 0) ==> 0.001 > u && (-0.001) < u
  where
    u =
      (definiteIntegral a b (\x -> x + 3.0) n) -
      (definiteIntegral a b (\x -> 3.0) n + definiteIntegral a b (\x -> x) n)

{-
  ? Function: funH
  ? Property: (k > 0) ==> round (funH k) == round (definiteIntegral 0 1 (\x -> x ** (1 / fromIntegral k)) 100 - definiteIntegral 0 1 (\x -> x ** (fromIntegral k)) 100)
  ? Result: Passed 100 Test Cases
  | Explanation: The area between 2 functions is the area of the two functions difference - Remember, there are 2 ways to define funH. This proves that.
  | Round: We round because floating point addition is really shoty.
-}
funHProp1 :: Integer -> Property
funHProp1 k =
  (k > 0) ==> round (funH k) ==
  round
    (definiteIntegral 0 1 (\x -> x ** (1 / fromIntegral k)) 100 -
     definiteIntegral 0 1 (\x -> x ** (fromIntegral k)) 100)

{-
  ? Function: funH
  ? Property: k > 0 ==> u > (-0.0001) && u < 0.0001 where u = funH k - (definiteIntegral 0 1 (\x -> x ** (1 / fromIntegral k)) 100 - definiteIntegral 0 1 (\x -> x ** (fromIntegral k)) 100)
  ? Result: Passed 100 Test Cases
  | Explanation: This is a better version of the previous function that doesn't rely on the potential flaws of the round and floor system.
-}
funHProp1v2 :: Integer -> Property
funHProp1v2 k = k > 0 ==> 0.001 > u && (-0.001) < u
  where
    u =
      funH k -
      (definiteIntegral 0 1 (\x -> x ** (1 / fromIntegral k)) 100 -
       definiteIntegral 0 1 (\x -> x ** (fromIntegral k)) 100)

{-
  ? Function: funH
  ? Property: (a < b && a > 0 && b > 0) ==> funH a < funH b
  ? Result: Passed 100 Test Cases
  | Explanation: Because the limit of funH approaches 1 as the input gets bigger, we know that this holds true.
  | Round: We round because floating point addition is really shoty.
-}
funHProp2 :: Integer -> Integer -> Property
funHProp2 a b = (a < b && a > 0 && b > 0) ==> funH a < funH b

{-
  ? Function: funK
  ? Property: (a < b && a > 1 && b > 1) ==> funH a < funH b
  ? Result: Passed 100 Test Cases
  | Explanation: Because the limit of funH approaches Infinity as the input gets bigger, we know that this holds true while being above 1.
  | Round: We round because floating point addition is really shoty.
-}
funKProp1 :: Double -> Double -> Property
funKProp1 a b = (a < b && a > 1 && b > 1) ==> funK a < funK b

{-
  ? Function: definiteIntegral and technically funK
  ? Property: (k > 0 && n > 0) ==> funK k - (definiteIntegral a b (\x -> x) n - definiteIntegral a b (\x -> x) n) == funK k
  ? Result: Passed 100 Test Cases
  | Explanation: we know dI a b = -dI b a; Thus, funK + 0 == funK, Slightly proving both weakly.
-}
funKProp2 :: Double -> Double -> Double -> Integer -> Property
funKProp2 k a b n =
  (k > 0 && n > 0) ==> funK k -
  (definiteIntegral a b (\x -> x) n + definiteIntegral b a (\x -> x) n) ==
  funK k

main :: IO ()
main = do
  putStrLn "Performing Test 1 - definiteIntegral:"
  quickCheck dIProp1
  putStrLn "Performing Test 2 - definiteIntegral:"
  quickCheck dIProp2
  putStrLn "Performing Test 3 - definiteIntegral:"
  putStrLn "Testing Version 1 - Round:"
  quickCheck dIProp3
  putStrLn "Testing Version 2 - Approximate Equality:"
  quickCheck dIProp3v2
  putStrLn "Performing Test 4 - funH:"
  putStrLn "Testing Version 1 - Round:"
  quickCheck funHProp1
  putStrLn "Testing Version 2 - Approximate Equality:"
  quickCheck funHProp1v2
  putStrLn "Performing Test 5 - funH:"
  quickCheck funHProp2
  putStrLn "Performing Test 6 - funK:"
  quickCheck funKProp1
  putStrLn "Performing Test 7 - funK:"
  quickCheck funKProp2
