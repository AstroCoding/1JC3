module Tutorial where
import Test.QuickCheck

sumPrime :: [Integer] -> Integer
sumPrime [] = 0
sumPrime (x:xs) = x + sumPrime xs

sumPrimeProp :: [Integer] -> Bool
sumPrimeProp [x] = sumPrime [x] == x

{-
  ! Proof
  * sumPrime [x] == x
  | sumPrime [x]
  | x + sumPrime []
  | x + 0
  | x
-}

fact :: Integer -> Integer
fact x
  | x == 0 = 1
  | otherwise = x * fact (x - 1)

-- a = = > b means if a is true, then b must also be true
{-
a ==> b = case (a,b) of
  (True,False) -> False
  _ -> True
-}

factorialProp n = n > 0 ==> (fact n) `div` n == fact (n - 1)

{-
  ! Proof
  * (fact n) `div` n == fact (n - 1)
  | fact (n * (n - 1)) == fact (n - 1)
  | fact (n - 1) == fact (n - 1)
-}

{-
  ! Properties
  * => is not ==>
  | Base Case: Prove P(Base Case) outright
  | Induction Case: Prove P(xs) ==> P(x:xs)
-}

reversePrime [] = []
reversePrime (x:xs) = (reversePrime xs) ++ [x]

reverseProof x = reverse [x] == [x]
{-
  ! Bad Proof
  | reverse [x]
  | reverse [] ++ [x]
  | [x]

  ! Good Proof
  * Base Case
  | reverse (reverse []) == []
  | reverse ([]) == []
  | [] == []

  * Induction Steps
  * Assume that: reverse (reverse (xs)) == (xs)
  * Prove     :  reverse (reverse (x:xs)) == (x:xs)
  | reverse (reverse (x:xs)) == (x:xs)
  | reverse (xs ++ [x]) == (x:xs)
  | (reverse [x]) ++ (reverse (reverse xs)) == (x:xs)
  | [x] ++ xs == (x:xs)
  | (x:xs) == (x:xs)
-}

katyPerry = "drifting in the wind like a plastic bag " ++ katyPerry