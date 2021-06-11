module Tutorial where

import           Test.QuickCheck

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + (length' xs)

take' :: Integer -> [a] -> [a]
take' 0 list = []
take' n (x:xs) = (x : take' (n-1) xs)

takeLenProperty list = take' (length' list) list == list
{-
  ! Base Case:
  * when list := []
  * Prove: take' (length' []) [] == []
  | take' (length' []) [] == []
  | take' (0) [] == []
  | [] == []
  | True

  ! Induction Hypothesis
  * Assume: take' (length' list) list == list
  * Prove: take' (length' (h:t)) (h:t) == (h:t)

  ! Induction Step
  | take' (length' (h:t)) (h:t) == (h:t)
  | take' (1 + (length' t)) (h:t) == (h:t)
  | h : (take' (1 + length t - 1) xs) == (h:t)
  | h : (take' (length t) t) == (h:t)
  | (h:t) == (h:t)
-}

mean :: [Integer] -> Integer
mean xs = (sum xs) `div` (length' xs)

meanProp xs = length' xs > 0 ==> mean xs * length' xs <= sum xs

sum':: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sumProp (xs, ys) = sum' (xs ++ ys) == sum' xs + sum' ys

{-
  ! Base Case:
  * when xs == []
  * Prove: sum' ([] ++ ys) == sum' [] + sum' ys
  | sum' ([] ++ ys) == sum' [] + sum' ys
  | sum' ([] ++ ys) == 0 + sum' ys
  | sum' ([] ++ ys) == sum' ([] ++ ys)

  ! Induction Hypothesis
  * Assume: sum' (xs ++ ys) == sum' xs + sum' ys
  * Lemma: sum' (a:(as ++ bs)) == sum' ((a:as) ++ bs)
  * Prove: sum' ((x:xs) ++ ys) == sum' (x:xs) + sum' ys

  ! Induction Step
  | sum' ((x:xs) ++ ys) == sum' (x:xs) + sum' ys
  | sum' ((x:xs) ++ ys) == (x + sum' xs) + sum' ys
  | sum' ((x:xs) ++ ys) == x + sum' (xs ++ ys)
  | sum' ((x:xs) ++ ys) == sum' (x:(xs++ ys))
  | sum' ((x:xs) ++ ys) == sum' ((x:xs)++ ys) --> By the Lemma Case
  ? This proof is true given that Lemma is True
-}