{-
! Assignment 4 Tests
 * Name: Mark Hutchison
 * Date: 16/11/19
 -}
import           Assign_4

-- * I haven't done Extra Credit, so it caused an error due to lack of content.
-- import           Assign_4_ExtraCredit
import           Test.QuickCheck

{-
  ? Function: value
  ? Property: value (Sum (Prod (Coef 1.0) X) (Prod (Coef 1.0) X)) n == 2 * n
  ? Result: Passed 100 Test Cases
  | Explanation: The value of 1*a + 1*n is always a + n, no matter the value of n and a.
-}
valueProp1 :: Double -> Double -> Bool
valueProp1 a n =
  value (Sum (Prod (Coef 1.0) (Coef a)) (Prod (Coef 1.0) X)) n == a + n

{-
  ? Function: value
  ? Property: n > 0 ==> value (Prod (Exp (Coef 0.0)) (Log X)) n == log n
  ? Result: Passed 100 Test Cases
  | Explanation: As long as the domain of Log is respected, 1 * Log n will always be log n.
  * Note: Exp (Coef 0.0) == Coef 1.0
-}
valueProp2 :: Double -> Property
valueProp2 n = n > 0 ==> value (Prod (Exp (Coef 0.0)) (Log X)) n == log n

{-
  ? Function: value
  ? Property: n > 0 ==> value (Prod (Exp (Coef 0.0)) (Log X)) n == log n
  ? Result: Passed 100 Test Cases
  | Explanation: Because log 1 = 0, anything added to 0 will be that value
-}
valueProp3 :: Double -> Bool
valueProp3 n = value (Sum (Exp X) (Log (Coef 1.0))) n == exp n

{-
  ? Function: value
  ? Property: n /= 0 ==> value (Quot X X) n == 1.0
  ? Result: Passed 100 Test Cases
  | Explanation: The value of n / n is always 1.0, no matter the value of n as long as n does not equal 0.
  * This was added because I had to test Quot as it was the only property left out.
-}
valueProp4 :: Double -> Property
valueProp4 n = (n /= 0) ==> value (Quot X X) n == 1.0

{-
  ? Function: simp
  ? Property: (n /= 0) ==> value (simp (Quot (Prod (Exp (Coef 0.0)) X) (Sum (Log (Coef 1.0)) X))) n == 1.0
  ? Result: Passed 100 Test Cases
  | Explanation: The numerator and the denominators will both simplify down to X. The result of X / X is always 1, unless x is 0.
-}
simpProp1 :: Double -> Property
simpProp1 n =
  (n /= 0) ==>
  value (simp (Quot (Prod (Exp (Coef 0.0)) X) (Sum (Log (Coef 1.0)) X))) n ==
  1.0

{-
  ? Function: simp
  ? Property:  value (simp $ Prod (Coef 1.0) (Sum (X) (X))) n == value (Prod (Coef 2.0) (X)) n
  ? Result: Passed 100 Test Cases
  | Explanation: The value of a simplified expression at n is the same as the value the non simplified expression at n.
-}
simpProp2 :: Double -> Bool
simpProp2 n =
  value (simp $ Prod (Coef 1.0) (Sum (X) (X))) n ==
  value (Prod (Coef 2.0) (X)) n

{-
  ? Function: simp
  ? Property:  a /= 0 && b /= 0 ==> Sum (Coef a) (Coef b) == simp (Sum (Prod (Coef 1.0) (Coef a)) (Prod (Coef 1.0) (Coef b)))
  ? Result: Passed 100 Test Cases
  | Explanation: Due to how Exp works and Prod works in simplification, it simplifies down to Sum (Coef a) (Coef b)
-}
simpProp3 :: Double -> Double -> Property
simpProp3 a b =
  a /= 0 && b /= 0 ==> Sum (Coef a) (Coef b) ==
  simp (Sum (Prod (Coef 1.0) (Coef a)) (Prod (Exp (Coef 0.0)) (Coef b)))

{-
  ? Function: diff
  ? Property: value (diff . simp $ Prod (Coef 1.0) (Sum (X) (X))) n == value (diff $ Prod (Coef 2.0) (X)) n
  ? Result: Passed 100 Test Cases
  | Explanation: The value of the derivate at n on a simplified expression is the same as the value of the first derivate at n on the non simplified expression.
-}
diffProp1 :: Double -> Bool
diffProp1 n =
  value (diff . simp $ Prod (Coef 1.0) (Sum (X) (X))) n ==
  value (diff $ Prod (Coef 2.0) (X)) n

{-
  ? Function: diff
  ? Property: diff (Coef n) == Coef 0.0
  ? Result: Passed 100 Test Cases
  | Explanation: Every constant's derivative is 0.0.
-}
diffProp2 :: Double -> Bool
diffProp2 n = diff (Coef n) == Coef 0.0

{-
  ? Function: diff
  ? Property: (n /= 0) ==> value (diff (Quot (Prod (Coef n) X) (Prod (Coef n) X))) n == 1.0
  ? Result: Passed 100 Test Cases
  | Explanation: This simplifies to the Coef 1.0, and the value derivative of a coef is always 0.
-}
diffProp3 :: Double -> Property
diffProp3 n =
  (n /= 0) ==> value (diff (Quot (Prod (Coef n) X) (Prod (Coef n) X))) n == 0.0

{-
  ? Function: diff
  ? Property: (a > 0) ==> value (simp $ diff (Exp (Coef a))) n == value (simp $ diff (Log (Coef a))) n
  ? Result: Passed 100 Test Cases
  | Explanation: diff Exp (Coef a) is 0, diff Log (Coef a) is 0, therefore, no matter the value of n, the values will always equal each other (0.0).
-}
diffProp4 :: Double -> Double -> Property
diffProp4 a n =
  (a > 0) ==> value (diff (Exp (Coef a))) n == value (diff (Log (Coef a))) n

main :: IO ()
main = do
  putStrLn "valueProp1:"
  quickCheck valueProp1
  putStrLn "valueProp2:"
  quickCheck valueProp2
  putStrLn "valueProp3:"
  quickCheck valueProp3
  putStrLn "valueProp4:"
  quickCheck valueProp4
  putStrLn "simpProp1:"
  quickCheck simpProp1
  putStrLn "simpProp2:"
  quickCheck simpProp2
  putStrLn "simpProp3:"
  quickCheck simpProp3
  putStrLn "diffProp1:"
  quickCheck diffProp1
  putStrLn "diffProp2:"
  quickCheck diffProp2
  putStrLn "diffProp3:"
  quickCheck diffProp3
  putStrLn "diffProp4:"
  quickCheck diffProp4
{-
  * value
  ? Function: value
  ? Test Case Number: 1
  ? Input: (Quot (Coef (-3.0)) X) 5.0
  ? Expected Output: -0.6
  ? Actual Output: -0.6

  ? Function: value
  ? Test Case Number: 2
  ? Input: (Prod (Log X) (Coef 3.0)) (-1.0)
  ? Expected Output: NaN
  ? Actual Output: NaN

  ? Function: value
  ? Test Case Number: 3
  ? Input: (Sum (Coef (-3.0)) (Prod (Coef (-4.0)) (X))) (-5.0)
  ? Expected Output: 17.0
  ? Actual Output: 17.0

  ? Function: value
  ? Test Case Number: 4
  ? Input: (Quot (Coef (125.0)) X) 0.0
  ? Expected Output: Infinity
  ? Actual Output: Infinity
  | Haskell treats 4.0/0.0 in limit formats as opposed to number format.
  | Big number / really small number = really big number, thus 125/0 = Infinity
-}
{-
  * simp
  ? Function: simp
  ? Test Case Number: 5
  ? Input: (Sum (Coef 0.0) (Coef 3.0))
  ? Expected Output: Coef 3.0
  ? Actual Output: Coef  3.0

  ? Function: simp
  ? Test Case Number: 6
  ? Input: Quot (X) (Coef 1.0)
  ? Expected Output: X
  ? Actual Output: X

  ? Function: simp
  ? Test Case Number: 7
  ? Input: Prod (Exp (Prod (Coef 0.0) (Coef 100.0))) (Sum (Coef 5.5) (Coef 7.5))
  ? Expected Output: Sum (Coef 5.5) (Coef 7.5)
  ? Actual Output: Sum (Coef 5.5) (Coef 7.5)
  | Proves Prod 0 _ = 0, Exp 0 = 1, and Prod 1 _ = _
-}
{-
  * diff
  ? Function: diff
  ? Test Case Number: 8
  ? Input: Log (Sum (Prod (X) (X)) (Coef (-1.0)))
  ? Expected Output: Quot (Sum (Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))) (Coef 0.0)) (Log (Sum (Prod X X) (Coef (-1.0))))
  ? Actual Output: Quot (Sum (Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))) (Coef 0.0)) (Log (Sum (Prod X X) (Coef (-1.0))))

  ? Function: diff
  ? Test Case Number: 9
  ? Input: (Coef 13.0)
  ? Expected Output: Coef 0.0
  ? Actual Output: Coef 0.0

  ? Function: diff
  ? Test Case Number: 10
  ? Input: Prod X X
  ? Expected Output: Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))
  ? Actual Output: Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))
-}
{-
  * readDiffWrite
  ? Function: readDiffWrite
  ? Test Case Number: 11
  ? Input: "../test/fileA.txt" "../test/fileB.txt"
  ? Input File Contents: Log (Sum (Prod (X) (X)) (Coef (-1.0)))
  ? Expected Output File Contents: Quot (Sum X X) (Log (Sum (Prod X X) (Coef (-1.0)))) \n
  ? Actual Output File Contents: Quot (Sum X X) (Log (Sum (Prod X X) (Coef (-1.0)))) \n

  ? Function: readDiffWrite
  ? Test Case Number: 12
  ? Input: "../test/fileA.txt" "../test/fileA.txt"
  ? Expected Output: error "The input and output files must be different."
  ? Actual Output: error "The input and output files must be different."

  ? Function: readDiffWrite
  ? Test Case Number: 13
  ? Input: "../test/fileA.txt" "../test/fileB.txt"
  ? Input File Contents: empty
  ? Expected Output: error "Your input file is empty"
  ? Actual Output: error "Your input file is empty"

  ? Function: readDiffWrite
  ? Test Case Number: 14
  ? Input: "../test/fileA.txt" "../test/fileB.txt"
  ? Input File Contents: This is an invalid string
  ? Expected Output: **Exception: Prelude.read: no parse
  ? Actual Output: **Exception: Prelude.read: no parse
-}
