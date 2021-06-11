{-
  ! Assignment 4
  * Date: 05/11/19
  * Name: Mark Hutchison
 -}
module Assign_4 where

macid :: String
macid = "hutchm6"

{-
 * Disclaimer:
 | I have a VS Code Extension called 'Better Comments' in which allow me to colour code my comments based off of the symbol I place at the beginning of each line. I use these various colours in order to keep track of all of the information I am typing and quickly find something in a comment.
 * For Explanation:
 ! - ! means title
 * - * means Header, Section Name, or Important Note
 ? - ? means information or explanation
 | - | means note or general information / other

 ? If I have a conflicting comment symbol, I will use another in its place to keep them clear and show they are not related to each other.
 | This system was inspired by my use of the Markdown language for ReadMe files and note taking.
 | https://marketplace.visualstudio.com/items?itemName=aaron-bond.better-comments
-}
data MathExpr a
  = X
  | Coef a
  | Sum (MathExpr a) (MathExpr a)
  | Prod (MathExpr a) (MathExpr a)
  | Quot (MathExpr a) (MathExpr a)
  | Exp (MathExpr a)
  | Log (MathExpr a)
  deriving (Eq, Show, Read)

{-
 ! value
 * Description:
 ? Value takes in an expression and evaluates it. This is literally a copy paste of polyValue from assignment three, but with more cases and changing function names...
 | n / 0 = Infinity in Haskell, so we have to make it return an error instead.
-}
value :: (Floating a, Eq a) => MathExpr a -> a -> a
value (Coef a) n   = a
value X n          = n
value (Sum a b) n  = (value a n) + (value b n)
value (Prod a b) n = (value a n) * (value b n)
value (Quot a b) n = (value a n) / (value b n)
value (Exp a) n    = exp $ value a n
value (Log a) n    = log $ value a n

{-
 ! simp
 * Description:
 ? When you have a massive expression, a lot of it is pretty useless. We all know the simple steps, n + 0 = 1, n * 0 = 0, etc...
 ? This function takes the simplest cases of each expression possibility and converts an expression of these cases to a more simple form.
 ? The function can either return a Coef if possible, or just returns a simplified expression that isn't just a number.
-}
simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp (Coef n) = Coef n
simp X = X
simp (Sum (Coef 0.0) v) = simp v
simp (Sum u (Coef 0.0)) = simp u
simp (Sum u v) =
  let uPrime = simp u
      vPrime = simp v
   in if uPrime == u && vPrime == v
        then Sum u v
        else simp $ Sum uPrime vPrime
simp (Prod (Coef 0.0) v) = Coef 0.0
simp (Prod u (Coef 0.0)) = Coef 0.0
simp (Prod (Coef 1.0) v) = simp v
simp (Prod u (Coef 1.0)) = simp u
simp (Prod u v) =
  let uPrime = simp u
      vPrime = simp v
   in if uPrime == u && vPrime == v
        then Prod u v
        else simp $ Prod uPrime vPrime
simp (Quot u (Coef 1.0)) = simp u
simp (Quot u v) =
  let uPrime = simp u
      vPrime = simp v
   in if uPrime == u && vPrime == v
        then Quot u v
        else simp $ Quot uPrime vPrime
simp (Exp (Coef 0.0)) = Coef 1.0
simp (Exp u) =
  let uPrime = simp u
   in if uPrime == u
        then Exp u
        else simp $ Exp uPrime
simp (Log (Coef 1.0)) = Coef 0.0
simp (Log u) =
  let uPrime = simp u
   in if uPrime == u
        then Log u
        else simp $ Log uPrime

{-
 ! diff
 * Description:
 ? Differentiating is used to find the slope of a function at a specific point. If complicated enough, the function will return another function designed for this slope at a point on the graph.
 ? Differentiation is something all calculus students have to learn. We all know the formulas - and if we forgot, they were given to us - so let's just implement them nice and easy.
-}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = Coef 1.0
diff (Coef n) = Coef 0.0
diff (Sum a b) = Sum (diff a) (diff b)
diff (Prod a b) =
  let a' = diff a
      b' = diff b
   in Sum (Prod a' b) (Prod a b')
diff (Quot a b) =
  let a' = diff a
      b' = diff b
   in Quot (Sum (Prod a' b) (Prod (Coef (-1.0)) (Prod a b'))) (Prod b b)
diff (Exp a) = Prod (Exp a) (diff a)
diff (Log a) = Quot (diff a) a

{-
 ! readDiffWrite
 * Description:
 ? readDiffWrite takes in 2 FilePaths. The function will find the 1st file given and read it.
 ? If that file contains a mathematically expression, then the function will read it, differentiate it, simplify it, and write it to the second file.
 ? If the file is empty, nothing should happen and an error should be displayed.
-}
readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite fileA fileB =
  if (fileA == fileB)
    then error "The input and output files must be different."
    else do
      fileContents <- readFile fileA
      if fileContents == ""
        then error "Your input file is empty."
        else writeFile fileB $ -- Write to fileB
             unlines $ -- Convert back into file lines format
             map show $ -- Show each MathExpr Double as String
             map simp $ -- Simplify each derivative
             map diff $ -- Differentiate each MathExprDouble
             map (read :: String -> MathExpr Double) (lines fileContents) -- Read each line as MathExpr Double
-- ! Test Cases
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
  ? Expected Output: Quot (Sum (Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))) (Coef 0.0)) (Sum (Prod X X) (Coef (-1.0)))
  ? Actual Output: Quot (Sum (Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))) (Coef 0.0)) (Sum (Prod X X) (Coef (-1.0)))

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
  ? Expected Output File Contents: Quot (Sum X X) (Sum (Prod X X) (Coef (-1.0))) \n
  ? Actual Output File Contents: Quot (Sum X X) (Sum (Prod X X) (Coef (-1.0))) \n

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
-- ! QuickCheck Properties
{-
  ? Function: value
  ? Property: value (Sum (Prod (Coef 1.0) X) (Prod (Coef 1.0) X)) n == 2 * n
  ? Result: Passed 100 Test Cases
  | Explanation: The value of 1*n + 1*n is always 2*n, no matter the value of n.

  ? Function: value
  ? Property: n > 0 ==> value (Prod (Exp (Coef 0.0)) (Log X)) n == log n
  ? Result: Passed 100 Test Cases
  | Explanation: As long as the domain of Log is respected, 1 * Log n will always be log n.
  * Note: Exp (Coef 0.0) == Coef 1.0

  ? Function: value
  ? Property: n > 0 ==> value (Prod (Exp (Coef 0.0)) (Log X)) n == log n
  ? Result: Passed 100 Test Cases
  | Explanation: Because log 1 = 0, anything added to 0 will be that value

  ? Function: value
  ? Property: n /= 0 ==> value (Quot X X) n == 1.0
  ? Result: Passed 100 Test Cases
  | Explanation: The value of n / n is always 1.0, no matter the value of n as long as n does not equal 0.
  * This was added because I had to test Quot as it was the only property left out.
-}
{-
  ? Function: simp
  ? Property: (n /= 0) ==> value (simp (Quot (Prod (Exp (Coef 0.0)) X) (Sum (Log (Coef 1.0)) X))) n == 1.0
  ? Result: Passed 100 Test Cases
  | Explanation: The numerator and the denominators will both simplify down to X. The result of X / X is always 1, unless x is 0.

  ? Function: simp
  ? Property:  value (simp $ Prod (Coef 1.0) (Sum (X) (X))) n == value (Prod (Coef 2.0) (X)) n
  ? Result: Passed 100 Test Cases
  | Explanation: The value of a simplified expression at n is the same as the value the non simplified expression at n.

  ? Function: simp
  ? Property:  a /= 0 && b /= 0 ==> Sum (Coef a) (Coef b) == simp (Sum (Prod (Coef 1.0) (Coef a)) (Prod (Coef 1.0) (Coef b)))
  ? Result: Passed 100 Test Cases
  | Explanation: Due to how Exp works and Prod works in simplification, it simplifies down to Sum (Coef a) (Coef b)
-}
{-
  ? Function: diff
  ? Property: value (diff . simp $ Prod (Coef 1.0) (Sum (X) (X))) n == value (diff $ Prod (Coef 2.0) (X)) n
  ? Result: Passed 100 Test Cases
  | Explanation: The value of the derivate at n on a simplified expression is the same as the value of the first derivate at n on the non simplified expression.

  ? Function: diff
  ? Property: diff (Coef n) == Coef 0.0
  ? Result: Passed 100 Test Cases
  | Explanation: Every constant's derivative is 0.0.

  ? Function: diff
  ? Property: (n /= 0) ==> value (diff (Quot (Prod (Coef n) X) (Prod (Coef n) X))) n == 1.0
  ? Result: Passed 100 Test Cases
  | Explanation: This simplifies to the Coef 1.0, and the value derivative of a coef is always 0.

  ? Function: diff
  ? Property: (a > 0) ==> value (simp $ diff (Exp (Coef a))) n == value (simp $ diff (Log (Coef a))) n
  ? Result: Passed 100 Test Cases
  | Explanation: diff Exp (Coef a) is 0, diff Log (Coef a) is 0, therefore, no matter the value of n, the values will always equal each other (0.0).
-}
