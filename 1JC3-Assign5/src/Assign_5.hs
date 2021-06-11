{-
  ! Assignment 5
  * Name: Mark Hutchison
  * Date: TODO add of completion
-}
module Assign_5 where

macid :: String
macid = "hutchm6"

-- * Disclaimer: Due to this project reliance on Floating Point arithmetic, answers will not be exact
-- * All test cases that have an expected percent error will document that percent error.
{-
 ! definiteIntegral
 * Description:
 ? The definite integral of a function finds the area under the curve of that function on the domain [a,b]
 ? To do this, we will spit the area under the curve into trapezoids with their own areas.
 ? Next, we are going to use the following property of definite integrals from calculus class:
 | Integral of a -> b + Integral of b -> c = Integral a -> c
 ? To simulate this rule, I  will be split the integral of n trapezoids into n integrals of 1 trapezoid length.
 ? This is an easy recursive function and saves me from having to implement Sigma Notation.
 * Note: Sigma notation is probably the better approach in the long run as it uses the beliefs of Little Languages,
 * but given the scope won't change, I will take the easier road.
-}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n
  | n < 0 = error "Number of Trapezoids must be positive."
  | a > b = (-1) * definiteIntegral b a g n
  | a == b || n == 0 = 0
  | otherwise =
    let deltaX = (b - a) / fromIntegral n
        nextA = a + deltaX
        fraction = ((g a) + (g nextA)) / 2
     in (fraction * deltaX) + definiteIntegral (nextA) b g (n - 1)

{-
 ! funH
 * Description:
 ? This function finds the distance between the two curves x^a and x^(1/a) [0,1]. Because x^(1/a) is always greater than x^a between 0 and 1, this is just a straight forward integral calculation.
 | We also know that: definiteIntegral f(x) - definiteIntegral g(x) = definiteIntegral (f(x) - g(x)) from Calculus Class
 * Note: The variable a used is not representing the starting point of the Integral
 ? 100 appears to be the most accurate value of n within the most reasonable amount of time spent calculating on a variety of machines and also easiest on paper.
-}
funH :: Integer -> Double
funH a
  | a > 0 = definiteIntegral 0 1 (\x -> x ** (1 / fromIntegral a) - x ** (fromIntegral a)) 100
  | otherwise = undefined

{-
 ! funK
 * Description:
 ? This function is supposed to find the area under the curve of y=a^x on the interval [-1,1], where the user inputs an a value. Two things:
 | a must be positive for this to work
 | and n has to be really big to simulate Infinity, but not too big to cause Stack Overflow.
 * Note: The variable a used is not representing the starting point of the Integral
 ? 100 appears to be the most accurate value of n within the most reasonable amount of time spent calculating on a variety of machines and also easiest on paper.
-}
funK :: Double -> Double
funK a
  | a > 0 = definiteIntegral (-1) 1 (\x -> a ** x) 100
  | otherwise = undefined
-- ! Test Cases
{-
  * definiteIntegral
  ? Function: definiteIntegral
  ? Test Case Number: 1
  ? Input: 0.0 3.0 (\x -> 3.0) 3
  ? Expected Output: 9.0
  ? Actual Output: 9.0

  ? Function: definiteIntegral
  ? Test Case Number: 2
  ? Input: 5.0 0.0 (\x -> x) 1
  ? Expected Output: -12.5
  ? Actual Output: -12.5

  ? Function: definiteIntegral
  ? Test Case Number: 3
  ? Input: 4.0 2.0 (\x -> x**3) -2
  ? Expected Output: error "Number of Trapezoids must be positive."
  ? Actual Output: error "Number of Trapezoids must be positive."
-}
{-
  * To check the more difficult Trapezoidal Rule answers, I used https://www.emathhelp.net/calculators/calculus-2/trapezoidal-rule-calculator/?f=x+%5E+%281%2F2%29+-+x+%5E+2&a=0&b=1&n=100&steps=on
  * After all, it's exam week and we need to study hard.
  * funH
  ? Function: funH
  ? Test Case Number: 4
  ? Input: 0
  ? Expected Output: Prelude.undefined
  ? Actual Output: Prelude.undefined

  ? Function: funH
  ? Test Case Number: 5
  ? Input: 1
  ? Expected Output: 0.0
  ? Actual Output:   0.0

  ? Function: funH
  ? Test Case Number: 6
  ? Input: 2
  ? Expected Output: 0.333112973821287
  ? Actual Output: 0.3333266343936808
  | Percent Error: 0.064%
-}
{-
  * To check the more difficult Trapezoidal Rule answers, I used https://www.emathhelp.net/calculators/calculus-2/trapezoidal-rule-calculator/?f=x+%5E+%281%2F2%29+-+x+%5E+2&a=0&b=1&n=100&steps=on
  * After all, it's exam week and we need to study hard.
  * funK
  ? Function: funK
  ? Test Case Number: 7
  ? Input: 0
  ? Expected Output: Prelude.undefined
  ? Actual Output: Prelude.undefined

  ? Function: funK
  ? Test Case Number: 8
  ? Input: 10
  ? Expected Output: 4.30027129081648
  ? Actual Output: 4.299522969370294
  | Percent Error: -0.0174%

  ? Function: funK
  ? Test Case Number: 9
  ? Input: 1000
  ? Expected Output: 144.994867600213
  ? Actual Output: 144.7669851117216
  | Percent Error: 0.1572%
-}
-- ! Quick Checks
{-
  ? Function: definiteIntegral
  ? Property: n > 0 ==> definiteIntegral 0 doubleN (\x -> doubleN) n == doubleN ** 2
  ? Result: Passed 100 Test Cases
  | Explanation: The area under a curve from a -> n of y = n is always n ^ 2 as long as n is a whole number
  | DoubleN: This represents the Integer as a Double since definiteIntegral's type is Double -> Double -> (Double -> Double) -> Integer -> Double

  ? Function: definiteIntegral
  ? Property: (n > 0) ==> definiteIntegral a b (\x -> 3.0) n == (-1) * definiteIntegral b a (\x -> 3.0) n
  ? Result: Passed 100 Test Cases
  | Explanation: The area under the curve in one direction is the same as the negative area of the curve in the opposite direction

  ? Function: definiteIntegral
  ? Property: (n > 0) ==> (n > 0) ==> round (definiteIntegral a b (\x -> x + 3.0) n) == round (definiteIntegral a b (\x -> 3.0) n + definiteIntegral a b (\x -> x) n)
  ? Result: Passed 100 Test Cases
  | Explanation: The sum of the area of 2 functions is the area of the two functions combined
  | Round: We round because floating point addition is really shoty.
  * A rewrite without the round function can be found in the Assign_5_Test.hs incase this is invalid.
-}
{-
  ? Function: funH
  ? Property: (k > 0) ==> round (funH k) == round (definiteIntegral 0 1 (\x -> x ** (1 / fromIntegral k)) 100 - definiteIntegral 0 1 (\x -> x ** (fromIntegral k)) 100)
  ? Result: Passed 100 Test Cases
  | Explanation: The area between 2 functions is the area of the two functions difference - Remember, there are 2 ways to define funH. This proves that.
  | Round: We round because floating point addition is really shoty.
  * A rewrite without the round function can be found in the Assign_5_Test.hs incase this is invalid.

  ? Function: funH
  ? Property: (a < b && a > 0 && b > 0) ==> funH a < funH b
  ? Result: Passed 100 Test Cases
  | Explanation: Because the limit of funH approaches 1 as the input gets bigger, we know that this holds true.
  | Round: We round because floating point addition is really shoty.
-}
{-
  ? Function: funK
  ? Property: (a < b && a > 1 && b > 1) ==> funH a < funH b
  ? Result: Passed 100 Test Cases
  | Explanation: Because the limit of funH approaches Infinity as the input gets bigger, we know that this holds true while being above 1.
  | Round: We round because floating point addition is really shoty.
-}
