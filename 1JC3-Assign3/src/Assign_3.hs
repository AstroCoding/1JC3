{- Assignment 3
 - Name: Mark Hutchison
 - Date: October 27, 2019
-}
module Assign_3 where

import qualified Data.Map.Strict as IM

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
data Poly a
  = X
  | Coef a
  | Sum (Poly a) (Poly a)
  | Prod (Poly a) (Poly a)
  deriving (Show)

newtype PolyList a =
  PolyList [a]
  deriving (Show)

{-
  ! polyValue
  * Description:
  ? Given a polynomial and a value, map the value onto the polynomial using the custom data type "Poly" provided above.
  ? For example, we know (x + 1) * (x + 2) = (2) + (3 * x) + (1 * x * x)
  ? However, the polynomials are not in this format. They are in the format of the Poly Data Type.
  ? For example:
  | (x+1)*(x+2) would be the following:
  | Prod (Sum X (Coef 1)) (Sum X (Coef 2))
-}
polyValue :: Num a => Poly a -> a -> a
polyValue (Coef z) n           = z
polyValue X n                  = n
polyValue (Sum polyA polyB) n  = (polyValue polyA n) + (polyValue polyB n)
polyValue (Prod polyA polyB) n = (polyValue polyA n) * (polyValue polyB n)

{-
  ! polyListValue
  * Description:
  ? A polynomial list consists of the coefficients of the simplified expression. For example, let's use (x+1)*(x+2) again.
  | (x+1) * (x+2) = 2 + 3*x + x^2
  ? Looking at the equation above, we can see the coefficients go [2,3,1]. That is the polynomial list. This list can also be represented as the following:
  | Sum (Sum (Coef 2) (Prod (Coef 3) (X))) (Prod (Coef 3) (Prod X X)))
  ? As you get further in the list, the amount of times you multiply X together goes up.
  * NOTE: The order of a polynomial list is CRUCIAL. It must go from the lowest degree to the highest degree. Meaning x^2 + 3x + 2 is not in the correct order and must be rearranged to be in 2 + 3x + x^2 format.
  | This will be accomplished using the general formula given in the assignment 3 pdf file.
-}
polyListValue :: (Num a, Eq a) => PolyList a -> a -> a
polyListValue (PolyList []) n = 0
polyListValue (PolyList p1) n = (head p1) + (n * (polyListValue (PolyList (tail p1)) n))

{-
 ! polyListSum
 * Description:
 ? Since tutorial told us to just take advantage of the tools given to us, I decided to listen to that advice and use the zipWith function. All it does is takes a function and two lists and applies the function on the two lists item by item, saving it as a final list.
 ? The problem is that this does not account for when the lists are of different sizes. So, to fix this, I just made them the same size using guarded statements. If the lists were not the same size, add a 0 to the end of the list and try again.
 * Note: It was brought to my attention that there is a function called zipWithPadding in which does what I did below. Given the problem however, I felt it was easier to define my own guarded statements as I know how each function and will react in any situation.
 * Note: The reason I am using the trimPolyListZeros function is explained in the commenting of polyListProd. Please refer to that explanation.
-}
polyListSum :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList p1) (PolyList q1)
  | p1 == [] && q1 == [] = PolyList []
  | (length p1) == (length q1) =
    PolyList (trimPolyListZeros (zipWith (+) p1 q1))
  | (length p1) < (length q1) = polyListSum (PolyList (p1 ++ [0])) (PolyList q1)
  | (length p1) > (length q1) = polyListSum (PolyList p1) (PolyList (q1 ++ [0]))

{-
 ! polyListDegree
 * Description:
 ? The degree of the list is the length of the list in which the highest degree is not equal to 0 minus one. So the degree of [2,3,1] is 2 and the degree of [1,0,4,0,5,0] is 4. There are 0 x^5s, so we actually count up to the x^4 digit.
 ? So what I will do is reverse the list and find when the head isn't 0. Once I have that, I will slice the list down to the correct size by using the init function. Once the list is the right size, all I have to do is length - 1.
 ? However, Haskell fights me whenever I try to take the length of p1 because of the PolyList type class. Instead of trying to find a way around this problem, I just found a different solution: Use an Auxiliary function that basically does what length does. And so I did.
-}
polyListDegree :: (Num a, Eq a) => PolyList a -> Integer
polyListDegree (PolyList []) =
  error "Degree of Zero Polynomial List is Undefined"
polyListDegree (PolyList p1) =
  if (trimPolyListZeros p1 == []) then 0
    else polyListDegreeAux (PolyList (trimPolyListZeros p1)) 0

polyListDegreeAux :: (Num a, Eq a) => PolyList a -> Integer -> Integer
polyListDegreeAux (PolyList []) degree = (degree - 1)
polyListDegreeAux (PolyList p1) degree =
  polyListDegreeAux (PolyList (drop (1) (p1))) (degree + 1)

{-
 ! polyListProd
 * Description:
 ? Multiplication is only intimidating when you think about. I took out a pencil and paper and just started writing though and it was simply. Every time you multiply a number by x, the degree of x increase by one. Therefore, you can just concatenate a 0 to the front of the list you're multiplying to every time the function runs. Anything times 0 is 0 and you have effectively increased the degree by one each time. Just add the many different lists we create using the existing polyListSum function and you get a working multiplier.
 * Note: PolyList [0,0,0] is the same as PolyList []. So I will use the trimPolyListZeros function to find this case and not change it. Because polyListProd uses the polyListSum function, I will be putting it in that function. Thus also repairing this error in that function.

 -}
polyListProd :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList []) (PolyList q1) = (PolyList [])
polyListProd (PolyList p1) (PolyList []) = (PolyList [])
polyListProd (PolyList p1) (PolyList q1) =
  polyListSum
    (PolyList (map (* (head p1)) q1))
    (polyListProd (PolyList (tail p1)) (PolyList ([0] ++ q1)))

trimPolyListZeros :: (Num a, Eq a) => [a] -> [a]
trimPolyListZeros [] = []
trimPolyListZeros [0] = []
trimPolyListZeros polyList
  | last polyList == 0 = trimPolyListZeros (init polyList)
  | otherwise = polyList

{-
 ! polyListToPoly
 * Description:
 ? Given the Polynomial List [2,3,1], convert it back into Sum (Coef 2) (Sum (Prod (Coef 3) (Prod (Coef 1) (X))) (Prod (Coef 1) (Prod (X) (X))))
 ? The first part is easy, simply take the first item and make it the Coef in a sum.
 ? because I now have a list of [3,1] after the first loop, we know that the degree of this list will be the degree of the last item in the list.
 ? So every time the function calls itself, I will get the product statement that calculates X to the power of the degree, done in a second function below, and multiply it by the head of the reverse list.
 ? Repeat with the init of the list (the tail of the reverse), until you're simply adding the final item because it is x^0.
 * Note: For my guarded statement of (last p1 == 0): This was added because the polyListDegree function slices 0s out of it. Meaning that if I had left it in, this would cause an infinite loop. Additionally, if a value is 0, it is insignificant to the final polynomial, meaning it can just be skipped.
 --------------------------------------------------------------------------------------------------------------------------------------------------------------
  * This version of the function adds Eq a in order to prevent more errors and provide shorter answers
 | polyListToPoly :: (Num a, Eq a) => PolyList a -> Poly a
 | polyListToPoly (PolyList []) = Coef 0
 | polyListToPoly (PolyList [head]) = (Coef head)
 | polyListToPoly (PolyList p1)
 |   | last p1 == [0] = (polyListToPoly (PolyList (init p1)))
 |   | otherwise =
 |     Sum
 |       (polyListToPoly (PolyList (init p1)))
 |       (Prod (Coef (last p1)) (xToThePower (polyListDegree (PolyList p1))))
-}

polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList []) = Coef 0
polyListToPoly (PolyList [head]) = Coef head
polyListToPoly (PolyList p1) = Sum (polyListToPoly (PolyList (init p1))) (Prod (Coef (last p1)) (xToThePower (toInteger ((length p1) - 1))))


xToThePower :: Num a => Integer -> (Poly a)
xToThePower 1      = (X)
xToThePower 2      = Prod (X) (X)
xToThePower degree = Prod (X) (xToThePower (degree - 1))

{-
  ! polyToPolyList
  * Description:
  ? Given any polynomial, make a polynomial list. Meaning I should be able to give (x+1)*(x+2) and still get [2,3,1]...
  ? This will be accomplished in a similar style to the first function, but in the exact reverse.
-}
polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a
-- The most simple cases imaginable: when Poly is the non-recursive data types
polyToPolyList (Coef n) = PolyList (trimPolyListZeros [n])
polyToPolyList X = PolyList [0, 1]
-- The recursive function for the recursive Data Constructors
polyToPolyList (Sum (polyA) (polyB)) =
  polyListSum (polyToPolyList (polyA)) (polyToPolyList (polyB))
polyToPolyList (Prod (polyA) (polyB)) =
  polyListProd (polyToPolyList (polyA)) (polyToPolyList (polyB)) -- And these cover all cases that the input poly can be
{-
  * polyValue
  ? - Function: polyValue
  ? - Test Case Number: 1
  ? - Input: (Prod (Sum (X) (Coef 1)) (Sum (X) (Coef 2))) 3
  ? - Expected Output: 20
  ? - Actual Output: 20

  ? - Function: polyValue
  ? - Test Case Number: 2
  ? - Input: (Sum (Prod (Coef 3) X) X) 5.75
  ? - Expected Output: 23.0
  ? - Actual Output: 23.0

  ? - Function: polyValue
  ? - Test Case Number: 3
  ? - Input: (Sum (Sum (Sum (Sum (Coef 0) (Prod (Coef 2) X)) (Prod (Coef 0) X)) (Prod (Coef (-3)) (Prod X (Prod X X)))) (Prod (Coef 2) (Prod X (Prod X (Prod X X))))) 7
  ? - Expected Output: 3787
  ? - Actual Output: 3787
-}
{-
  * polyListValue
  ? - Function: polyListValue
  ? - Test Case Number: 4
  ? - Input: (PolyList [2,3,1]) 3
  ? - Expected Output: 20
  ? - Actual Output: 20

  ? - Function: polyListValue
  ? - Test Case Number: 5
  ? - Input: (PolyList [0,4]) 5.75
  ? - Expected Output: 23.0
  ? - Actual Output: 23.0

  ? - Function: polyListValue
  ? - Test Case Number: 6
  ? - Input: (PolyList [0,2,0,-3,2]) 7
  ? - Expected Output: 3787
  ? - Actual Output: 3787
-}
{-
  * polyListSum
  ? - Function: polyListSum
  ? - Test Case Number: 7
  ? - Input: (PolyList [2,3,1])  (PolyList [1,5,3,4])
  ? - Expected Output: PolyList [3,8,4,4]
  ? - Actual Output: PolyList [3,8,4,4]

  ? - Function: polyListSum
  ? - Test Case Number: 7
  ? - Input: (PolyList [0,1,0,5])  (PolyList [2,0,2.75,3])
  ? - Expected Output: PolyList [2.0,1.0,2.75,8.0]
  ? - Actual Output: PolyList [2.0,1.0,2.75,8.0]

  ? - Function: polyListSum
  ? - Test Case Number: 7
  ? - Input: (PolyList [0,0,0,0,1])  (PolyList [1,1,1,1])
  ? - Expected Output: PolyList [1,1,1,1,1]
  ? - Actual Output: PolyList [1,1,1,1,1]
-}
{-
  * polyListDegree
  ? - Function: polyListDegree
  ? - Test Case Number: 8
  ? - Input: (PolyList [2,3,1,0,0])
  ? - Expected Output: 2
  ? - Actual Output: 2

  ? - Function: polyListDegree
  ? - Test Case Number: 9
  ? - Input: (PolyList [2,3,1,3,1,0,9])
  ? - Expected Output: 6
  ? - Actual Output: 6

  ? - Function: polyListDegree
  ? - Test Case Number: 10
  ? - Input: (PolyList [0,0,0,0,0,0])
  ? - Expected Output: 0
  ? - Actual Output: 0
-}
{-
  * polyListProd
  ? - Function: polyListProd
  ? - Test Case Number: 11
  ? - Input: (PolyList [2,3,1]) (PolyList [1,5,3,4])
  ? - Expected Output: PolyList [2,13,22,22,15,4]
  ? - Actual Output: PolyList [2,13,22,22,15,4]

  ? - Function: polyListProd
  ? - Test Case Number: 12
  ? - Input: (PolyList [1,1]) (PolyList [2,1])
  ? - Expected Output: PolyList [2,3,1]
  ? - Actual Output: PolyList [2,3,1]

  ? - Function: polyListProd
  ? - Test Case Number: 13
  ? - Input: (PolyList []) (PolyList [2,1])
  ? - Expected Output: PolyList []
  ? - Actual Output: PolyList []
-}
{-
  * polyListToPoly
  ? - Function: polyListToPoly
  ? - Test Case Number: 14
  ? - Input: (PolyList [2,3,1])
  ? - Expected Output: Sum (Sum (Coef 2) (Prod (Coef 3) X)) (Prod (Coef 1) (Prod X X))
  ? - Actual Output: Sum (Sum (Coef 2) (Prod (Coef 3) X)) (Prod (Coef 1) (Prod X X))

  ? - Function: polyListToPoly
  ? - Test Case Number: 15
  ? - Input: (PolyList [2,0,9])
  ? - Expected Output: Sum (Sum (Coef 2) (Prod (Coef 0) X)) (Prod (Coef 9) (Prod X X))
  ? - Actual Output: Sum (Sum (Coef 2) (Prod (Coef 0) X)) (Prod (Coef 9) (Prod X X))

  ? - Function: polyListToPoly
  ? - Test Case Number: 16
  ? - Input: (PolyList [1,2,3,4,5,6,7])
  ? - Expected Output: Sum (Sum (Sum (Sum (Sum (Sum (Coef 1) (Prod (Coef 2) X)) (Prod (Coef 3) (Prod X X))) (Prod (Coef 4) (Prod X (Prod X X)))) (Prod (Coef 5) (Prod X (Prod X (Prod X X))))) (Prod (Coef 6) (Prod X (Prod X (Prod X (Prod X X)))))) (Prod (Coef 7) (Prod X (Prod X (Prod X (Prod X (Prod X X))))))
  ? - Actual Output: Sum (Sum (Sum (Sum (Sum (Sum (Coef 1) (Prod (Coef 2) X)) (Prod (Coef 3) (Prod X X))) (Prod (Coef 4) (Prod X (Prod X X)))) (Prod (Coef 5) (Prod X (Prod X (Prod X X))))) (Prod (Coef 6) (Prod X (Prod X (Prod X (Prod X X)))))) (Prod (Coef 7) (Prod X (Prod X (Prod X (Prod X (Prod X X))))))
-}
{-
  * polyToPolyList
  ? - Function: polyToPolyList
  ? - Test Case Number: 17
  ? - Input: Sum (Sum (Coef 2) (Prod (Coef 3) X)) (Prod (Coef 1) (Prod X X))
  ? - Expected Output: PolyList [2,3,1]
  ? - Actual Output: PolyList [2,3,1]

  ? - Function: polyToPolyList
  ? - Test Case Number: 18
  ? - Input: Sum (Coef 2) (Prod (Coef 9) (Prod X X))
  ? - Expected Output: PolyList [2,0,9]
  ? - Actual Output: PolyList [2,0,9]

  ? - Function: polyToPolyList
  ? - Test Case Number: 19
  ? - Input: Sum (Coef 0) (Prod (Sum (Coef 1) (X)) (Sum (Coef 2) (X)))
  ? - Expected Output: PolyList [2,3,1]
  ? - Actual Output: PolyList [2,3,1]
-}
