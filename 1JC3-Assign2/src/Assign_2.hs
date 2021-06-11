{-
 ! Assignment 2
 * Name: Mark Hutchison
 * Date: October 6th, 2019
-}
module Assign_2 where

macid :: String
macid = "hutchm6"

type GaussianInt = (Integer, Integer)

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
-}
{-
 ! gaussReal
 * Description:
 ? gaussReal is to return the real part of a Gaussian Number that consists of a real and imaginary number.
 ? For example, if a Gaussian Number is in the format of (a + bi), the real number is a.
-}
gaussReal :: GaussianInt -> Integer
gaussReal (x, y) = x

{-
 ! gaussImag
 * Description:
 ? gaussReal is to return the imaginary part of a Gaussian Number that consists of a real and imaginary number.
 ? For example, if a Gaussian Number is in the format of (a + bi), the imaginary number is b.
-}
gaussImag :: GaussianInt -> Integer
gaussImag (x, y) = y

{-
 ! gaussConj
 * Description:
 ? If an gaussian number is in the format of (a + bi), then the conjugate is (a - bi). So all we need to do is turn b into a negative number.
 ? When you add a conjugate to it's original gaussian number, You get (a1 + a1, b1i - b1i), in which just returns 2 * a1. This eliminates the imaginary number all together.
-}
gaussConj :: GaussianInt -> GaussianInt
gaussConj g = ((gaussReal g), -(gaussImag g))

{-
 ! gaussAdd
 * Description:
 ? When given two gaussian numbers in the format of (a + bi), adding them together is as simple as adding the two a values and the two b values separately. They then form a new gaussian number.
-}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd g0 g1 = (gReal, gImag)
  where
    gReal = (gaussReal g0) + (gaussReal g1)
    gImag = (gaussImag g0) + (gaussImag g1)

{-
 ! gaussMult
 * Description:
 ? Gaussian numbers don't multiply the same way as they add. Instead, they apply the FOIL rules.
 ? (a1 * a2) + (a1 * b2i) + (b1i * a2) + (b1i * b2i)
 ? The hard part getting the final result to be in Gaussian Format. We know that i^2 is (-1) because (sqrt x) ^ 2 is x and i = sqrt (-1), so i^2 is (-1)
 ? So when doing (b1i * b2i) does not result in an imaginary number. So it would be part of the real number in the final Gaussian Number
-}
gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult g0 g1 = (gReal, gImag)
  where
    a1 = gaussReal g0
    b1 = gaussImag g0
    a2 = gaussReal g1
    b2 = gaussImag g1
    gReal = (a1 * a2) - (b1 * b2)
    gImag = (a1 * b2) + (b1 * a2)

{-
 ! gaussNorm
 * Description:
 ? When a Gaussian Number is Multiplied with it's conjugate and the constants in front of the real and imaginary value are added, you end up with a Gaussian Number that is in the middle of the two; the Gaussian Number's Normal.
 ? All we need to do is create the GaussianInt in which contains the real and imaginary numbers that are being added together and add those two parts together.
-}
gaussNorm :: GaussianInt -> Integer
gaussNorm g = norm
  where
    finalGaussNum = gaussMult g (gaussConj g)
    norm = gaussReal finalGaussNum + gaussImag finalGaussNum

{-
 ! maxGaussNorm
 * Description:
 ? Considering we have already done maxInList and minInList during our lectures, this function will be a piece of cake. First, maxGaussNorm will split the list into a head and a tail so that way each can be managed individually. The head will become the maximum, and through each call of its auxiliary function, the maximum will be replaced or kept depending on its normal.
 ? Next, the auxiliary function. The Auxiliary will do all the work. If the normal of the head of the list is bigger then the current normal of the maximum, the head of the list becomes the new maximum. The reason we are not making the normal the new maximum is because we need to return a GaussianInt at the end of the recursive loop. If the normal of the head is smaller or the same, we will simply ignore it and run the function again with the same maximum GaussianInt. Once the list is empty, we know we are finished.
 ? There is really no difference between this and the maxInList function covered in lecture except that maxInList was comparing Int to Int without the aid of another function.
-}
maxGaussNorm :: [GaussianInt] -> GaussianInt
maxGaussNorm [] = (0,0)
maxGaussNorm startingList = maxGaussNormAux (tail startingList) (head startingList)

maxGaussNormAux :: [GaussianInt] -> GaussianInt -> GaussianInt
maxGaussNormAux gaussList max
  | gaussList == [] = max
  | (gaussNorm $ head gaussList) > gaussNorm max =
    maxGaussNormAux (tail gaussList) (head gaussList)
  | (gaussNorm $ head gaussList) <= gaussNorm max =
    maxGaussNormAux (tail gaussList) max
{-
 ! Test Cases
 | Below are my test cases for each of the functions defined above.
 * Note: maxGaussNormAux is a part of maxGaussNorm and isn't its own separate test case.

 | If all of these match the cases I have done on hand, i will feel confident I am correct. That being said, I will use an online application to verify my test cases and I will also be attempting to form a quicktest proof to just verify. However, in this document, you only see my test cases given below.
-}
{-
 * gaussConj
 ? - Function: gaussConj
 ? - Test Case Number: 1
 ? - Input: (1,3)
 ? - Expected Output: (1,-3)
 ? - Actual Output: (1,-3)

 ? - Function: gaussConj
 ? - Test Case Number: 2
 ? - Input: (7,-8)
 ? - Expected Output: (7,8)
 ? - Actual Output: (7,8)

 ? - Function: gaussConj
 ? - Test Case Number: 3
 ? - Input: (4,-(-2))
 ? - Expected Output: (4,-2)
 ? - Actual Output: (4,-2)
-}
{-
 * gaussAdd
 ? - Function: gaussAdd
 ? - Test Case Number: 4
 ? - Input: (2,5) (9,-2)
 ? - Expected Output: (11,3)
 ? - Actual Output: (11,3)

 ? - Function: gaussAdd
 ? - Test Case Number: 5
 ? - Input: (-3,-5) (-9,-2)
 ? - Expected Output: (-12,-7)
 ? - Actual Output: (-12,-7)

 ? - Function: gaussAdd
 ? - Test Case Number: 6
 ? - Input: (3,5) (-9,-2)
 ? - Expected Output: (-6,3)
 ? - Actual Output: (-6,3)

 ? - Function: gaussAdd
 ? - Test Case Number: 7
 ? - Input: (NaN,5) (0,2)
 ? - Expected Output: error
 ? - Actual Output: error

 ? - Function: gaussAdd
 ? - Test Case Number: 8
 ? - Input: (3*(sqrt 2),5) ((4/3),2)
 ? - Expected Output: error
 ? - Actual Output: error
-}
{-
 * gaussMult
 ? - Function: gaussMult
 ? - Test Case Number: 9
 ? - Input: (3,7) (2,-5)
 ? - Expected Output: (41,-1)
 ? - Actual Output: (41,-1)

 ? - Function: gaussMult
 ? - Test Case Number: 10
 ? - Input: (4,3) (4,-3)
 ? - Expected Output: (25,0)
 ? - Actual Output: (25,0)

 ? - Function: gaussMult
 ? - Test Case Number: 11
 ? - Input: (4,0) (0,-3)
 ? - Expected Output: (0,-12)
 ? - Actual Output: (0,-12)
-}
{-
 * gaussNorm
 ? - Function: gaussNorm
 ? - Test Case Number: 12
 ? - Input: (4,3)
 ? - Expected Output: 25
 ? - Actual Output: 25

 ? - Function: gaussNorm
 ? - Test Case Number: 13
 ? - Input: (0,-12)
 ? - Expected Output: 144
 ? - Actual Output: 144

 ? - Function: gaussNorm
 ? - Test Case Number: 14
 ? - Input: (0,0)
 ? - Expected Output: 0
 ? - Actual Output: 0
-}
{-
 * maxGaussNorm
 ? - Function: maxGaussNorm
 ? - Test Case Number: 15
 ? - Input: [(1,1),(4,3),(2,2)]
 ? - Expected Output: (4,3)
 ? - Actual Output: (4,3)

 ? - Function: maxGaussNorm
 ? - Test Case Number: 16
 ? - Input: [(2,1),(7,8),(1,3),(8,7)]
 ? - Expected Output: (7,8)
 ? - Actual Output: (7,8)

 ? - Function: maxGaussNorm
 ? - Test Case Number: 17
 ? - Input: [(2,1),(7,8),(NaN,NaN),(8,7)]
 ? - Expected Output: error
 ? - Actual Output: error
-}
