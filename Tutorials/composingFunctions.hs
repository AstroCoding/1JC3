module MyModule where

import           Data.Char
import           Data.List

{-
  * Functions can be combined exactly like mathematical notations.
  * They have their own rules.

  | f ( g x ) = f . g x

  * foldMap [3,10,4] = (3 + 1) + (10 + 1) + (4 + 1) = 4 + 11 + 5 = 20
-}
notThat list = foldr (+) (0) (map (+ 1) list)

-- * this can be written as
whichIs ls = ((foldr (+) 0) . (map (+ 1))) ls

-- * which simplifies to:
doThis = foldr (+) 0 . map (+ 1)

{-
  | See how it does not have an input in the function, but still knows it needs an input.

  * (.) :: (b -> c) -> (a -> b) -> a -> c
  * f . g = \x -> f (g x)

  -- | Now let's talk $
-}
-- | jimmy xs ys = foldr max 0 (filter even (map (+ 1) (concat (xs : [ys]))))
-- * becomes
jimmy xs ys = foldr max 0 $ filter even $ map (+ 1) $ concat $ xs : [ys] -- * see how much prettier this is?

-- | Even without rainbow colours...
{-
  ? Start from the inside and work your way out.
  ? Some $ can not be removed.

  | Now let's undo $ by using |>!
  * (|>) :: a -> (a -> b) -> b
  * (|>) x f = f x
  -- | All this does is reverse the order of the function!
  ? g = putStrLn $ map toLower $ concat $ intersperse " " ["Hello", "Goodbye"]
  ? g' = ["Hello", "Goodbye"] |> intersperse " " |> concat |> map toLower |> putStrLn
-}
{-
  ! Association
  * Function Application is left associative:
  | f x y == (f x) y
  | f x y /= f (x y)
  * The function symbol is right associative:
  | a -> b -> c == a -> (b -> c)
  | a -> b -> c /= (a -> b) -> c
-}
f1 :: Integral a => [a] -> [a]
f1 xs = filter even (map succ xs)

f2 :: Integral a => [a] -> [a]
f2 xs = filter even $ map succ xs

f3 :: Integral a => [a] -> [a]
f3 = filter even . map succ

f4 :: Integral a => [a] -> [a]
f4 = \xs -> filter even $ map succ xs

data Student =
  Student
    { name       :: String
    , identifier :: Int
    }
  deriving (Show)

-- * Create a list of Students given a [String] and [Int]
-- | name $ Student "Mark" 3 = "Mark"
-- | :t Student = Student :: String -> Int -> Student
students :: [String]
students = ["Mark", "Daniel", "Mihir"]

identifiers :: [Int]
identifiers = [1, 2, 3]

listStudents :: [String] -> [Int] -> [Student]
listStudents names ids = zipWith (Student) names ids
