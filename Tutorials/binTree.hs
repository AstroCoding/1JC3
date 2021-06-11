module Main where

-- data BinTree a
--   = Leaf a
--   | Branch (BinTree a) a (BinTree a)
--   deriving (Show)
-- binTreeNodes :: BinTree a -> Integer
-- binTreeNodes (Leaf _)       = 1
-- binTreeNodes (Branch s _ t) = (binTreeNodes s) + 1 + (binTreeNodes t)
-- binTreeSum :: Num a => BinTree a -> a
-- binTreeSum (Leaf x)       = x
-- binTreeSum (Branch s x t) = (binTreeNodes s) + x + (binTreeNodes t)
data Tree
  = Leaf Int
  | Branch Tree Int Tree

occursIn :: Int -> Tree -> Bool
occursIn x (Leaf n)
  | x == n = True
  | otherwise = False
occursIn x (Branch left n right)
  | x == n = True
  | otherwise = (occursIn x left) || (occursIn x right)

occursInBST :: Int -> Tree -> Bool
occursInBST x (Leaf n)
  | x == n = True
  | otherwise = False
occursInBST x (Branch left n right)
  | x == n = True
  | x < n = (occursIn x left)
  | x > n = (occursIn x right)

exampleTree =
  (Branch (Branch (Leaf 6) 7 (Leaf 9)) 5 (Branch (Leaf 1) 3 (Leaf 4))) :: Tree

exampleBST =
  (Branch (Branch (Leaf 1) 3 (Leaf 4)) 5 (Branch (Leaf 6) 7 (Leaf 9))) :: Tree

-- data StudentID = StudentID String String
-- macID (StudentID mID _) = mID
-- studentNum (StudentID _ sID) = sID
data StudentID
  = StudentID
      { macID     :: String
      , studentID :: String
      }
  | FacultyID
      { madID      :: String
      , facultyNum :: String
      , salary     :: Float
      }

data List a
  = Cons a (List a)
  | Empty

data Lights
  = Red
  | Green
  | Yellow
  deriving (Show)

nextLight Red    = Green
nextLight Green  = Yellow
nextLight Yellow = Red

main :: IO ()
main = putStrLn "Go ahead, sir!"
