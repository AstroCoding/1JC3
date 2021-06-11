module Tut9b where

type GaussianInt = (Int, Int)

data Color
  = Red
  | Yellow
  | Green

data Student =
  Student String Int Bool

newtype Z1 =
  Z1 Int

newtype Z2 =
  Z2 [Int]

newtype S1 =
  S1 Student

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

split :: [a] -> ([a], [a])
split ls = (take half ls, drop half ls)
  where
    half = (length ls) `div` 2

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let (ys, zs) = split xs
   in merge (mergeSort ys) (mergeSort zs)

readMergeSortWrite :: FilePath -> FilePath -> IO ()
readMergeSortWrite fileA fileB =
  if fileA == fileB
    then error "Input and Output File cannot be the same File Path."
    else do
      fileContents <- readFile fileA
      if fileContents == ""
        then error "Input File is Empty."
        else writeFile fileB $
             unlines $
             map show $
             mergeSort $ map (read :: String -> Int) (lines fileContents)
