module MyModule where

unzip' :: [(a, b)] -> ([a], [b])
unzip' ls = ([a | (a, b) <- ls], [b | (a, b) <- ls])

find' :: (Eq a) => a -> [(a, b)] -> [b]
find' search ls = [b | (a, b) <- ls, a == search]

concat' :: [[a]] -> [a]
concat' ls = [x | xs <- ls, x <- xs]

perfect :: Integral a => a -> Bool
perfect n = sum [x | x <- [1 .. n - 1], n `mod` x == 0] == n

cut :: Eq a => [a] -> [a]
cut []          = []
cut (head:tail) = head : cut [y | y <- tail, y /= head]

data Person
  = PersonConstructor
      { name   :: String
      , mother :: Person
      , father :: Person
      }
  | Unknown
  deriving (Show)

type People = [Person]

maternalTree :: Person -> People
maternalTree Unknown = []
maternalTree (PersonConstructor n Unknown f) = maternalTree f
maternalTree (PersonConstructor n m f) = [m] ++ maternalTree m ++ maternalTree f

position :: (Eq a) => a -> [a] -> [Int]
position element list =
  [y | (x, y) <- zip list [1 .. length list], element == x]
