module Abstract where

import           Test.QuickCheck

add []     = 0
add (x:xs) = x + add xs

subt []     = 0
subt (x:xs) = x - subt xs

mult []     = 1
mult (x:xs) = x * mult xs

and' []     = True
and' (x:xs) = x && and' xs

concat' []     = []
concat' (x:xs) = x ++ concat' xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f v []     = v
foldl' f v (x:xs) = f (foldl' f v xs) x

length' ls = foldr' (\x y -> 1 + y) 0 ls

{-
  * This lambda expression is recursive because foldr' is recursive on that function until it is 0. So it is the head of ls + y which is the head of the tail of ls and so on...
-}
data BinTree a
  = Node (BinTree a) (BinTree a) a
  | Leaf a
  deriving (Show, Eq)

tree = (Node (Node (Leaf 3) (Leaf 8) 5) (Node (Leaf 4) (Leaf 1) 2) 9)

treeMap :: (a -> b) -> BinTree a -> BinTree b
treeMap f (Leaf a)       = Leaf (f a)
treeMap f (Node t1 t2 a) = Node (treeMap f t1) (treeMap f t2) (f a)

treeFold :: (a -> b -> b) -> b -> BinTree a -> b
treeFold f v (Leaf a) = f a v
treeFold f v (Node t1 t2 a) =
  let t1Value = (treeFold f v t1)
      aValue = (f a v)
   in treeFold f aValue t2

map' f ls = foldr (\x y -> (f x) : y) [] ls

list_filter p ls =
  foldr
    (\x y ->
       if p x
         then x : y
         else y)
    []
    ls
