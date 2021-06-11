module MyModule where

data BinTree a
  = Leaf a
  | Node a (BinTree a) (BinTree a)

exampleTree = Node 5 (Node 3 (Leaf 2) (Leaf 4)) (Node 7 (Leaf 6) (Leaf 8))

treePaths :: BinTree a -> [[a]]
treePaths (Leaf x) = [[x]]
treePaths (Node x left right) =
  (map (x :) $ treePaths left) ++ (map (x :) $ treePaths right)

treeFold op u (Node x t1 t2) =
  let u' = x `op` u''
      u'' = treeFold op u t2
   in treeFold op u' t1
treeFold op u (Leaf x) = x `op` u

flattenTree :: BinTree a -> [a]
flattenTree tree = treeFold (:) [] tree

data Tree a =
  TNode a [Tree a]

newtype Positive =
  Positive Integer
  deriving (Show)

fromIntList :: [Integer] -> [Positive]
fromIntList ls = map Positive . filter (\x -> x > 0)
