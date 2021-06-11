data Poly a
  = X
  | Coef a
  | Sum (Poly a) (Poly a)
  | Prod (Poly a) (Poly a)
  deriving (Show)

newtype PolyList a =
  PolyList [a]
  deriving (Show)

intLength :: (Eq a) => [a] -> Integer
intLength []     = 0
intLength (x:xs) = 1 + intLength xs

polyListSum :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList p1) (PolyList q1)
  | p1 == [] && q1 == [] = PolyList []
  | (length p1) == (length q1) =
    PolyList (trimPolyListZeros (zipWith (+) p1 q1))
  | (length p1) < (length q1) = polyListSum (PolyList (p1 ++ [0])) (PolyList q1)
  | (length p1) > (length q1) = polyListSum (PolyList p1) (PolyList (q1 ++ [0]))

polyListProd :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList []) (PolyList q1) = (PolyList [])
polyListProd (PolyList p1) (PolyList []) = (PolyList [])
polyListProd (PolyList (x:xs)) (PolyList q1) =
  polyListSum
    (PolyList (prodConst x q1))
    (polyListProd (PolyList xs) (PolyList ([0] ++ q1)))

prodConst a zs = map (* a) zs

trimPolyListZeros :: (Num a, Eq a) => [a] -> [a]
trimPolyListZeros [] = []
trimPolyListZeros [0] = []
trimPolyListZeros polyList
  | last polyList == 0 = trimPolyListZeros (init polyList)
  | otherwise = polyList
