module Types where

{-
  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    a /= b = not (a == b)
    a == b = not (a /= b)

  instance Eq Bool where
    a == b = (a && b) || not (a || b)

  instance Eq a => Eq (Maybe a) where
    (Just a) == (Just b) = a == b
    Nothing == Nothing = True
    _ == _ = False

-- * Also see Functor
-- * Let's do a quick Vector Type Class
-}
class VectorSpace v where
  vecZero :: Num a => v a
  vecSum :: Num a => v a -> v a -> v a
  vecScalarProd :: Num a => a -> v a -> v a
  vecMagnitude :: Floating a => v a -> a

newtype Vector2D a =
  Vector2D (a, a)
  deriving (Show)

newtype Vector3D a =
  Vector3D (a, a, a)
  deriving (Show)

instance VectorSpace Vector2D where
  vecZero = Vector2D (0, 0)
  vecSum (Vector2D (x0, y0)) (Vector2D (x1, y1)) = Vector2D (x0 + x1, y0 + y1)
  vecScalarProd n (Vector2D (x, y)) = Vector2D (n * x, n * y)
  vecMagnitude (Vector2D (x, y)) = sqrt $ (x ** 2) + (y ** 2)

instance VectorSpace Vector3D where
  vecZero = Vector3D (0, 0, 0)
  vecSum (Vector3D (x0, y0, z0)) (Vector3D (x1, y1, z1)) =
    Vector3D (x0 + x1, y0 + y1, z0 + z1)
  vecScalarProd n (Vector3D (x, y, z)) = Vector3D (n * x, n * y, n * z)
  vecMagnitude (Vector3D (x, y, z)) = sqrt $ (x ** 2) + (y ** 2) + (z ** 2)

vecDiff :: (VectorSpace v, Num a) => v a -> v a -> v a
vecDiff vec1 vec2 = vecSum vec1 $ vecScalarProd (-1) vec2
