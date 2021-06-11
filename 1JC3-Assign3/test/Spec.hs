import           Test.QuickCheck

data Poly a
  = X
  | Coef a
  | Sum (Poly a) (Poly a)
  | Prod (Poly a) (Poly a)
  deriving (Show)

polyValue :: Num a => Poly a -> a -> a
polyValue (Coef z) n           = z
polyValue X n                  = n
polyValue (Sum polyA polyB) n  = (polyValue polyA n) + (polyValue polyB n)
polyValue (Prod polyA polyB) n = (polyValue polyA n) * (polyValue polyB n)

xToThePower :: Num a => Integer -> (Poly a)
xToThePower 1      = (X)
xToThePower 2      = Prod (X) (X)
xToThePower degree = Prod (X) (xToThePower (degree - 1))

testCase degree value =
  degree >= 1 ==> polyValue (xToThePower degree) value == value ^ degree
