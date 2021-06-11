module Testing where

marten :: Float -> [Float] -> Float -> Float
marten x [] z = z
marten x (y:ys) z =
  let w = abs (x - y)
   in if abs w < z
        then marten x ys w
        else marten x ys z

identifyFunction :: a -> a
identifyFunction x = x

hadrian :: Integer -> Maybe Integer
hadrian n =
  let claudius :: Integer -> Integer -> Integer -> Maybe Integer
      claudius a b c
        | a < 0 = Nothing
        | a == 0 = Just b
        | a > 0 = claudius (a - 1) c (b + c)
   in claudius n 0 1

marcus :: Eq a => [a] -> [a] -> [a]
marcus xs [] = xs
marcus xs (y:ys)
  | y `elem` ys = marcus xs ys
  | otherwise = marcus (xs ++ [y]) ys

commodus :: Eq a => [a] -> [a] -> Bool
commodus s t
  | length s /= length t = False
  | s == [] && t == [] = True
  | (head s) == (head t) = commodus (tail s) (tail t)
  | (head s) `elem` t = commodus s ((tail t) ++ [head t])
  | otherwise = False

nero :: Eq a => [a] -> [a] -> Bool
nero s t =
  let s' = marcus [] s
      t' = marcus [] t
   in commodus s' t'

caligula f = do
  a <- readFile f
  let b = lines a
  let c =\ map read b :: [Double]
  return c
   -- quadratic a b c =
--   if discriminant < 0
--     then error "0"
--     else (x, y)
--   where
--     x = constant + sqrt discriminant / (2 * a)
--     y = constant - sqrt discriminant / (2 * a)
--     discriminant = b * b - 4 * a * c
--     constant = -b / (2 * a)
