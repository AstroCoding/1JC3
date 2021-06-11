module MyModule where

data DivExpr
  = Val Int
  | Div DivExpr DivExpr

safeDiv :: Int -> Int -> Maybe Int
safeDiv m n
  | n == 0 = Nothing
  | otherwise = Just (m `div` n)

-- * Short Form
eval :: DivExpr -> Maybe Int
eval (Val n) = return n
eval (Div x y) = do
  m <- eval x
  n <- eval y
  safeDiv m n

-- * Long Form
evalPrime :: DivExpr -> Maybe Int
evalPrime (Val n) = Just n
evalPrime (Div x y) =
  case eval x of
    Nothing -> Nothing
    Just m ->
      case eval y of
        Nothing -> Nothing
        Just n  -> safeDiv m n

data LogExpr
  = Value Float
  | Log LogExpr

safeLog :: Float -> Maybe Float
safeLog n
  | n <= 0 = Nothing
  | otherwise = Just (log n)

evalLog :: LogExpr -> Maybe Float
evalLog (Value n) = return n
evalLog (Log x) = do
  m <- evalLog x
  safeLog m
