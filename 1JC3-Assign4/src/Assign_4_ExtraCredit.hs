{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}

{-
  ! Assignment 4 Extra Credit
 * Name: Mark Hutchison
 * Date:
-}
module Assign_4_ExtraCredit where

macid :: String
macid = "hutchm6"

data MathExpr a
  = X
  | Coef a
  | Sum (MathExpr a) (MathExpr a)
  | Prod (MathExpr a) (MathExpr a)
  | Quot (MathExpr a) (MathExpr a)
  | Exp (MathExpr a)
  | Log (MathExpr a)
  deriving (Eq, Show, Read)

{-
 ! value
 * Description:
 ? Value takes in an expression and evaluates it. This is literally a copy paste of polyValue from assignment three, but with more cases and changing function names...
 | n / 0 = Infinity in Haskell, so we have to make it return an error instead.
 -}
value :: (Floating a, Eq a) => MathExpr a -> a -> a
value (Coef a) n   = a
value X n          = n
value (Sum a b) n  = (value a n) + (value b n)
value (Prod a b) n = (value a n) * (value b n)
value (Quot a b) n = (value a n) / (value b n)
value (Exp a) n    = exp (value a n)
value (Log a) n    = log (value a n)

{-
 ! simp
 * Description:
 ? When you have a massive expression, a lot of it is pretty useless. We all know the simple steps, n + 0 = 1, n * 0 = 0, etc...
 ? This function takes the simplest cases of each expression possibility and converts an expression of these cases to a more simple form.
 ? The function can either return a Coef if possible, or just returns a simplified expression that isn't just a number.
-}
simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp (Coef n) = Coef n
simp X = X
simp (Sum (Coef a) (Coef b)) = Coef (a + b)
simp (Sum (Coef 0.0) v) = simp v
simp (Sum u (Coef 0.0)) = simp u
simp (Sum u v)
  | u == (Prod (Coef (-1.0)) v) ||
      u == (Prod v (Coef (-1.0))) ||
      v == (Prod (Coef (-1.0)) u) || v == (Prod u (Coef (-1.0))) = Coef 0.0
  | otherwise =
    let uPrime = simp u
        vPrime = simp v
     in if uPrime == u && vPrime == v
          then (Sum u v)
          else simp (Sum uPrime vPrime)
simp (Prod (Coef a) (Coef b)) = Coef (a * b)
simp (Prod (Coef 0.0) v) = Coef 0.0
simp (Prod u (Coef 0.0)) = Coef 0.0
simp (Prod (Coef 1.0) v) = simp v
simp (Prod u (Coef 1.0)) = simp u
simp (Prod u v) =
  let uPrime = simp u
      vPrime = simp v
   in if uPrime == u && vPrime == v
        then (Prod u v)
        else simp (Prod uPrime vPrime)
simp (Quot (Coef a) (Coef b)) = Coef (a / b)
simp (Quot u (Coef 1.0)) = simp u
simp (Quot (Coef 0.0) _) = Coef 0.0
simp (Quot u v) =
  if u == v
    then Coef 1.0
    else let uPrime = simp u
             vPrime = simp v
          in if uPrime == u && vPrime == v
               then (Quot u v)
               else simp (Quot uPrime vPrime)
simp (Exp (Coef 0.0)) = Coef 1.0
simp (Exp u) =
  let uPrime = simp u
   in if uPrime == u
        then (Exp u)
        else simp (Exp uPrime)
simp (Log (Coef 1.0)) = Coef 0.0
simp (Log u) =
  let uPrime = simp u
   in if uPrime == u
        then (Log u)
        else simp (Log uPrime)

{-
 ! diff
 * Description:
 ? Differentiating is used to find the slope of a function at a specific point. If complicated enough, the function will return another function designed for this slope at a point on the graph.
 ? Differentiation is something all calculus students have to learn. We all know the formulas - and if we forgot, they were given to us - so let's just implement them nice and easy.
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = Coef 1.0
diff (Coef n) = Coef 0.0
diff (Sum a b) = Sum (diff a) (diff b)
diff (Prod a b) =
  let a' = diff a
      b' = diff b
   in Sum (Prod a' b) (Prod a b')
diff (Quot a b) =
  let a' = diff a
      b' = diff b
   in Quot (Sum (Prod a' b) (Prod (Coef (-1.0)) (Prod a b'))) (Prod b b)
diff (Exp a) = Prod (Exp a) (diff a)
diff (Log a) = Quot (diff a) (Log a)

domain :: (Floating a, Eq a) => MathExpr a -> String
domain X = "{xER}"
domain (Coef n) = "{xER}"
domain (Exp a) = "{xER | (" ++ (read a :: MathExpr a -> String) ++ ") > 0}"
domain (Log a) = "{xER | (" ++ (read a :: MathExpr a -> String) ++ ") > 0}"
domain (Quot a b) =
  "{(" ++
  (read b :: MathExpr a -> String) ++
  ")ER | (" ++ (read b :: MathExpr a -> String) ++ ") /= 0}"
domain (Sum a b) = domain a ++ " && " ++ domain b
domain (Prod a b) = domain a ++ " && " ++ domain b
