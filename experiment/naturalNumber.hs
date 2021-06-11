module MyModule where

data Nat
  = Zero
  | Suc Nat
  deriving (Show)

natPlus :: Nat -> Nat -> Nat
x `natPlus` Zero = x
x `natPlus` (Suc y) = Suc (x `natPlus` y)

nat0 = Zero

nat1 = Suc Zero

nat2 = Suc (Suc Zero)

nat3 = Suc (Suc (Suc Zero))

nat4 = Suc (Suc (Suc (Suc Zero)))
