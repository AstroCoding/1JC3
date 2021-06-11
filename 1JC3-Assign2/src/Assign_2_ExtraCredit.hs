{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}

{-
 ! Assignment 1 Extra Credit
 * Name: Mark Hutchison
 * Date:
 -}
module Assign_2_ExtraCredit where

import           Data.Complex

macid = "hutchm6"

data GaussianInt a =
  a :@ a
  deriving (Show)

class GaussianIntegral g where
  gaussZero :: Integral a => g a
  gaussReal :: Integral a => g a -> a
  gaussImag :: Integral a => g a -> a
  gaussConj :: Integral a => g a -> g a
  gaussAdd :: Integral a => g a -> g a -> g a
  gaussMult :: Integral a => g a -> g a -> g a

instance GaussianIntegral GaussianInt where
  gaussZero = 0 :@ 0
  gaussReal (a :@ b) = a
  gaussImag (a :@ b) = b
  gaussConj (a :@ b) = a :@ (-b)
  gaussAdd (a :@ b) (x :@ y) = (a + x) :@ (b + y)
  gaussMult (a :@ b) (x :@ y) = (a * x - b * y) :@ (b * x + a * y)

gaussNorm :: (Integral a, GaussianIntegral g) => g a -> a
gaussNorm g =
  let finalGaussNum = gaussMult g (gaussConj g)
   in (gaussReal finalGaussNum) + (gaussImag finalGaussNum)

maxGaussNorm :: (Integral a, GaussianIntegral g) => [g a] -> g a
maxGaussNorm gs = error "I'm notable to define instance of Eq"
