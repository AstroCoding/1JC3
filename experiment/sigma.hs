module Sigma where

function1 :: Int -> Int
function1 i = i

function2 :: Int -> Int
function2 i = i ^ 2

function3 :: Int -> Int
function3 i = i + 1

sigma :: Int -> Int -> (Int -> Int) -> Int
sigma n i f
  | n < i = error "n must be bigger than the Index"
  | otherwise = sum . map f $ [i .. n]

piNotation :: Int -> Int -> (Int -> Int) -> Int
piNotation n i f
  | n < i = error "n must be bigger than the Index"
  | otherwise = product . map f $ [i .. n]
