module Main where

import Lib

main :: IO ()
main = do
    let
        x  = (-2)

    print (double (double x))
    print (quadruple x)
