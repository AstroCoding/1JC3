module Lib (
    double
    ) where
double x = x * x

main :: IO()
main = do
    {- let
        listHead = head listExample
        listTail = tail listExample
        list3rd = listExample !! 2
    print ([listHead] ++ listTail)
    putStrLn " and the third digit is "
    print [list3rd] -}
    inputNumber <- getLine
    print (double inputNumber)