module Quadratic (
    roots
) where

roots :: (Float, Float, Float) -> (Float, Float)
roots (a, b, c) = (x1, x2) where
    e = - b / (2 * a)
    d = b * b - 4 * a * c
    x1 = e + sqrt d / (2 * a)
    x2 = e - sqrt d / (2 * a)

main :: IO()
main = do
    putStrLn "Welcome to the Quadratic Calculator."
    putStrLn "Choose an A Value:"
    va <- getLine
    putStrLn "Choose a B Value:"
    vb <- getLine
    putStrLn "Choose a C Value:"
    vc <- getLine
    let a = read va
        b = read vb
        c = read vc
    putStrLn "The roots of your polynomial equation are: "
    print(roots(a, b, c))