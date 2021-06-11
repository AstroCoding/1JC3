timesTen :: Int -> Int
timesTen x = x * 10

charToInt :: Char -> Int
charToInt x = case x of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        _ -> error "Invalid Character"

main :: IO()
main = do
    print(map timesTen [1,2,3,4])

    print(reverse [1,2,3,4])
