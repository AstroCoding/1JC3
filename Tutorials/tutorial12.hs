module Tutorial where

helloWorld = "Hello World"

-- ! Input/Output
get2Lines = do
  putStrLn "Type Something"
  input <- getLine
  putStrLn "Type Something Else"
  input2 <- getLine
  return (input ++ " " ++ input2)

isStrEmpty :: String -> Bool
isStrEmpty x
  | x == "" = True
  | otherwise = False

printForever = do
  line <- getLine
  print line
  if (isStrEmpty line)
    then return ("You are free.")
    else printForever

-- * readFile -> lines -> unlines -> writeFile
-- Bad Code
add2Strings = do
  putStrLn "Type a number"
  input1 <- getLine
  putStrLn "Type a second number"
  input2 <- getLine
  let num1 = read input1 :: Int
      num2 = read input2 :: Int
      answer = num1 + num2
  return (answer)

-- Good Code
numbersInputOutput :: IO ()
numbersInputOutput = do
  x <- getLine
  y <- getLine
  return (addInputs x y)

addInputs :: String -> String -> Int
addInputs x y = do
  return ((read x :: Int) + (read y :: Int))

files = do
  input <- readFile "Ints1.txt"
  writeFile "results.txt" (processingInts input)

processingInts input = map show new_ints
  where
    strings = lines input
    ints = map (read) (strings)
    new_ints = map (+ 1) ints

tutorial :: IO ()
tutorial = do
  putStrLn "No need to load a module here."
{-
  print "Hello World!"
  putStrLn "Hello World!"
  putSt "Hello World!\n"
  print helloWorld
  putStrLn helloWorld
-}
