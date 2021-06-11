-- ! Overloaded Functions
-- * A function with the same name, but multiple definitions
-- ! Class
-- * A class is a collection of types that support certain overloaded functions on them
-- ? Ask for type of class? :i {class_name}
-- | Returns all possible types the class can accept and what each will respond.
-- ! Instance
-- * In a class, each instance is what the class does with every type.
-- * Eq is the class, while Bool, Int, Char, String, etc... are the Instances it supports
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- * Must check is the type class of a is comparable, so we check a again Eq (The equality of all types)
palindromeCheck :: Eq a => [a] -> Bool
palindromeCheck xs = (reverse xs == xs)

helloResponse :: String -> String
helloResponse input
  | input == "Hello" = "Hello There!"
  | otherwise = "Say Hello"

and' :: Bool -> Bool -> Bool
and' False _ = False
and' True x  = x

and'' :: Bool -> Bool -> Bool
and'' True True = True
and'' _ _       = False

or' :: Bool -> Bool -> Bool
or' True _      = True
or' _ True      = True
or' False False = False

or'' :: Bool -> Bool -> Bool
or'' False False = False
or'' _ _         = True

main :: IO ()
main
    -- * function
 = do
  print (sum [1, 2, 3])
    -- ? What type class? The Num type class.
    -- * Each different Num type is an Instance, e.g. Int, Float, Double, etc...
  input <- getLine
    -- * All three below do the exact same thing.
  if input == "Hello"
    then print ("Hello There!")
    else print ("Say Hello")
  case input of
    "Hello" -> print ("Hello There!")
    _       -> print ("Say Hello")
  print (helloResponse input)
