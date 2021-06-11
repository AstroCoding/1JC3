module Caesar where

import           Data.Char

encLower :: String -> Int -> String
encLower xs key = map (\x -> chr $ (ord x + key) `mod` 123) $ map toLower xs

decLower :: String -> Int -> String
decLower xs key = map (\x -> chr $ (ord x - key) `mod` 123) $ map toLower xs

encUpper :: String -> Int -> String
encUpper xs key = map (\x -> chr $ (ord x + key) `mod` 91) $ map toUpper xs

decUpper :: String -> Int -> String
decUpper xs key = map (\x -> chr $ (ord x - key) `mod` 91) $ map toUpper xs

enc :: IO ()
enc = do
  putStrLn "Message to Encrypt (No Symbols):"
  inputA <- getLine
  putStrLn "Caesar Key :: Int:"
  inputB <- getLine
  print $ encUpper inputA (read inputB :: Int)

dec :: IO ()
dec = do
  putStrLn "Message to Decrypt (No Symbols):"
  inputA <- getLine
  putStrLn "Caesar Key :: Int:"
  inputB <- getLine
  print $ decUpper inputA (read inputB :: Int)
