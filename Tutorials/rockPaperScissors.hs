module Tut10 where

data Move
  = Rock
  | Paper
  | Scissors
  deriving (Show, Read, Eq, Ord, Enum)

type Strategy = [Move] -> Move

alwaysRock :: Strategy
alwaysRock _ = Rock

copyLast :: Strategy
copyLast (x:_) = x

cycleS :: Strategy
cycleS moves =
  case length moves `mod` 3 of
    0 -> Rock
    1 -> Paper
    _ -> Scissors

decideWinner :: String -> Move -> String
decideWinner "Rock" Rock         = "It's a Tie"
decideWinner "Paper" Paper       = "It's a Tie"
decideWinner "Scissors" Scissors = "It's a Tie"
decideWinner "Rock" Paper        = "AI Wins"
decideWinner "Paper" Scissors    = "AI Wins"
decideWinner "Scissors" Rock     = "AI Wins"
decideWinner "Rock" _            = "Player Wins"
decideWinner "Paper" _           = "Player Wins"
decideWinner "Scissors" _        = "Player Wins"
decideWinner _ _                 = "Ending Game"

playGame :: Strategy -> [Move] -> IO ()
playGame strategy moves = do
  putStrLn "Make a Move:"
  input <- getLine
  putStrLn $ "AI Plays:\n" ++ show (strategy moves)
  putStrLn $ decideWinner input $ strategy moves
  case input of
    "Rock"     -> playGame strategy (Rock : moves)
    "Paper"    -> playGame strategy (Paper : moves)
    "Scissors" -> playGame strategy (Scissors : moves)
    _          -> return ()

main :: IO ()
main = playGame (alternate cycleS copyLast) []

alternate :: Strategy -> Strategy -> Strategy
alternate str1 str2 moves
  | length moves `mod` 2 == 0 = str1 moves
  | otherwise = str2 moves
