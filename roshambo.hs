import Data.Char
import Data.Maybe
import System.Random

data Choice = Rock | Paper | Scissors deriving (Show, Read, Eq, Enum, Bounded)

-- Allow us to generate a random choices
instance Random Choice where
         randomR (a, b) g =
                 case randomR (fromEnum a, fromEnum b) g of
                      (x, g') -> (toEnum x, g')
         random g = randomR (minBound, maxBound) g

readChoice :: String -> Maybe Choice
readChoice str
           | lower == "rock"     = Just Rock
           | lower == "paper"    = Just Paper
           | lower == "scissors" = Just Scissors
           | otherwise           = Nothing
           where lower = map toLower str

-- Just true if first choice beats second, Nothing on tie
beats :: Choice -> Choice -> Maybe Bool
beats a b
      | a == Rock     && b == Scissors  = Just True
      | a == Paper    && b == Rock      = Just True
      | a == Scissors && b == Paper     = Just True
      | a == b                          = Nothing
      | otherwise                       = Just False

main = do
     putStrLn "Rock, Paper, Scissors?"
     a <- getLine
     if readChoice a == Nothing
        then putStrLn "Invalid Choice" >> main
     else do
          g <- newStdGen
          let opp = fst (random g :: (Choice, StdGen)) -- Opponent's random choice

          do putStrLn ("Opponent chose " ++ show opp)

          -- Holds Maybe value, Nothing if tie, Just True if we win, Just False otherwise
          let b = (fromJust . readChoice $ a) `beats` opp

          if (isNothing b)
             then do putStrLn "Tie..."
             else if (fromJust b)
                  then do putStrLn "You win!!"
                  else do putStrLn "You lose..."

     do putStrLn ""
     main
