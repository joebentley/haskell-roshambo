import Data.Char
import Data.Maybe
import System.Random

data Choice = Rock | Paper | Scissors deriving (Show, Read, Eq, Enum, Bounded)

data Result = Win | Lose | Tie deriving (Show, Eq)
     
-- Allow us to generate random choices
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
beats :: Choice -> Choice -> Result
beats a b
      | a == Rock     && b == Scissors  = Win
      | a == Paper    && b == Rock      = Win
      | a == Scissors && b == Paper     = Win
      | a == b                          = Tie
      | otherwise                       = Lose

main = do
     putStrLn "Rock, Paper, Scissors?"
     a <- getLine
     if readChoice a == Nothing
        then putStrLn "Invalid Choice" >> main
     else do
          g <- newStdGen
          let opp = fst (random g :: (Choice, StdGen)) -- Opponent's random choice

          do putStrLn ("Opponent chose " ++ show opp)
          do putStrLn (show $ beats (fromJust . readChoice $ a) opp)
     

     do putStrLn ""
        main
