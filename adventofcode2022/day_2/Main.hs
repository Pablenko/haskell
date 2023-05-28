data Choice = Rock | Paper | Scissors deriving(Show, Eq, Ord)

type Round = (Choice, Choice)

charToChoice :: Char -> Choice
charToChoice 'A' = Rock
charToChoice 'B' = Paper
charToChoice 'C' = Scissors
charToChoice 'X' = Rock
charToChoice 'Y' = Paper
charToChoice 'Z' = Scissors


choiceToScore :: Choice -> Int
choiceToScore Rock = 1
choiceToScore Paper = 2
choiceToScore Scissors = 3


parseLine :: String -> Round
parseLine (c1:' ':c2:_) = (charToChoice c1, charToChoice c2)


parseInput :: String -> [Round]
parseInput xs = map parseLine $ lines xs


fight :: Round -> Int
fight (Rock, Paper) = 6
fight (Rock, Scissors) = 0
fight (Paper, Rock) = 0
fight (Paper, Scissors) = 6
fight (Scissors, Paper) = 0
fight (Scissors, Rock) = 6
fight (_, _) = 3


roundScore :: Round -> Int
roundScore (opp, me) = fight (opp, me) + choiceToScore me


main :: IO ()
main = do parseInput <$> readFile "input.txt" >>= print . sum . map roundScore
