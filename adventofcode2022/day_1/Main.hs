import Data.List
import Data.List.Split


subSums :: String -> [Int]
subSums =  map (sum . map read . lines) . splitOn "\n\n"


main:: IO()
main = do
    sortedSubSums <- sort . subSums <$> readFile "input.txt"
    print $ last sortedSubSums
