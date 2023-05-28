import Data.List
import Data.List.Split


solution :: String -> Int
solution xs =  maximum . map (sum . map read . lines) . splitOn "\n\n" $ xs


main:: IO()
main = do
    readFile "day_1/input.txt" >>= print . solution
