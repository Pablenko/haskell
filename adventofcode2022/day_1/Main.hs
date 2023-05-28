import Data.List
import Data.List.Split


solution :: String -> Int
solution xs =  maximum . map (sum . map read . lines) . splitOn "\n\n" $ xs


main:: IO()
main = do
    readFile "input.txt" >>= print . solution
