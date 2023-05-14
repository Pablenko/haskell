import System.IO
import System.Environment
import Text.Read


maxelem :: Ord a => [a] -> a
maxelem = foldr1 (\x y ->if x >= y then x else y)


readMaybeInt :: String -> Int
readMaybeInt line =
    case readMaybe line of
        Just x -> x
        Nothing -> -1 


biggestSubSum :: String -> Int
biggestSubSum xs =
    let parsedints =  map readMaybeInt $ lines xs
        subsums = foldl(\xs next -> if next /= -1 then head xs + next:tail xs else [0] ++ xs) [0] parsedints
    in maxelem subsums


main = do
    useargs <- getArgs
    if length useargs /= 1
        then do putStrLn "Please provide filepath as first argument!"
        else do
            handle <- openFile (head useargs) ReadMode
            contents <- hGetContents handle
            let biggestsubsum = biggestSubSum contents
            print biggestsubsum
            hClose handle
