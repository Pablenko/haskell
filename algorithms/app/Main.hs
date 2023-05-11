module Main where

import Sorting

main :: IO ()
main = do
    let sorted_array = quicksort [-2, 1, 3, -5]
    putStrLn $ "Sorted array: " ++ show sorted_array
    putStrLn "Hello, Haskell!"
