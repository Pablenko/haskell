module Sorting (quicksort) where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let left_sorted = quicksort [elem | elem <- xs, elem <= x]
        right_sorted = quicksort [elem | elem <- xs, elem > x]
    in left_sorted ++ [x] ++ right_sorted
