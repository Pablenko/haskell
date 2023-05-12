module Sorting (quicksort, mergesort, bubblesort, selectionsort) where

import Misc

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let left_sorted = quicksort [elem | elem <- xs, elem <= x]
        right_sorted = quicksort [elem | elem <- xs, elem > x]
    in left_sorted ++ [x] ++ right_sorted


merge :: (Ord a) => [a] -> [a] -> [a]
merge [] right           = right
merge left []            = left
merge (x:left) (y:right) | x < y     = x:merge left (y:right)
                         | otherwise = y:merge (x:left) right


mergesort :: (Ord a) => [a] -> [a]
mergesort xs
    | length xs < 2 = xs
    | otherwise = merge (mergesort left) (mergesort right)
    where left = take half xs
          right = drop half xs
          half = length xs `div` 2


swap :: (Ord a) => [a] -> [a]
swap []  = []
swap [x] = [x]
swap (x:xs) | x > head xs = head xs:swap (x:tail xs)
            | otherwise = x:swap xs 

bubblesort :: (Ord a) => [a] -> [a] 
bubblesort xs = foldl (\acc _ -> swap acc) xs xs


selectionsort :: Ord a => [a] -> [a]  
selectionsort [] = []  
selectionsort xs = min_elem:selectionsort (delete min_elem xs)  
    where min_elem = minimum xs
