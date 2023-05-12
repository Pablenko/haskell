module Misc (max_elem, min_elem, delete) where

max_elem :: (Ord a) => [a] -> a
max_elem xs = foldl1 (\max_elem candidate -> if max_elem > candidate then max_elem else candidate) xs

min_elem :: (Ord a) => [a] -> a
min_elem xs = foldl1 (\min_elem candidate -> if min_elem > candidate then candidate else min_elem) xs

delete :: (Eq a) => a -> [a] -> [a]
delete elem xs = foldl(\xs candidate -> if candidate == elem then xs else xs++[candidate]) [] xs
