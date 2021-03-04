module Quicksort2 where

quicksort :: Ord a => [a] -> [a]
quicksort list = quicksort' list []
    where
        quicksort' [] list = list
        quicksort' (x:xs) list = quicksort' (filter (< x) xs) $
                             x : quicksort' (filter (>= x) xs) list
