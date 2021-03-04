merge :: Ord a => [a] -> [a] -> [a]
merge [] s = s
merge s [] = s
merge list1@(x1:s1) list2@(x2:s2) | x1 < x2 = x1 : merge s1 list2
                                  | otherwise = x2 : merge list1 s2
                                  
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort s@[_] = s
mergeSort list = merge (mergeSort list1) (mergeSort list2)
    where (list1, list2) = splitAt (length list `div` 2) list
    
data Vector a = Vector { get :: (a, a) }
getCoords :: Vector x -> (x, x)
getCoords (Vector x) = x
