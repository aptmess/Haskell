s2 :: Eq a => Int -> [a] -> [a] -> [a]
s2 max_length array_of_numbers (x:t) | null t && length array_of_numbers >= max_length = array_of_numbers
                                     | null t && length array_of_numbers < max_length = array_of_numbers
                                     | x == head array_of_numbers = s2 (max_length+1) (x:array_of_numbers) t
                                     | x /= head array_of_numbers && length array_of_numbers > max_length = s2 (length array_of_numbers) array_of_numbers t
                                     | x /= head array_of_numbers && length array_of_numbers <= max_length = s2 max_length [x] t
                                     
                                     
sumEquals :: Eq a => [a] -> [a]
sumEquals [] = []
sumEquals (x:t) = s2 1 [x] t