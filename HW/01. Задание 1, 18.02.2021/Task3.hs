-- s2
-- Arguments
    -- x - текущий список оставшихся чисел
    -- current_num - текущий список повторяющихся чисел
    -- best_num - текущий лучший список повторяющихся чисел
    
-- sumEquals
-- Argument
    -- x - входной список

s2 :: Eq a => [a] -> [a] -> [a] -> [a]
s2 [] _ best_num  = best_num
s2 [x] current_num best_num = if length current_num > length best_num then current_num else best_num
s2 (x:t) current_num best_num | x == head t = s2 t (x:current_num) best_num
                              | length current_num > length best_num = s2 t [head t] current_num 
                              | otherwise = s2 t [head t] best_num
                                     
sumEquals :: Eq a => [a] -> [a]
sumEquals [] = []
sumEquals x = s2 x [] []