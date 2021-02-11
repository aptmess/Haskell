-- prodIndices
    -- 1. [Integer] - список целых чисел
    -- 2. [Int] - список индексов
    
differences :: [Integer] -> [Integer]
differences (x:fs) = zipWith (*) (x:fs) (tail fs)

prodIndices :: [Integer] -> [Int]
prodIndices list = map fst $ 
                   filter (\(i, x) -> x == 0) $ 
                   zip [0..length list - 1] $ 
                   zipWith (-) (differences (1:list++[1])) list