factorial :: Integer -> Integer
factorial n | n == 0 = 1 -- если равно нулю, то единица
            | n > 0  = n * factorial(n-1)
            
-- Аргументы
    --- x - Double
    --- n - количество суммируемых членов, Int
ch :: Double -> Int -> Double
ch x 0 = 1
ch x n = x ** fromIntegral(2*n) / fromIntegral(factorial (2 * toInteger n)) + ch x (fromIntegral n-1) 