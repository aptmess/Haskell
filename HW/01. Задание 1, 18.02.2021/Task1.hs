bitOnes :: Integer -> Int
bitOnes 0 = 0
bitOnes x = fromIntegral x `mod` 2 + bitOnes (x `div` 2)