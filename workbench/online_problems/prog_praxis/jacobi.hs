jacobi :: Integer -> Integer -> Integer
jacobi a m
    | even m        = error "m must be odd."
    | a == 0        = 0
    | a == 1        = 1
    | a == 2        = if (m `mod` 8) `elem` [3, 5] then -1 else 1
    | even a        = jacobi 2 m * jacobi (a `div` 2) m
    | a >= m        = jacobi (a `mod` m) m
    | otherwise     = let j = jacobi m a in
                        if (a `mod` 4 == 3) && (m `mod` 4 == 3) then -j else j
