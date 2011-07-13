import Data.Map

osculator :: (Integral a) => a -> a
osculator d = f e where
          f x = 1 + d * x `div` 10
          e = fromList [(1,9), (3,3), (7,7), (9,1)] ! (d `mod` 10)

isDivisible :: (Integral a) => a -> a -> Bool
isDivisible n d = aux old new where
            old = n + 1
            new = n
            osc = osculator d
            aux x y | x <= y            = y `mod` d == 0
                    | otherwise         = aux y (a + b * osc) where (a,b) = divMod y 10

main :: IO ()
main = mapM_ (\(n,d) -> print $ "Is " ++ show n ++ " divisible by " ++ show d ++
     "? " ++ show (isDivisible n d)) [(13174584, 23), (175121, 37), (134567, 29)]