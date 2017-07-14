c :: (Integral a) => a -> a
c n | even n    = n `div` 2
    | otherwise = 3 * n + 1

cs :: (Integral a) => a -> [a]
cs = iterate c

collatz :: (Integral a) => a -> [a]
collatz n = takeWhile (/=1) (cs n) ++ [1]

output :: (Integral a) => a -> IO ()
output n = putStrLn $ show n ++ ":\t" ++ show (collatz n)

main :: IO ()
main = mapM_ output [1 .. 25]
