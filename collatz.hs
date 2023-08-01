module Main where

import Data.Foldable (traverse_)

c :: (Integral a) => a -> a
c n
    | even n = n `div` 2
    | otherwise = 3 * n + 1

collatz :: (Integral a) => a -> [a]
collatz n = takeWhile (/= 1) (iterate c n) ++ [1]

output :: (Integral a, Show a) => a -> IO ()
output n = putStrLn $ show n ++ ":\t" ++ show (collatz n)

main :: IO ()
main = traverse_ output [1 .. 25]
