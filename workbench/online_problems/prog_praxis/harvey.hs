import Data.List ((\\))

combinations :: [Integer] -> Integer -> [[Integer]]
combinations _ 0 = []
combinations [] _ = []
combinations xs 1 = map (:[]) xs
combinations (x:xs) k = map (x:) (combinations xs (k - 1)) ++ combinations xs k

sums :: Integer -> [Integer]
sums n = [(n * (n + 1)) `div` 2,
     (n * (n + 1) * (2 * n + 1)) `div` 6,
     (head $ sums n) ^ 2]
     --(n * (n + 1) `div` 2) ^2]

prop :: [Integer] -> Bool
prop xs = map (`div` 2) (sums 16) == map sum [xs, map (^2) xs, map (^3) xs]

main :: IO ()
main = mapM_ printPairs filteredCombs where
     printPairs = print . (\ c -> (c, [1..16] \\ c))
     filteredCombs =  filter prop $ combinations [1..16] 8