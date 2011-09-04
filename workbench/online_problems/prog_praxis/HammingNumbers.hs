merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | x > y     = y : merge (x:xs) ys
                    | x == y    = y : merge xs ys


hammingNumbers :: [Integer]
hammingNumbers = 1 : merge hN2 (merge hN3 hN5) where
               hN2 = map (*2) hammingNumbers
               hN3 = map (*3) hammingNumbers
               hN5 = map (*5) hammingNumbers


main :: IO ()
main = print $ take 1000 hammingNumbers