fibs :: (Integral a) => [a]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
-- fibs = 1 : scanl (+) 1 fibs

zeckendorf :: (Integral a) => a -> [a]
zeckendorf 0 = []
zeckendorf n = f : zeckendorf (n - f) where f = last $ takeWhile (<= n) fibs

main :: IO ()
main = mapM_ (print . zeckendorf) [100, 3 ^ 15, 10 ^ 100]
