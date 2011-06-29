{- shamir_threshold_scheme.hs
 -
 - My Haskell implementation of Shamir's (k, n) Threshold scheme.
 - GRE, 6/23/11
 -}

import Data.Bits (shiftR, testBit)
import Data.List (foldl', nub)
import Random (mkStdGen, randomRs)

-- Modular Exponentiation, from Remco Niemeijer's blog
-- bonsaicode.wordpress.com/2009/07/08/programming-praxis-modular-arithmetic/
expm :: Integer -> Integer -> Integer -> Integer
expm b e m = foldl' (\r (b', _) -> mod (r * b') m) 1 .
             filter (flip testBit 0 . snd) .
             zip (iterate (flip mod m . (^ 2)) b) $
             takeWhile (> 0) $ iterate (`shiftR` 1) e


-- Modular Multiplicative Inverse
-- Note: p _must_ be prime
modInv :: Integer -> Integer -> Integer
modInv x p = expm x (p - 2) p


-- Horner's Scheme
hornerMod :: [Integer] -> Integer -> Integer -> (Integer, Integer)
hornerMod cs m x = (x, foldl' (\ a b -> (a * x + b) `mod` m) 0 (reverse cs))

--
-- Shamir (k, n) Threshold scheme
shamirThreshold :: Integer -> Int -> Int -> Integer -> Int ->
                   [(Integer, Integer)]
shamirThreshold s k n p seed = map (hornerMod cs p) xs where
    rs = randomRs (1, p - 1) (mkStdGen seed) :: [Integer]
    cs = s : take (k-1) rs
    xs = take n . nub $ drop (k - 1) rs


-- Lagrange Interpolation to recover constant term
interpConst :: [(Integer, Integer)] -> Int -> Integer -> Integer
interpConst xyPairs k p = sum [y i * c i `mod` p| i <- [0..k-1]] `mod` p
    where
    x i = fst $ xyPairs !! i
    y i = snd $ xyPairs !! i
    c i = product [x j * modInv (x j - x i) p | j <- [0..k-1], j /= i] `mod` p


-- Driver Code
main :: IO ()
main = do   print s
            mapM_ print xyPairs
            print $ interpConst xyPairs k p where
                s = 1557514036
                n = 20
                k = 5
                p = 1557514061
                seed = 1729
                xyPairs = shamirThreshold s k n p seed
