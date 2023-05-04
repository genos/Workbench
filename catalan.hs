{- Catalan Numbers in Haskell, with some help from
 - http://rosettacode.org/wiki/Catalan_numbers#Haskell
 -}
import Test.QuickCheck (quickCheck)

-- helpers
binom :: Integer -> Integer -> Integer
binom n k = product [k + 1 .. n] `div` product [1 .. n - k]

factorial :: Integer -> Integer
factorial = product . enumFromTo 2

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | x <- xs, y <- xs]

-- Catalan definitions
cat0 :: [Integer]
cat0 = map (\n -> product [n + 2 .. 2 * n] `div` product [2 .. n]) [0 ..]

cat1 :: [Integer]
cat1 = 1 : map c [1 ..]
  where
    c n = sum $ zipWith (*) (reverse (take n cat1)) cat1

cat2 :: [Integer]
cat2 = scanl (\c n -> c * 2 * (2 * n - 1) `div` (n + 1)) 1 [1 ..]

cat3 :: [Integer]
cat3 = map c [0 ..]
  where
    c n
        | n <= 0 = 1
        | otherwise = binom (2 * n) n - binom (2 * n) (n + 1)

cat4 :: [Integer]
cat4 = map c [0 ..]
  where
    c n = sum (map (\k -> binom n k ^ 2) [0 .. n]) `div` (n + 1)

cat5 :: [Integer]
cat5 = map (\n -> binom (2 * n) n `div` (n + 1)) [0 ..]

cat6 :: [Integer]
cat6 = map c [0 ..]
  where
    c n = factorial (2 * n) `div` (factorial (n + 1) * factorial n)

-- Check that these look the same
prop_equal :: Int -> Bool
prop_equal n = all p $ pairs cats
  where
    cats = [cat0, cat1, cat2, cat3, cat4, cat5, cat6]
    p (xs, ys) = xs !! n' == ys !! n'
    n' = n `mod` 1000

main :: IO ()
main = quickCheck prop_equal
