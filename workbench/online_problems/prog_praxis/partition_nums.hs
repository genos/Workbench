import Data.List (foldl', nub, sort, subsequences)
import Data.Map ((!), findWithDefault, fromList, insert)
import Data.Numbers.Primes (primeFactors)

sigma1 :: (Integral a) => a -> a
sigma1 = sum . nub . map product . subsequences . primeFactors

partitions :: (Integral a) => a -> a
partitions n = foldl' p' (fromList [(0, 1)]) [1 .. n] ! n where
    p' m i = insert i (sum [sigma1 (i - j) * p'' j | j <- [0 .. i - 1],
        let p'' k = findWithDefault 1 k m] `div` i) m

main :: IO ()
main = print $ partitions 1000

{-
I was inspired by @Remco's slick Haskell solutions to try my own. After little
luck with IntMaps (perhaps due to overflow?) I came up with the following. Note:
I'm indebted to @Remco for nearly all of this, either in this exercise or
previous ones.
[sourcecode lang="css"]
import Data.List (foldl', nub, sort, subsequences)
import Data.Map ((!), findWithDefault, fromList, insert)
import Data.Numbers.Primes (primeFactors)

sigma1 :: (Integral a) => a -> a
sigma1 = sum . nub . map product . subsequences . primeFactors

partitions :: (Integral a) => a -> a
partitions n = foldl' p' (fromList [(0, 1)]) [1 .. n] ! n where
    p' m i = insert i (sum [sigma1 (i - j) * p'' j | j <- [0 .. i - 1],
        let p'' k = findWithDefault 1 k m] `div` i) m

main :: IO ()
main = print $ partitions 1000
[/sourcecode]
-}
