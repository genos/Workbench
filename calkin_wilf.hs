module Main where

import Data.Foldable   (traverse_)
import Data.Ratio
import Numeric.Natural

type ℕ = Natural
type ℚ = Ratio Natural

-- With rationals
cw :: [[ℚ]]
cw = iterate (concatMap f) [1 % 1]
 where
  f r =
    let (n, d) = (numerator r, denominator r) in [n % (n + d), (n + d) % d]

-- Original
qq :: [[(ℕ, ℕ)]]
qq = iterate (concatMap (\(a, b) -> [(a, a + b), (a + b, b)])) [(1, 1)]

main :: IO ()
main = do
  traverse_ print $ take 5 qq
  putStrLn ""
  traverse_ print $ take 5 cw
