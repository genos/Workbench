import Data.Ratio
import Numeric.Natural

type ℕ = Natural
type ℚ = Ratio Natural

-- With rationals
cw :: [[ℚ]]
cw = iterate (concatMap f) [1 % 1]
 where
  f r = let (n, d) = (numerator r, denominator r) in [n % (n + d), (n + d) % d]

-- Original
qq :: [[(ℕ, ℕ)]]
qq = iterate (concatMap (\(a, b) -> [(a, a + b), (a + b, b)])) [(1, 1)]

main :: IO ()
main = do
  mapM_ print $ take 5 qq
  putStrLn ""
  mapM_ print $ take 5 cw
