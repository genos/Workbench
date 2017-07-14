import Data.Ratio

-- With rationals
cw :: [[Rational]]
cw = iterate (concatMap f) [1 % 1]
 where
  f r = [x % (x + y), (x + y) % y]
   where
    x = numerator r
    y = denominator r

-- Original
qq :: [[(Integer, Integer)]]
qq = iterate (concatMap (\(a, b) -> [(a, a + b), (a + b, b)])) [(1, 1)]

main :: IO ()
main = do
  mapM_ print $ take 5 qq
  putStrLn ""
  mapM_ print $ take 5 cw
