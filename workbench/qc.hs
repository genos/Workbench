import Data.List
import Test.QuickCheck

qsort
  :: Ord a
  => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (< x) xs
    rhs = filter (>= x) xs

prop_idempotent
  :: Ord a
  => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_ordered
  :: Ord a
  => [a] -> Bool
prop_ordered xs = permutation xs (qsort xs)
  where
    permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_minimum
  :: Ord a
  => [a] -> Property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_maximum
  :: Ord a
  => [a] -> Property
prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

main :: IO ()
main = do
  mapM_
    quickCheck
    ([prop_idempotent, prop_ordered, prop_ordered] :: [[Integer] -> Bool])
  mapM_ quickCheck ([prop_minimum, prop_maximum] :: [[Integer] -> Property])
