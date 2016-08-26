module Main where

import qualified Data.Foldable   as F
import qualified Data.List       as L
import           QCPlay
import           Test.QuickCheck

balanced :: Tree a -> Bool
balanced Null = True
balanced (Fork _ l r) = (d == 0 || d == 1) && balanced l && balanced r
  where
    d = weight r - weight l

weight :: Tree a -> Int
weight = sum . fmap (const 1)

good :: Tree a -> Bool
good Null = True
good (Fork _ l r) = weight l <= weight r

prop_invariant_0 :: [Int] -> Bool
prop_invariant_0 = invariant . makeTree

prop_balanced_0 :: [Int] -> Bool
prop_balanced_0 = balanced . makeTree

data Op = Insert Int
        | DeleteMin
  deriving Show

make :: (Foldable f) => f Op -> Tree Int
make = F.foldl' op Null
  where
    op h (Insert n) = insert n h
    op Null DeleteMin = Null
    op h DeleteMin = deleteMin h

instance Arbitrary Op where
  arbitrary = frequency [(2, arbitrary >>= \n -> return $! Insert n), (1, return DeleteMin)]

prop_invariant_1 :: [Op] -> Bool
prop_invariant_1 = invariant . make

prop_balanced_1 :: [Op] -> Bool
prop_balanced_1 = balanced . make

prop_all_good :: [Op] -> Bool
prop_all_good = good . make

model :: Tree Int -> [Int]
model = L.sort . F.toList

models :: ([Int] -> [Int]) -> (Tree Int -> Tree Int) -> Tree Int -> Bool
models f g h = f (model h) == model (g h)

prop_insert :: Int -> [Op] -> Bool
prop_insert n ops = (L.insert n `models` insert n) h
  where
    h = make ops

prop_delete_min :: [Op] -> Property
prop_delete_min ops = weight h > 0 ==> (tail `models` deleteMin) h
  where
    h = make ops

main :: IO ()
main = do
  putStrLn "invariant 0"
  quickCheck prop_invariant_0
  putStrLn "balanced 0"
  quickCheck prop_balanced_0
  putStrLn "invariant 1"
  quickCheck prop_invariant_1
  putStrLn "balanced 1"
  quickCheck prop_balanced_1
  putStrLn "all good"
  quickCheck prop_all_good
  putStrLn "insert"
  quickCheck prop_insert
  putStrLn "delete min"
  quickCheck prop_delete_min
