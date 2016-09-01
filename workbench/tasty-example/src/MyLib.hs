module MyLib
  ( module Data.List
  , module Data.Ord
  , sortInts
  , flt
  , fLT
  ) where

import Data.List
import Data.Ord

sortInts :: [Int] -> [Int]
sortInts = sort

-- | requires prime @n@
flt :: Integer -> Integer -> Integer
flt x n = (x ^ n - x) `mod` n

fLT :: Integer -> Integer -> Integer -> Integer -> Integer
fLT x y z n = x ^ n + y ^ n - z ^ n
