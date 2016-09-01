import Data.List
import Data.Ord

mean
  :: (Floating a)
  => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

var
  :: (Floating a)
  => [a] -> a
var xs = mean (map (** 2) xs) - mean xs ** 2

median
  :: (Floating a)
  => [a] -> a
median xs
  | l == 1 = head xs
  | l == 2 = mean xs
  | otherwise = median . tail $ init xs
  where
    l = length xs

maxByLength :: [[a]] -> a
maxByLength xs = head (maximumBy (comparing length) xs)

mode
  :: (Floating a, Ord a)
  => [a] -> a
mode = maxByLength . group . sort
