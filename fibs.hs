{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U

fibs :: Int -> Vector Int
fibs n = U.unfoldrExactN n (\(!a, !b) -> (a, (b, a + b))) (1, 1)

main :: IO ()
main = U.mapM_ print . fibs $ 75
