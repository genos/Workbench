#!/usr/bin/env stack
-- stack --resolver lts-21.5 script

{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U

data Pair = P {α :: {-# UNPACK #-} !Int, β :: {-# UNPACK #-} !Int}

fibs :: Int -> Vector Int
fibs n = U.unfoldrN n f P{α = 1, β = 1}
  where
    {-# INLINEABLE f #-}
    f :: Pair -> Maybe (Int, Pair)
    f !p = Just (α p, P{α = β p, β = α p + β p})

main :: IO ()
main = U.mapM_ print . fibs $ 75
