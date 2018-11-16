{-# LANGUAGE MonadComprehensions #-}

module Main where

import Data.Foldable      (traverse_)
import Data.Maybe         (fromMaybe, listToMaybe, maybe)
import System.Environment (getArgs)

fizzbuzz :: (Integral a, Show a) => a -> String
fizzbuzz i =
  fromMaybe (show i)
    $  [ "fizz" | i `rem` 3 == 0 ]
    <> [ "buzz" | i `rem` 5 == 0 ]
    <> [ "bazz" | i `rem` 7 == 0 ]

main :: IO ()
main = do
  upTo <- fmap (maybe 100 read . listToMaybe) getArgs
  traverse_ putStrLn [ fizzbuzz i | i <- [1 .. upTo] ]
