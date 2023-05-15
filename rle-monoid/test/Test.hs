module Main (main) where

import RLEMonoid (rle)
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $
    testCase "Example from website" $ do
        rle "aaaabbbcca" @?= [('a', 4), ('b', 3), ('c', 2), ('a', 1)]
