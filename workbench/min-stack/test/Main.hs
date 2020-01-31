{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Foldable (minimum, null, toList)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import MinStack (MinStack)
import qualified MinStack as M

genInt :: Gen Int
genInt = Gen.int $ Range.constant (-10000) 10000

genList :: Gen [Int]
genList = Gen.list (Range.linear 0 10000) genInt

genMS :: Gen (MinStack Int)
genMS = M.fromList <$> genList

prop_unit :: Property
prop_unit = withTests 1 . property $ do
  let m0 = M.empty @Int
  M.pop m0 === Nothing
  M.minVal m0 === Nothing
  let m1 = M.push m0 1
  M.pop m1 === Just (1, m0)
  M.minVal m1 === Just 1
  let m2 = M.push m1 2
  M.pop m2 === Just (2, m1)
  M.minVal m2 === Just 1

prop_pop_empty :: Property
prop_pop_empty = property $ do
  m <- forAll genMS
  let p = M.pop m
  null p === null m

prop_push_elem :: Property
prop_push_elem = property $ do
  m <- forAll genMS
  i <- forAll genInt
  assert $ elem i (M.push m i)

prop_min_is_min :: Property
prop_min_is_min = property $ do
  l <- forAll genList
  i <- forAll genInt
  let l' = i : l
      m = M.fromList l'
  M.minVal m === Just (minimum l')

prop_to_from_trip :: Property
prop_to_from_trip = property $ do
  l <- forAll genList
  tripping l M.fromList (Just . toList)

prop_push_pop_trip :: Property
prop_push_pop_trip = property $ do
  m <- forAll genMS
  i <- forAll genInt
  tripping m (`M.push` i) (fmap snd . M.pop)

main :: IO ()
main = checkParallel $$(discover) >> pure ()
