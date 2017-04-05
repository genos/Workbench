module Main where

import MyLib
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps =
  testGroup
    "(checked by SmallCheck)"
    [ SC.testProperty "sort == sort . reverse" $
      \xs -> sortInts xs == sort (reverse xs)
    , SC.testProperty "Fermat's little theorem" $ \x -> flt x 7 == 0
    , SC.testProperty "Fermat's last theorem" $
      \x y z n -> n >= 3 SC.==> fLT x y z n /= 0
    ]

qcProps :: TestTree
qcProps =
  testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "sort == sort . reverse" $
      \xs -> sortInts xs == sortInts (reverse xs)
    , QC.testProperty "Fermat's little theorem" $ \x -> flt x 7 == 0
    , QC.testProperty "Fermat's last theorem" $
      \x y z n -> n >= 3 QC.==> fLT x y z n /= 0
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $ ([1, 2, 3] :: [Int]) `compare`
      ([1, 2] :: [Int]) @?=
      GT
    , testCase "List comparison (same length)" $ ([1, 2, 3] :: [Int]) `compare`
      ([1, 2, 2] :: [Int]) @?=
      LT
    ]
