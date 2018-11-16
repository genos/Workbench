module Main where

import Data.Foldable      (traverse_)
import System.Environment (getArgs)

data Q5 = Q5 Rational Rational deriving (Show)

instance Num Q5 where
  (Q5 p1 q1) + (Q5 p2 q2) = Q5 (p1 + p2) (q1 + q2)
  (Q5 p1 q1) * (Q5 p2 q2) = Q5 (p1 * p2 + 5 * q1 * q2) (p1 * q2 + p2 * q1)
  negate (Q5 p q) = Q5 (-p) (-q)
  signum (Q5 p 0) = Q5 (signum p) 0
  signum x@(Q5 p q) | p > 0 && q > 0 = 1
                    | p < 0 && q < 0 = -1
                    | q > 0          = Q5 (signum (-p * p / q * q + 5)) 0
                    | otherwise      = negate . signum $ negate x
  abs x = x * signum x
  fromInteger n = Q5 (fromInteger n) 0

instance Fractional Q5 where
  fromRational r = Q5 r 0
  recip (Q5 p q) = Q5 p (-q) * (fromRational . recip $ p * p - 5 * q * q)

rationalPart :: Q5 -> Rational
rationalPart (Q5 a _) = a

score :: Int -> Integer
score = floor . rationalPart . binet
 where
  binet n = (φ ^ n - recip φ ^ n) / sqrt5
  φ     = (1 + sqrt5) / 2
  sqrt5 = Q5 0 1

main :: IO ()
main = traverse_ (print . score . read) =<< getArgs
