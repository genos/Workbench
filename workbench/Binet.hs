import System.Environment (getArgs)

data ℚ5 = ℚ5 Rational Rational deriving (Show)

instance Num ℚ5 where
  (ℚ5 p1 q1) + (ℚ5 p2 q2) = ℚ5 (p1 + p2) (q1 + q2)
  (ℚ5 p1 q1) * (ℚ5 p2 q2) = ℚ5 (p1 * p2 + 5 * q1 * q2) (p1 * q2 + p2 * q1)
  negate (ℚ5 p q) = ℚ5 (-p) (-q)
  signum (ℚ5 p 0) = ℚ5 (signum p) 0
  signum x@(ℚ5 p q) | p > 0 && q > 0 = 1
                    | p < 0 && q < 0 = -1
                    | q > 0          = ℚ5 (signum (-p * p / q * q + 5)) 0
                    | otherwise      = negate . signum $ negate x
  abs x = x * signum x
  fromInteger n = ℚ5 (fromInteger n) 0

instance Fractional ℚ5 where
  fromRational r = ℚ5 r 0
  recip (ℚ5 p q) = ℚ5 p (-q) * (fromRational . recip $ p * p - 5 * q * q)

rationalPart :: ℚ5 -> Rational
rationalPart (ℚ5 a _) = a

score :: Int -> Integer
score = floor . rationalPart . binet
  where
    binet n = (φ ^ n - recip φ ^ n) / sqrt5
    φ = (1 + sqrt5) / 2
    sqrt5 = ℚ5 0 1

main :: IO ()
main = mapM_ (print . score . read) =<< getArgs
