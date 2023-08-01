{-# LANGUAGE BangPatterns #-}

{-
 - rws.hs
 -
 - Yet another go at the Red & White Socks Problem, using Haskell.
 -
 - How many red socks and white socks do we need to guarantee
 - P(2r, no replacement) = 1/2?
 -
 - GRE, 7/26/10  (& 2023-08-01)
 -}
-- computes the probability given r red socks and w white socks.
actProb :: Double -> Double -> Double
actProb !r !w = (r * (r - 1)) / ((r + w) * (r + w - 1))

-- Strict pair of unpacked doubles
data Pair = P {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- constructs a list of all (r,w) pairs such that actProb r w = p until the
-- count (c) exceeds the number of iterations (1e7)
iter :: Double -> Double -> Double -> Double -> [Pair]
iter !r !w !p !c
    | c > 1e7 = []
    | a == p = P r w : iter (r + 1) w p (c + 1)
    | a < p = iter (r + 1) w p (c + 1)
    | otherwise = iter r (w + 1) p (c + 1)
  where
    a = actProb r w

-- formats a list of pairs for output
strProb :: Pair -> String
strProb (P !r !w) =
    "\n# Red Socks:\t"
        <> show (truncate r)
        <> "\n"
        <> "# White Socks:\t"
        <> show (truncate w)
        <> "\n_________________________\n"

-- self-explanatory
main :: IO ()
main = putStr . concatMap strProb $ iter 1 1 0.5 0
