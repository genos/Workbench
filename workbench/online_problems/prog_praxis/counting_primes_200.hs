import Data.Numbers.Primes

primesLessThan :: (Integral a) => a -> [a]
primesLessThan n = takeWhile (< n) primes

primePi :: (Integral a) => a -> Int
primePi = length . primesLessThan

nthPrime :: (Integral a) => Int -> a
nthPrime n = primes !! n

test :: Int -> Bool
test n = (primePi (nthPrime n)) == n

main :: IO ()
main = do
    print $ primesLessThan 100
    print $ all test [1..(round 1e4)]

{-
I don't have time to do this properly today, so I'll cheat with Haskell's primes
package. It's wicked fast, using "an efficient lazy wheel sieve for prime
generation." I only tested up to 10,000, but the same code can work for 1e12
with patience and compilation.  My compiler's misbehaving today, though :-(
[sourcecode lang="css"]
import Data.Numbers.Primes

primesLessThan :: (Integral a) => a -> [a]
primesLessThan n = takeWhile (< n) primes

primePi :: (Integral a) => a -> Int
primePi = length . primesLessThan

nthPrime :: (Integral a) => Int -> a
nthPrime n = primes !! n

test :: Int -> Bool
test n = (primePi (nthPrime n)) == n

main :: IO ()
main = do
    print $ primesLessThan 100
    print $ all test [1..(round 1e4)]
[/sourcecode]
-}
