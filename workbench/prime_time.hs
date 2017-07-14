-- from http://mathlesstraveled.com/2011/01/06/prime-time-in-haskell/
import Control.Arrow       ((&&&), (>>>))
import Data.List           (find, intercalate, tails)
import Data.Numbers.Primes (isPrime, primes)

-- backwards function application
infixl 0 >$>

x >$> f = f x

-- Finds runs of consecutive primes that sum to n
primeSums :: Int -> [(Int, Int)]
primeSums n =
  primes
    >$> takeWhile (<n)
    >>> tails
    >>> init
    >>> map (scanl (+) 0 >>> tail >>> takeWhile (<=n) >>> length &&& last)
    >>> zip [0 ..]
    >>> filter ((==n) . snd . snd)
    >>> map (fst &&& fst . snd)

-- Expands a run into list of primes
expandRun :: (Int, Int) -> [Int]
expandRun (i, n) = primes >$> drop i >>> take n

-- Self explanatory
isSumOfConsecutivePrimes :: Int -> Bool
isSumOfConsecutivePrimes = any (isPrime . snd) . primeSums

-- is Sum of Prime Consecutive Primes in Multiple Ways
iSoPCPiMW :: Int -> Int -> Bool
iSoPCPiMW k = (>=k) . length . filter (isPrime . snd) . primeSums

-- Main event
main :: IO ()
main =
  let ps = primeSums 2011
  in  do
        print ps
        print $ map expandRun ps
        print $ isSumOfConsecutivePrimes 2011
        print $ find (iSoPCPiMW 4) [2012 ..]
        print . find isSumOfConsecutivePrimes $ dropWhile (<=2011) primes
