import Data.List (foldl')
import Data.Numbers.Primes (primes)

sieve :: Int -> [Int] -> [Int]
sieve p = filter (\x -> x `mod` p /= 0)

phi :: Int -> Int -> Int
phi x a = length $ foldl' (flip sieve) [1..x] (take a primes)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

primePi :: Int -> Int
primePi 1 = 0
primePi x = phi x a + a - 1 where a = primePi $ isqrt x

main :: IO ()
main = print $ primePi 1000000