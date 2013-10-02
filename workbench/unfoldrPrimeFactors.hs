import Data.List (unfoldr)
import Data.Numbers.Primes (primes)
import System.Environment (getArgs)

divides :: (Integral a) => a -> a -> Bool
divides m n = n `mod` m == 0

factors :: (Integral a) => a -> [a]
factors n = unfoldr findFactor n
    where
        findFactor 1 = Nothing
        findFactor n = Just (nextFactor, n `div` nextFactor)
            where
                nextFactor = head $ filter (`divides` n)  primes

handler :: (Integral a, Read a) => [String] -> a
handler []    = 1729
handler (n:_) =  read n

main :: IO ()
main = getArgs >>= (print . factors . handler)
