module Main where

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ratio (denominator, numerator, (%))

data FRACTRAN a = FT
    { _fractions :: [Rational]
    , _setup :: Integer -> Integer
    , _finish :: [Integer] -> a
    }

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust = (listToMaybe .) . mapMaybe

eval :: FRACTRAN a -> Integer -> a
eval (FT fs s f) = f . run [] . s
  where
    run ks k = maybe ks (run =<< (: ks)) $ firstJust (step k) fs
    step a b =
        let ab = (a % 1) * b
            n = numerator ab
            d = denominator ab
         in if d == 1 then Just n else Nothing

iLog2 :: (Integral a) => a -> Maybe Word
iLog2 = go 0
  where
    go l n
        | n == 1 = Just l
        | odd n || n < 1 = Nothing
        | otherwise = go (succ l) (n `div` 2)

fib :: FRACTRAN (Maybe Word)
fib =
    FT
        { _fractions = [17 % 65, 133 % 34, 17 % 19, 23 % 17, 2233 % 69, 23 % 29, 31 % 23, 74 % 341, 31 % 37, 41 % 31, 129 % 287, 41 % 43, 13 % 41, 1 % 13, 1 % 3]
        , _setup = (78 *) . (5 ^) . pred
        , _finish = firstJust iLog2
        }

collatz :: FRACTRAN [Word]
collatz =
    FT
        { _fractions = [165 % 14, 11 % 63, 38 % 21, 13 % 7, 34 % 325, 1 % 13, 184 % 95, 1 % 19, 7 % 11, 13 % 17, 19 % 23, 1575 % 4]
        , _setup = (2 ^)
        , _finish = reverse . mapMaybe iLog2
        }

main :: IO ()
main = do
    traverse_ (print . fromMaybe 0 . eval fib) [1 .. 20]
    print $ eval collatz 7
