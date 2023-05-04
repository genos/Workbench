{-# LANGUAGE OverloadedStrings #-}

-- Largely just http://www.parsonsmatt.org/2016/02/27/an_elegant_fizzbuzz.html
import Data.Foldable (fold, traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)

rule :: (Integral a) => a -> Text -> a -> Maybe Text
rule n t i
    | i `mod` n == 0 = Just t
    | otherwise = Nothing

showIntegral :: (Integral a) => a -> Text
showIntegral = toStrict . toLazyText . decimal

fizzBuzz :: (Integral a) => a -> Text
fizzBuzz = fromMaybe <$> showIntegral <*> fold [fizz, buzz]
  where
    fizz = rule 3 "Fizz"
    buzz = rule 5 "Buzz"

main :: IO ()
main = traverse_ (T.putStrLn . fizzBuzz) [1 .. 100]
