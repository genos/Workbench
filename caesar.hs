{-
 - caesar.hs
 -
 - Implements a caesar cipher encoder (and decoder), as well as a cracker built
 - using a frequency table.
 - From Graham Hutton's "Programming in Haskell."
 - Modified (per last exercise in chapter 5) to "handle upper-case letters" by
 - switching letters to lowercase in let2int.
 -
 - GRE, 7/24/10
 -}
-- for ord and chr
import Data.Char

-- en/decoding groundwork
let2int :: Char -> Int
let2int c = ord (toLower c) - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLetter c = int2let ((let2int c + n) `mod` 26)
          | otherwise  = c

-- encoding
encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs ]

-- decoding
decode :: Int -> String -> String
decode n xs = [ shift (-n) x | x <- xs ]

-- cracking groundwork
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [ i | (x', i) <- zip xs [0 .. n], x == x' ]
  where n = length xs - 1

lowers :: String -> Int
lowers xs = length [ x | x <- xs, isLower x ]

count :: Char -> String -> Int
count x xs = length [ x' | x' <- xs, x == x' ]

table :: [Float]
table =
  [ 8.2
  , 1.5
  , 2.8
  , 4.3
  , 12.7
  , 2.2
  , 2.0
  , 6.1
  , 7.0
  , 0.2
  , 0.8
  , 4.0
  , 2.4
  , 6.7
  , 7.5
  , 1.9
  , 0.1
  , 6.0
  , 6.3
  , 9.1
  , 2.8
  , 1.0
  , 2.4
  , 0.2
  , 2.0
  , 0.1
  ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [ percent (count x xs) n | x <- ['a' .. 'z'] ] where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((o - e) ^ 2) / e | (o, e) <- zip os es ]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- cracking
crack :: String -> String
crack xs = decode factor xs
 where
  factor = head (positions (minimum chitab) chitab)
  chitab = [ chisqr (rotate n table') table | n <- [0 .. 25] ]
  table' = freqs xs
