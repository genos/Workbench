module Main where


import Data.Char          (ord)
import Data.Foldable      (traverse_)
import Data.Function      (on)
import System.Environment (getArgs)
import System.IO          (readFile)

zwmoo :: String -> String -> [Int]
zwmoo = zipWith ((-) `on` ord)

taxicab :: String -> String -> Int
taxicab p = sum . map abs . zwmoo p

isCandidate :: String -> String -> Bool
isCandidate p w = (length p == length w) && (taxicab p w <= 4)

showHow :: String -> String -> String
showHow p w = w ++ ":\t" ++ show (zwmoo p w)

main :: IO ()
main = do
  (puzz : fileName : _) <- getArgs
  traverse_ (putStrLn . showHow puzz)
    .   filter (isCandidate puzz)
    .   lines
    =<< readFile fileName
