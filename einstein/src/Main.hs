-- https://maniagnosis.crsr.net/2009/12/einsteins-problem.html
module Main where

import Data.Foldable (traverse_)
import Data.List     (findIndex)
import Data.Maybe    (mapMaybe)

data House = Blue | Green | Red | White | Yellow deriving (Eq, Show, Enum, Bounded)
data Nationality = Brit | Dane | German | Norwegian | Swede deriving (Eq, Show, Enum, Bounded)
data Beverage = Beer | Coffee | Milk | Tea | Water deriving (Eq, Show, Enum, Bounded)
data Cigar = BlueMaster | Dunhill | PallMall | Prince | Blend deriving (Eq, Show, Enum, Bounded)
data Pet = Cat | Bird | Dog | Fish | Horse deriving (Eq, Show, Enum, Bounded)

data Owner = Owner {
    house       :: House
  , nationality :: Nationality
  , beverage    :: Beverage
  , cigar       :: Cigar
  , pet         :: Pet
  } deriving (Eq, Show)

type OwnerPred = Owner -> Bool

is, isnt :: (Eq a) => (Owner -> a) -> a -> OwnerPred
f `is` x = (==x) . f
f `isnt` x = (/=x) . f

(/\), (\/), (==>), (<==), (<=>) :: OwnerPred -> OwnerPred -> OwnerPred
l /\ r = (&&) <$> l <*> r
l \/ r = (||) <$> l <*> r
l ==> r = (not . l) \/ r
l <== r = r ==> l
l <=> r = (l ==> r) /\ (l <== r)

hint1, hint2, hint3, hint5, hint6, hint7, hintC, hintD :: OwnerPred
hint1 = (house `is` Red) <=> (nationality `is` Brit)
hint2 = (nationality `is` Swede) <=> (pet `is` Dog)
hint3 = (nationality `is` Dane) <=> (beverage `is` Tea)
hint5 = (house `is` Green) <=> (beverage `is` Coffee)
hint6 = (cigar `is` PallMall) <=> (pet `is` Bird)
hint7 = (house `is` Yellow) <=> (cigar `is` Dunhill)
hintC = (beverage `is` Beer) <=> (cigar `is` BlueMaster)
hintD = (nationality `is` German) <=> (cigar `is` Prince)

type Street = [Owner]
type StreetPred = Street -> Maybe Street

leftOf :: OwnerPred -> OwnerPred -> StreetPred
a `leftOf` b = \street -> do
  i <- findIndex a street
  h <- if i < 4 then Just (street !! (i + 1)) else Nothing
  if b h then Just street else Nothing

nextTo :: OwnerPred -> OwnerPred -> StreetPred
a `nextTo` b = \street -> do
  i <- findIndex a street
  j <- findIndex b street
  if abs (i - j) == 1 then Just street else Nothing

address :: Int -> OwnerPred -> StreetPred
address i p street | 0 <= i && i <= 5 && p (street !! i) = Just street
                   | otherwise                           = Nothing

center :: OwnerPred -> StreetPred
center = address 2

first :: OwnerPred -> StreetPred
first = address 0

hint4, hint8, hint9, hintA, hintB, hintE, hintF :: StreetPred
hint4 = (house `is` Green) `leftOf` (house `is` White)
hint8 = center (beverage `is` Milk)
hint9 = first (nationality `is` Norwegian)
hintA = (cigar `is` Blend) `nextTo` (pet `is` Cat)
hintB = (pet `is` Horse) `nextTo` (cigar `is` Dunhill)
hintE = (nationality `is` Norwegian) `nextTo` (house `is` Blue)
hintF = (cigar `is` Blend) `nextTo` (beverage `is` Water)

(/^\) :: StreetPred -> StreetPred -> StreetPred
l /^\ r = \s -> l s >> r s >> return s

outOf :: Int -> [Owner] -> [Street]
outOf 0 _  = [[]]
outOf k xs = [ x : xs' | x <- xs, xs' <- (k - 1) `outOf` delete x xs ]
 where
  delete (Owner h n b c p) = filter
    (  (house `isnt` h)
    /\ (nationality `isnt` n)
    /\ (beverage `isnt` b)
    /\ (cigar `isnt` c)
    /\ (pet `isnt` p)
    )

main :: IO ()
main = traverse_ print round2
 where
  owners :: [Owner]
  owners =
    [ Owner h n b c p
    | h <- allValues
    , n <- allValues
    , b <- allValues
    , c <- allValues
    , p <- allValues
    ]
  round1 :: [Owner]
  round1 = filter
    (hint1 /\ hint2 /\ hint3 /\ hint5 /\ hint6 /\ hint7 /\ hintC /\ hintD)
    owners
  round2 :: [Street]
  round2 =
    mapMaybe (hint4 /^\ hint8 /^\ hint9 /^\ hintA /^\ hintB /^\ hintE /^\ hintF)
      $ outOf 5 round1
  allValues :: (Enum a, Bounded a) => [a]
  allValues = enumFromTo minBound maxBound
