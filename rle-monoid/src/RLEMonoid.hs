module RLEMonoid (rle) where

import Data.Foldable (toList)
import Data.Sequence (Seq (..), fromList, singleton)

newtype Pairs a = Pairs {unPairs :: Seq (a, Word)}

instance (Eq a) => Semigroup (Pairs a) where
    x <> (Pairs Empty) = x
    (Pairs Empty) <> y = y
    (Pairs (xs :|> (a1, c1))) <> (Pairs ((a2, c2) :<| ys)) = Pairs (xs <> mid <> ys)
      where
        mid = if a1 == a2 then singleton (a1, c1 + c2) else fromList [(a1, c1), (a2, c2)]

instance (Eq a) => Monoid (Pairs a) where
    mempty = Pairs mempty

rle :: (Foldable f, Eq a) => f a -> [(a, Word)]
rle = toList . unPairs . foldMap (Pairs . singleton . (,1))
