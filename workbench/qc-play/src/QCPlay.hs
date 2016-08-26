module QCPlay where

import           Data.Monoid      ((<>))

data Tree a = Null
            | Fork a (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Null = Null
  fmap f (Fork x l r) = Fork (f x) (fmap f l) (fmap f r)

instance Applicative Tree where
  pure x = Fork x Null Null
  Null <*> _ = Null
  _ <*> Null = Null
  (Fork fx fl fr) <*> (Fork x l r) = Fork (fx x) (fl <*> l) (fr <*> r)

instance Foldable Tree where
  foldMap _ Null = mempty
  foldMap f (Fork x l r) = f x <> foldMap f l <> foldMap f r

instance Traversable Tree where
  traverse _ Null = pure Null
  traverse f (Fork x l r) = Fork <$> f x <*> traverse f l <*> traverse f r

invariant :: (Ord a) => Tree a -> Bool
invariant Null = True
invariant (Fork x l r) = smaller x l && smaller x r

smaller :: (Ord a) => a -> Tree a -> Bool
smaller _ Null = True
smaller x (Fork y l r) = x <= y && invariant (Fork y l r)

minElem :: Tree a -> a
minElem (Fork x _ _) = x
minElem Null = undefined

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Null = Fork x Null Null
insert x (Fork y l r) = Fork (min x y) r (insert (max x y) l)

makeTree :: (Integral a) => [a] -> Tree a
makeTree = foldr insert Null

deleteMin :: (Ord a) => Tree a -> Tree a
deleteMin Null = undefined
deleteMin (Fork _ l r) = merge l r

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Null Null = Null
merge Null t = t
merge t Null = t
merge l r
  | minElem l <= minElem r = join l r
  | otherwise = join r l
  where
    join (Fork x a b) t = Fork x b (merge a t)
    join Null _ = undefined
