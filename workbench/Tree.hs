module Tree where

import           Data.Foldable    (Foldable)
import qualified Data.Foldable    as F
import           Data.Monoid      ((<>))
import           Data.Traversable (Traversable)
import qualified Data.Traversable as T

-- http://stackoverflow.com/a/6799885
-- Edward Kmett shows that this has a good monad instance, while the typical
-- binary tree setup doesn't
data Tree a
  = Tip a
  | Bin (Tree a)
        (Tree a)

instance (Eq a) =>
         Eq (Tree a) where
  Tip x == Tip y = x == y
  Bin l1 r1 == Bin l2 r2 = l1 == l2 && r1 == r2
  _ == _ = False

instance (Show a) =>
         Show (Tree a) where
  show (Tip x)   = "<" ++ show x ++ ">"
  show (Bin l r) = "[" ++ show l ++ "]-[" ++ show r ++ "]"

instance Functor Tree where
  fmap f (Tip x)   = Tip (f x)
  fmap f (Bin l r) = Bin (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Tip
  (Tip f) <*> (Tip x) = Tip (f x)
  tf@(Tip f) <*> (Bin l r) = Bin (tf <*> l) (tf <*> r)
  (Bin lf rf) <*> tx@(Tip x) = Bin (lf <*> tx) (rf <*> tx)
  (Bin lf rf) <*> (Bin l r) =
    Bin (Bin (lf <*> l) (lf <*> r)) (Bin (rf <*> l) (rf <*> r))

instance Monad Tree where
  return = Tip
  Tip x >>= f = f x
  Bin l r >>= f = Bin (l >>= f) (r >>= f)

instance Foldable Tree where
  foldMap f (Tip x)   = f x
  foldMap f (Bin l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  traverse f (Tip x)   = Tip <$> f x
  traverse f (Bin l r) = Bin <$> traverse f l <*> traverse f r

t0 :: Tree Int
t0 = Tip 0

t1 :: Tree Int
t1 = Bin (Tip 0) (Tip 1)

t2 :: Tree Int
t2 = Bin (Bin (Tip 0) (Tip 1)) (Bin (Tip 2) (Tip 3))

f0 :: Tree (Int -> Int)
f0 = Tip (+1)

f1 :: Tree (Int -> Int)
f1 = Bin (Tip (\n -> 3 * n + 1)) (Tip (`div`2))

main :: IO ()
main = mapM_
  print
  [t0, t1, t2, f0 <*> t0, f0 <*> t1, f0 <*> t2, f1 <*> t0, f1 <*> t1, f1 <*> t2]
