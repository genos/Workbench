{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Foldable

oddIndices :: [a] -> [a]
oddIndices =
  histo $
  \case
    Nil -> []
    Cons h (_ :< Nil) -> [h]
    Cons h (_ :< Cons _ (t :< _)) -> h : t

evenIndices :: [a] -> [a]
evenIndices =
  histo $
  \case
    Nil -> []
    Cons _ (_ :< Nil) -> []
    Cons _ (_ :< Cons h (t :< _)) -> h : t

oddIndicesF :: [a] -> [a]
oddIndicesF = futu coalg
  where
    coalg list =
      case project list of
        Nil -> Nil
        Cons x s ->
          Cons x $
          return $!
          case project s of
            Nil -> s
            Cons _ t -> t

evenIndicesF :: [a] -> [a]
evenIndicesF = futu coalg
  where
    coalg list =
      case project list of
        Nil -> Nil
        Cons _ s ->
          case project s of
            Nil -> Nil
            Cons h t -> Cons h $ return t

nil :: Free (Prim [a]) b
nil = liftF Nil

cons :: a -> b -> Free (Prim [a]) b
cons h t = liftF $ Cons h t

twiddle :: [a] -> [a]
twiddle = futu coalg
  where
    coalg r =
      case project r of
        Nil -> Nil
        Cons x l ->
          case project l of
            Nil -> Cons x nil
            Cons h t -> Cons h $ cons x t

main :: IO ()
main = print . take 20 . twiddle $ ([1 ..] :: [Integer])
