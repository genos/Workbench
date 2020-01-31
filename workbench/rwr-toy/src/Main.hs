{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad.Free.Reflectable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Toy a next = Output a next | Bell next | Done

instance Functor (Toy a) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell next) = Bell (f next)
  fmap _ Done = Done

liftF :: Functor f => f r -> FreeMonad f r
liftF = fromView . Impure . fmap (fromView . Pure)

output :: a -> FreeMonad (Toy a) ()
output x = liftF $ Output x ()

bell :: FreeMonad (Toy a) ()
bell = liftF $ Bell ()

done :: FreeMonad (Toy a) ()
done = liftF Done

subroutine :: FreeMonad (Toy Char) ()
subroutine = output 'A'

program :: FreeMonad (Toy Char) ()
program = do
  subroutine
  _ <- bell
  done

tshow :: Show a => a -> Text
tshow = T.pack . show

showProgram :: (Show a, Show r) => FreeMonad (Toy a) r -> Text
showProgram (toView -> Impure (Output a x)) = "output " <> tshow a <> "\n" <> showProgram x
showProgram (toView -> Impure (Bell x)) = "bell\n" <> showProgram x
showProgram (toView -> Impure Done) = "done\n"
showProgram (toView -> Pure x) = "return " <> tshow x <> "\n"
showProgram _ = error "Exhaustive pattern match…"

showProgram' :: (Show a, Show r) => FreeMonad (Toy a) r -> Text
showProgram' f = case toView f of
  Impure (Output a x) -> "output " <> tshow a <> "\n" <> showProgram x
  Impure (Bell x) -> "bell\n" <> showProgram x
  Impure Done -> "done\n"
  Pure x -> "return " <> tshow x <> "\n"

main :: IO ()
main = do
  T.putStrLn . showProgram $ program
  T.putStr . showProgram' $ program
