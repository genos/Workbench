{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (liftM2, (<=<))
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Functor.Foldable      hiding (Foldable, Unfoldable)
import qualified Data.Functor.Foldable      as RS (Foldable, Unfoldable)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text

data ExprF r
  = VarF Text
  | LitF Int
  | AddF r
         r
  deriving (Show, Functor, Foldable, Traversable)

type Expr = Fix ExprF

data Error =
  FreeVar Text
  deriving (Show)

var :: Text -> Expr
var = Fix . VarF

lit :: Int -> Expr
lit = Fix . LitF

add :: Expr -> Expr -> Expr
add a b = Fix $ AddF a b

cataM
  :: (Monad m, Traversable (Base t), RS.Foldable t)
  => (Base t a -> m a)
  -> t
  -> m a
cataM alg = c where c = alg <=< traverse c . project

anaM
  :: (Monad m, Traversable (Base t), RS.Unfoldable t)
  => (a -> m (Base t a))
  -> a
  -> m t
anaM coalg = a where a = (return . embed) <=< traverse a <=< coalg

paraM
  :: (Monad m, Traversable (Base t), RS.Foldable t)
  => (Base t (t, a) -> m a)
  -> t
  -> m a
paraM alg = p
 where
  p = alg <=< traverse f . project
  f t = liftM2 (,) (return t) (p t)

apoM
  :: (Monad m, Traversable (Base t), RS.Unfoldable t)
  => (a -> m (Base t (Either t a)))
  -> a
  -> m t
apoM coalg = a
 where
  a = (return . embed) <=< traverse f <=< coalg
  f = either return a

hyloM :: (Monad m, Traversable t) => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM alg coalg = h where h = alg <=< traverse h <=< coalg

eval :: Expr -> ReaderT (Map Text Int) (Either Error) Int
eval = cataM $ \case
  LitF j   -> return j
  AddF i j -> return $! i + j
  VarF v   -> do
    env <- ask
    case Map.lookup v env of
      Nothing -> lift . Left $ FreeVar v
      Just j  -> return j

main :: IO ()
main = do
  let open = add (var "x") (var "y")
  let p    = Text.putStrLn . Text.pack . show
  p $ runReaderT (eval open) (Map.singleton "x" 1)
  p $ runReaderT (eval open) (Map.fromList [("x", 1), ("y", 5)])
