module Seats
  ( Seats -- ^ don't export constructor
  , getNum -- ^ accessor
  , Error(..) -- ^ our errors
  , make -- ^ smart constructor
  ) where

import Data.Validation

-- | Wrapper around 'Int' that ensures >= 0
newtype Seats = Seats
  { getNum :: Int
  } deriving (Eq, Show)

data Error =
  BadCount Int -- ^ attempted number of seats
  deriving (Eq)

instance Show Error where
  show (BadCount c) = "# seats was " ++ show c ++ ", but must be >= 0"

-- | Smart constructors for 'Seats' that ensures >= 0
make :: Int -> Validation Error Seats
make seats | seats <= 0 = Failure $ BadCount seats
           | otherwise  = Success $ Seats seats
