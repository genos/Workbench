module Form where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validation
import Numeric.Natural

data SignupForm = SignupForm {formName :: !Text, formAge :: {-# UNPACK #-} !Int} deriving (Eq, Show)

data SignupError = NameTooShort Text | NameTooLong Text | InvalidAge Int deriving (Eq, Show)

data Signup = Signup {name :: !Text, age :: !Natural} deriving (Eq, Show)

validateSignup :: SignupForm -> Validation (NonEmpty SignupError) Signup
validateSignup (SignupForm n a) = Signup <$> validateName n <*> validateAge a

validateName :: Text -> Validation (NonEmpty SignupError) Text
validateName n
    | l <= 0 = Failure (NameTooShort n :| [])
    | l > 50 = Failure (NameTooLong n :| [])
    | otherwise = Success n
  where
    l = T.length n

validateAge :: Int -> Validation (NonEmpty SignupError) Natural
validateAge a
    | a < 18 || a > 150 = Failure (InvalidAge a :| [])
    | otherwise = Success (fromIntegral a)
