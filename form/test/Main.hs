{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Validation
import Form
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

validName :: Gen Text
validName = Gen.text (Range.linear 1 50) Gen.alphaNum

invalidName :: Gen Text
invalidName = Gen.choice [mempty, Gen.text (Range.linear 51 100) Gen.alphaNum]

anyName :: Gen Text
anyName = Gen.choice [validName, invalidName]

validAge :: Gen Int
validAge = Gen.integral (Range.linear 18 150)

invalidAge :: Gen Int
invalidAge =
    Gen.choice
        [Gen.integral (Range.linear minBound 0), Gen.integral (Range.linear 151 300)]

anyAge :: Gen Int
anyAge = Gen.choice [validAge, invalidAge]

prop_valid_signup_form_succeeds :: Property
prop_valid_signup_form_succeeds = property $ do
    form <- forAll $ SignupForm <$> validName <*> validAge
    case validateSignup form of
        Success{} -> pure ()
        Failure other -> annotateShow other >> failure

prop_invalid_name_fails :: Property
prop_invalid_name_fails = property $ do
    form <- forAll $ SignupForm <$> invalidName <*> validAge
    let n = T.length (formName form)
    cover 5 "too short" (n <= 0)
    cover 5 "too long" (n >= 51)
    case validateSignup form of
        Failure (NameTooShort{} :| []) -> pure ()
        Failure (NameTooLong{} :| []) -> pure ()
        other -> annotateShow other >> failure

prop_invalid_age_fails :: Property
prop_invalid_age_fails = property $ do
    form <- forAll $ SignupForm <$> validName <*> invalidAge
    let a = formAge form
    cover 5 "too young" (a <= 0)
    cover 5 "too old" (a >= 151)
    case validateSignup form of
        Failure (InvalidAge{} :| []) -> pure ()
        other -> annotateShow other >> failure

prop_two_failures_are_returned :: Property
prop_two_failures_are_returned = property $ do
    form <- forAll $ SignupForm <$> invalidName <*> invalidAge
    case validateSignup form of
        Failure failures | length failures == 2 -> pure ()
        other -> annotateShow other >> failure

prop_two_different_failures_are_returned :: Property
prop_two_different_failures_are_returned = property $ do
    form <- forAll $ SignupForm <$> invalidName <*> invalidAge
    case validateSignup form of
        Failure (failure1 :| [failure2]) -> failure1 /== failure2
        other -> annotateShow other >> failure

prop_validates_correctly :: Property
prop_validates_correctly = property $ do
    n <- forAll anyName
    a <- forAll anyAge
    let l = T.length n
    cover 20 "name too short" (l <= 0)
    cover 20 "name too long" (l >= 51)
    cover 20 "name long enough" (0 < l && l < 51)
    cover 20 "age too young" (a < 18)
    cover 20 "age too old" (a > 150)
    cover 20 "age old enough" (18 <= a && a <= 150)
    case validateSignup (SignupForm n a) of
        Success{} -> pure ()
        Failure (InvalidAge{} :| []) -> pure ()
        Failure (NameTooShort{} :| []) -> pure ()
        Failure (NameTooLong{} :| []) -> pure ()
        Failure (failure1 :| [failure2]) -> failure1 /== failure2
        other -> annotateShow other >> failure

main :: IO ()
main = T.putStrLn "" >> checkParallel $$(discover) >> pure ()
