module Main
  ( main
  , spec
  ) where

import qualified BookingRequest
import           Data.Validation
import qualified Date
import qualified Seats
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Failing" $
    it "junk" $
         do let now = Date.Date 2
            BookingRequest.make now (Just "1") (Just (-3)) `shouldSatisfy`
              (== AccFailure
                    [ BookingRequest.DateBefore (Date.Date 1) (Date.Date 2)
                    , BookingRequest.SeatsError (Seats.BadCount (-3))
                    ])
  describe "Succeeding" $
    it "All good" $
         do let now = Date.Date 2
            BookingRequest.make now (Just "3") (Just 5) `shouldSatisfy`
              isAccSuccess

isAccSuccess :: AccValidation e a -> Bool
isAccSuccess (AccSuccess _) = True
isAccSuccess _              = False
