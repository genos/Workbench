#!/usr/bin/env stack
-- stack --resolver lts-8.17 script
{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import qualified Data.Conduit.Text as CT
import           Data.Monoid
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Formatting
import           System.IO

numberLine :: Int -> Text -> Text
numberLine i t = sformat (right 7 ' ') i <> t <> "\n"

count :: Monad m => Int -> Conduit Text m Text
count n = do
  mLine <- await
  case mLine of
    Nothing   -> return ()
    Just line -> do
      yield $! numberLine n line
      count $! n + 1

main :: IO ()
main =
  runConduitRes
    $! sourceFileBS "conduitLineNumber.hs"
    .| CT.decodeUtf8
    .| CT.lines
    .| count 1
    .| CT.encodeUtf8
    .| sinkHandle stdout
