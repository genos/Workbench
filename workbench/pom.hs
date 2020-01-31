#!/usr/bin/env stack
-- stack --resolver lts-14.22 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
module Main where

import Data.Default
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Options.Applicative.Text
import System.Exit (exitFailure)

tshow :: Show a => a -> Text
tshow = T.pack . show

data Options = Options {_retryCount :: {-# UNPACK #-} !Int, _host :: !Text, _characterCode :: Maybe Char}
  deriving (Show, Eq)

data PartialOptions = PartialOptions {_pRetryCount :: Last Int, _pHost :: Last Text, _pCharacterCode :: Last (Maybe Char)}
  deriving (Show, Eq)

instance Semigroup PartialOptions where
  (PartialOptions rx hx cx) <> (PartialOptions ry hy cy) = PartialOptions (rx <> ry) (hx <> hy) (cx <> cy)

instance Monoid PartialOptions where
  mempty = PartialOptions mempty mempty mempty

lastToEither :: Text -> Last a -> Either Text a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

makeOptions :: PartialOptions -> Either Text Options
makeOptions PartialOptions {..} = do
  _retryCount <- lastToEither "Missing retry count" _pRetryCount
  _host <- lastToEither "Missing host" _pHost
  _characterCode <- lastToEither "Missing character code" _pCharacterCode
  return Options {..}

instance Default PartialOptions where
  def = mempty {_pRetryCount = pure 5, _pCharacterCode = pure $ Just 'c'}

lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

partialOptionsParser :: Parser PartialOptions
partialOptionsParser =
  PartialOptions
    <$> lastOption (option auto $ long "retry-count")
    <*> lastOption (option text $ long "host")
    <*> lastOption (fmap Just (option auto $ long "character-code") <|> flag' Nothing (long "no-character-code"))

die :: Text -> IO a
die err = T.putStrLn err >> exitFailure

parseOptions :: IO Options
parseOptions = do
  cmdLineOptions <- execParser $ info partialOptionsParser mempty
  let combinedOptions = def <> cmdLineOptions
  either die return $! makeOptions combinedOptions

run :: Options -> IO ()
run opts = T.putStrLn $ "Running with " <> tshow opts

main :: IO ()
main = parseOptions >>= run
