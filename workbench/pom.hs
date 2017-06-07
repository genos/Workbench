#!/usr/bin/env stack
-- stack --resolver lts-8.17 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
module Main where

import           Data.Default
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Options.Applicative
import           Options.Applicative.Text
import           System.Exit              (exitFailure)

tshow :: Show a => a -> Text
tshow = T.pack . show

data Options = Options
  { oRetryCount    :: !Int
  , oHost          :: !Text
  , oCharacterCode :: Maybe Char
  } deriving (Show, Eq)

data PartialOptions = PartialOptions
  { poRetryCount    :: Last Int
  , poHost          :: Last Text
  , poCharacterCode :: Last (Maybe Char)
  } deriving (Show, Eq)

instance Monoid PartialOptions where
  mempty = PartialOptions mempty mempty mempty
  x `mappend` y = PartialOptions
    { poRetryCount = poRetryCount x <> poRetryCount y
    , poHost = poHost x <> poHost y
    , poCharacterCode = poCharacterCode x <> poCharacterCode y
    }

lastToEither :: Text -> Last a -> Either Text a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

makeOptions :: PartialOptions -> Either Text Options
makeOptions PartialOptions {..} = do
  oRetryCount    <- lastToEither "Missing retry count" poRetryCount
  oHost          <- lastToEither "Missing host" poHost
  oCharacterCode <- lastToEither "Missing character code" poCharacterCode
  return Options {..}

instance Default PartialOptions where
  def = mempty {poRetryCount = pure 5, poCharacterCode = pure $ Just 'c'}

lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

partialOptionsParser :: Parser PartialOptions
partialOptionsParser =
  PartialOptions
    <$> lastOption (option auto $ long "retry-count")
    <*> lastOption (option text $ long "host")
    <*> lastOption
          (   fmap  Just    (option auto $ long "character-code")
          <|> flag' Nothing (long "no-character-code")
          )

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
