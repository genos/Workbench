{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Monoid ((<>))
import Data.Random
import Data.Text (breakOn, Text)
import qualified Data.Text.IO as T
import System.Exit (die)
import Text.HTML.Scalpel

data Tweet = Tweet { _text :: !Text, _image :: !Text, _href :: !Text }

fmt :: Tweet -> Text
fmt t =
  "**"
    <> _text t
    <> "**\n![Black Metal Cats]("
    <> _image t
    <> ")\n*[@evilbmcats on Twitter]("
    <> _href t
    <> ")*"

url :: URL
url = "https://twitter.com/search?q=from:%40evilbmcats"

allTweets :: Scraper Text [Tweet]
allTweets = chroots ("div" @: [hasClass "content"]) $ do
  textAndHref <- text $ "p" @: [hasClass "tweet-text"]
  let (_text, _href) = breakOn "pic.twitter.com" textAndHref
  _image <- attr "src" "img"
  return Tweet {..}

randomTweet :: [Tweet] -> IO Tweet
randomTweet ts = runRVar (randomElement ts) StdRandom

main :: IO ()
main = do
  mts <- scrapeURL url allTweets
  case mts of
    Nothing -> die "Unable to scrape tweets"
    Just ts -> T.putStrLn . fmt =<< randomTweet ts
