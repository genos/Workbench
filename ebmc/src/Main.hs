{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Random
import Data.Text (Text, breakOn)
import qualified Data.Text.IO as T
import System.Exit (die)
import Text.HTML.Scalpel

data Tweet = Tweet {_text :: {-# UNPACK #-} !Text, _image :: {-# UNPACK #-} !Text, _href :: {-# UNPACK #-} !Text}

fmt :: Tweet -> Text
fmt Tweet{_text, _image, _href} = "**" <> _text <> "**\n![Black Metal Cats](" <> _image <> ")\n*[@evilbmcats on Twitter](" <> _href <> ")*"

url :: URL
url = "https://twitter.com/search?q=from:%40evilbmcats"

allTweets :: Scraper Text [Tweet]
allTweets = chroots ("div" @: [hasClass "content"]) $ do
    textAndHref <- text $ "p" @: [hasClass "tweet-text"]
    let (_text, _href) = breakOn "pic.twitter.com" textAndHref
    _image <- attr "data-image-url" $ "div" @: [hasClass "js-adaptive-photo"]
    return Tweet{_text, _image, _href}

randomTweet :: [Tweet] -> IO Tweet
randomTweet ts = runRVar (randomElement ts) StdRandom

main :: IO ()
main = do
    mts <- scrapeURL url allTweets
    case mts of
        Nothing -> die "Unable to scrape tweets"
        Just ts -> T.putStrLn . fmt =<< randomTweet ts
