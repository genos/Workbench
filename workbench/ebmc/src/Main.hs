{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import qualified Data.ByteString.Lazy as B
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Wreq
import Safe
import Text.HTML.Scalpel.Core

body :: IO Text
body = do
  response <- get "https://twitter.com/search?q=from:%40evilbmcats"
  return $ response ^. responseBody & B.toStrict & T.decodeUtf8

pTweetText :: Selector
pTweetText = "p" @: [hasClass "tweet-text"]

tweets :: Scraper Text [Text]
tweets =
  chroots pTweetText $ fst . T.breakOn "pic.twitter.com" <$> text anySelector

images :: Scraper Text [Text]
images = attrs "src" $ "img" @: ["style" @= "width: 100%; top: -0px;"]

hrefs :: Scraper Text [Text]
hrefs = chroots pTweetText $ innerHTML "a"

tweetImageHref :: Text -> Int -> Maybe (Text, Text, Text)
tweetImageHref b n = do
  t <- (`atMay` n) =<< scrapeStringLike b tweets
  i <- (`atMay` n) =<< scrapeStringLike b images
  h <- (`atMay` n) =<< scrapeStringLike b hrefs
  return (t, i, h)

fmt :: (Text, Text, Text) -> Text
fmt (tweet, image, href) = ""

main :: IO ()
main = putStrLn "hi"
