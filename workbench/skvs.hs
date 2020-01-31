#!/usr/bin/env stack
{- stack
    --resolver lts-14.22
    --install-ghc runghc
    --package aeson
    --package servant-server
    --package text
    --package transformers
    --package unordered-containers
    --package warp
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- https://vaibhavsagar.com/blog/2017/01/24/simple-kv-store/

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap, empty, insert, lookup)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (getArgs)
import Prelude hiding (lookup)

type API =
  "get" :> Capture "key" Text :> Get '[JSON] (Maybe Value)
    :<|> "put" :> Capture "key" Text :> ReqBody '[JSON] Value :> Put '[JSON] Text

type Store = IORef (HashMap Text Value)

server :: Store -> Server API
server store = getValue store :<|> putValue store

getValue :: Store -> Text -> Handler (Maybe Value)
getValue store key = do
  liftIO . putStrLn $ "Looking for " <> show key <> "…"
  liftIO $ lookup key <$> readIORef store

putValue :: Store -> Text -> Value -> Handler Text
putValue store key value = do
  liftIO . putStrLn $ "Putting " <> show key <> " = " <> show value
  liftIO $ atomicModifyIORef' store f
  where
    f kv = (insert key value kv, key)

kvAPI :: Proxy API
kvAPI = Proxy

port :: Int
port = 8080

main :: IO ()
main = run port . serve kvAPI . server =<< newIORef empty
