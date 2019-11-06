#! /usr/bin/env nix-shell
#! nix-shell -p "haskell.packages.ghc881.ghcWithPackages (ps: [ ps.text ])" -i "runghc --ghc-arg=-Wall --ghc-arg=-Werror"
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T

main :: IO ()
main = T.putStrLn "Hello world"
