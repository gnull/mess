{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO

import Web.VKHS (runVK, defaultOptions, apiR)
import Web.VKHS.API.Base (jsonEncodePretty)

main :: IO ()
main = do
  x <- runVK defaultOptions $ apiR "messages.get" [("count", "10")]
  case x of
    (Left e) ->  putStrLn $ show e
    (Right a) -> Data.Text.IO.putStrLn $ jsonEncodePretty a
