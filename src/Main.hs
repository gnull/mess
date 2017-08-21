{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO

import Web.VKHS (runVK, defaultOptions, apiR)
import Web.VKHS.API.Base (jsonEncodePretty)

main :: IO ()
main = do
  x <- runVK defaultOptions $ apiR "users.get" [("user_ids", "1,2"), ("fields", "last_seen")]
  case x of
    (Left e) ->  putStrLn $ show e
    (Right a) -> Data.Text.IO.putStrLn $ jsonEncodePretty a
