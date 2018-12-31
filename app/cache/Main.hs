module Main where

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.ByteString.Lazy.Char8 (unpack)

main :: IO ()
main = do
  r <- get "http://httpbin.org/get"
  putStrLn $ unpack $ r ^. responseBody
