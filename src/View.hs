{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile)

import Data.Binary (encode, decode)
import Data.ByteString.Lazy (readFile)
import Data.VkMess (Message(..), Snapshot(..), addrEq, whateverId)

import Options.Applicative
import Data.Semigroup((<>))

optparser :: IO FilePath
optparser = execParser opts
  where
    opts = info (inFile <**> helper)
      ( fullDesc
     <> progDesc "View messages fetched by mess-fetch")
    inFile = argument str $
              metavar "FILE"
           <> help "Input file"

main :: IO ()
main = do
  inFile <- optparser
  (Snapshot ms) <- decode <$> readFile inFile
  putStrLn $ show $ length ms
