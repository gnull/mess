{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (readFile, writeFile)

import Control.Monad (forM_)
import Data.Binary (decode)

import Data.VkMess
  ( Snapshot(..)
  , readFile
  , writeFile
  )

import Options.Applicative
import Data.Semigroup((<>))

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Html.VkMess
  ( mainHtml
  , convPath
  , dialogHtml
  )

optparser :: IO FilePath
optparser = execParser opts
  where
    opts = info (inFile <**> helper)
      ( fullDesc
     <> progDesc "Render messages fetched by mess-fetch as html")
    inFile = argument str $
              metavar "FILE"
           <> help "Input file"

main :: IO ()
main = do
  inFile <- optparser
  (Snapshot ms self users chats) <- decode <$> readFile inFile
  writeFile "index.html" $ renderHtml $ mainHtml users chats self ms
  forM_ ms $ \(d, m) -> writeFile (convPath d) $ renderHtml
                      $ dialogHtml users chats self (d, m)
