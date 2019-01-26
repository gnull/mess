{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (readFile, writeFile)

import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad (forM_)
import Data.Binary (decode)

import Data.VkMess
  ( Snapshot(..)
  , readFile
  , writeFile
  , mDate
  )

import Options.Applicative
import Data.Semigroup((<>))

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Html.VkMess
  ( mainHtml
  , convPath
  , dialogHtml
  , messagesHtml
  , standalone
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
  writeFile "messages.html" $ renderHtml $ standalone "All messages"
                            $ messagesHtml users self
                            $ sortBy (comparing $ mDate . snd)
                            $ concatMap (\(d, m) -> map ((,) d) m) ms
  forM_ ms $ \(d, m) -> writeFile (convPath d) $ renderHtml
                      $ dialogHtml users chats self (d, m)
