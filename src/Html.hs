{-# OPTIONS_GHC -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (readFile, putStrLn, writeFile)

import Control.Monad (forM_)
import Data.Binary (decode)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Text (unpack)

import Data.List (sortOn, sort, intersperse)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe, fromJust)

import Data.UnixTime (UnixTime, formatUnixTimeGMT, webDateFormat)
import Data.VkMess
  ( Message(..)
  , MessageAddr(..)
  , Snapshot(..)
  , MessageGroup(..)
  , messageGroup
  , isMessageTo
  , messageAuthor
  , UserId
  , ChatId
  , ChatRecord(..)
  , readFile, writeFile
  , Attachment(..)
  )

import Options.Applicative
import Data.Semigroup((<>))
import Text.Blaze (ToValue(..), Attribute)
import Text.Blaze.Html5 as H
  ( Html
  , docTypeHtml, head, title
  , body, div, p, span, a
  , toHtml
  , (!)
  , meta
  , pre
  , img
  )

import Text.Blaze.Html5.Attributes (src, style, href, charset)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Html.VkMess

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
  writeFile "index.html" $ renderHtml $ mainHtml users chats self $ map fst ms
  forM_ ms $ \(d, m) -> do
    let g = messageGroup $ mAddr d
    writeFile (urlFor g) $ renderHtml $ dialogHtml users chats self $ sortOn mDate m
