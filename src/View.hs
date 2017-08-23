{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (readFile, putStrLn)

import Control.Monad (sequence_)
import Data.Binary (encode, decode)
import Data.ByteString.Lazy.Char8 (readFile, putStrLn)
import Data.ByteString.Char8 (unpack)

import Data.Function (on)
import Data.List (sortOn, groupBy, intersperse)

import Data.UnixTime (UnixTime, formatUnixTimeGMT, webDateFormat)
import Data.VkMess ( Message(..)
                   , MessageAddr(..)
                   , Snapshot(..)
                   , MessageGroup(..)
                   , messageGroup
                   , isMessageTo
                   )

import Options.Applicative
import Data.Semigroup((<>))

import Text.Blaze.Html5 as H ( Html
                             , docTypeHtml, head, title
                             , body, hr, div, pre, span
                             , toHtml
                             , (!)
                             )
import Text.Blaze.Html5.Attributes (src, style)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

addrHtml :: MessageAddr -> Html
addrHtml x = H.span . toHtml $ field ++ show (messageGroup x) where
  field = if isMessageTo x then "To: " else "From: "

unixTimeHtml :: UnixTime -> Html
unixTimeHtml = H.span . toHtml . unpack . formatUnixTimeGMT webDateFormat

messageHtml :: Message -> Html
messageHtml (Message {..}) = do
  H.div $ do
    H.div $ do
      addrHtml mAddr
      unixTimeHtml mDate
    pre $ toHtml mBody

dialogHtml :: [Message] -> Html
dialogHtml = mapM_ messageHtml

mainHtml :: Snapshot -> Html
mainHtml (Snapshot ms) = docTypeHtml $ do
  H.head $ do
    H.title "My title"
  body $ do
    sequence_ $ intersperse hr $ map (dialogHtml . sortOn mDate) $ groupBy (on (==) $ messageGroup . mAddr) $ sortOn (messageGroup . mAddr) ms

optparser :: IO FilePath
optparser = execParser opts
  where
    opts = info (inFile <**> helper)
      ( fullDesc
     <> progDesc "View messages fetched by mess-fetch")
    inFile = argument str $
              metavar "FILE"
           <> help "Input file"

-- TODO: Implement simple program to render these messages as an HTML page
--    See: https://hackage.haskell.org/package/blaze-html
--         https://jaspervdj.be/blaze/tutorial.html
main :: IO ()
main = do
  inFile <- optparser
  ms <- decode <$> readFile inFile
  putStrLn $ renderHtml $ mainHtml ms
