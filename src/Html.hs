{-# OPTIONS_GHC -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (readFile, putStrLn, writeFile)

import Control.Monad (forM_)
import Data.Binary (encode, decode)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Text (unpack)

import Data.Function (on)
import Data.List (nub, sortOn, groupBy, sort)
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
import Text.Blaze (ToValue(..))
import Text.Blaze.Html5 as H
  ( Html
  , docTypeHtml, head, title
  , body, hr, div, p, span, a
  , toHtml
  , (!)
  , meta
  , pre
  , img
  )

import Text.Blaze.Html5.Attributes (src, style, href, charset)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

addrHtml :: [(UserId, String)] -> UserId -> MessageAddr -> Html
addrHtml us s x = H.span . (a ! href url) . toHtml
                $ fromMaybe "Unknown User"
                $ flip lookup us
                $ messageAuthor s x where
  url = toValue $ "https://vk.com/id" ++ show (messageAuthor s x)

unixTimeHtml :: UnixTime -> Html
unixTimeHtml = H.span . toHtml . unpack . formatUnixTimeGMT webDateFormat

attachmentHtml :: Attachment -> Html
attachmentHtml (Other x) = H.span ! style "border: 1px solid grey;" $ H.pre ! style "white-space: pre-wrap;" $ toHtml $ Data.ByteString.Lazy.Char8.unpack x
attachmentHtml (Photo xs) = H.span $ a ! href url $ H.img ! style "height: auto; max-width: 100%;" ! src url
  where url = (toValue $ snd $ Prelude.head $ reverse $ sort $ xs)

messageHtml :: [(UserId, String)] -> UserId -> Message -> Html
messageHtml us s (Message {..}) = do
  H.div ! style ((if isMessageTo mAddr then "border: 1px dashed black; background-color: #fff;" else "border: 1px solid black; background-color: #ddd;") `mappend` "margin: 1px; padding: 2px;") $ do
    H.div $ do
      addrHtml us s mAddr
      H.span ! style "display: inline-block; width: 0.5cm;" $ toHtml ("" :: String)
      unixTimeHtml mDate
    p ! style "white-space: pre-wrap;" $ toHtml mBody
    H.div $ mapM_ attachmentHtml mAtt
    H.div ! style "padding-left: 10px;" $
      forM_ mFwd $ messageHtml us s

dialogHtml :: [(UserId, String)] -> [(ChatId, ChatRecord)] -> UserId -> [Message] -> Html
dialogHtml us cs s ms = docTypeHtml $ do
  H.head $ do
    title $ toHtml
          $ let addressee = case messageGroup $ mAddr $ Prelude.head ms of
                     (MessageChat x) -> Data.Text.unpack $ cTitle $ fromJust $ lookup x cs
                     (MessageDialog x) -> fromMaybe "Unknown User" $ lookup x us
            in "«" ++ addressee ++ "» — " ++ fromJust (lookup s us)
    H.meta ! charset "UTF-8"

  H.body ! style "font-family: Verdana, Sans-Serif; font-size: 14.4px;"
    $ H.div ! style "width: 700px; word-wrap: break-word; margin: auto;"
    $ mapM_ (messageHtml us s) ms

class Urlable a where
  urlFor :: a -> String

-- Type returned by this function is not exported by Blaze, therefore this
-- function has no signature
hrefFor = href . toValue . urlFor

instance Urlable MessageGroup where
  urlFor = (++ ".html") . show

groupCaption :: [(UserId, String)] -> [(ChatId, ChatRecord)] -> Message -> Html
groupCaption us cs g = H.div
                   $ a ! hrefFor (messageGroup $ mAddr $ g)
                   $ toHtml
                   $ case messageGroup $ mAddr $ g of
  (MessageChat x) -> ("☭ " ++) $ Data.Text.unpack $ cTitle $ fromJust $ lookup x cs
  (MessageDialog x) -> fromMaybe "Unknown user" $ lookup x us

mainHtml :: [(UserId, String)] -> [(ChatId, ChatRecord)] -> UserId -> [Message] -> Html
mainHtml us cs self ms = docTypeHtml $ do
  H.head $ do
    H.title $ do
      toHtml ("«" :: String)
      toHtml $ fromMaybe "Unknown User" $ lookup self us
      toHtml ("»" :: String)
    H.meta ! charset "UTF-8"
  body $ do
    forM_ ms $ groupCaption us cs

optparser :: IO FilePath
optparser = execParser opts
  where
    opts = info (inFile <**> helper)
      ( fullDesc
     <> progDesc "Render messages fetched by mess-fetch as html")
    inFile = argument str $
              metavar "FILE"
           <> help "Input file"

main = do
  inFile <- optparser
  (Snapshot ms self users chats) <- decode <$> readFile inFile
  writeFile "index.html" $ renderHtml $ mainHtml users chats self $ map fst ms
  forM_ ms $ \(d, m) -> do
    let g = messageGroup $ mAddr d
    writeFile (urlFor g) $ renderHtml $ dialogHtml users chats self $ sortOn mDate m
