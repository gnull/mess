{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (readFile, putStrLn, writeFile)

import Control.Monad (forM_)
import Data.Binary (encode, decode)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 (unpack)

import Data.Function (on)
import Data.List (nub, sortOn, groupBy, sort)
import Data.Maybe (fromMaybe)

import Data.UnixTime (UnixTime, formatUnixTimeGMT, webDateFormat)
import Data.VkMess ( Message(..)
                   , MessageAddr(..)
                   , Snapshot(..)
                   , MessageGroup(..)
                   , messageGroup
                   , isMessageTo
                   , messageAuthor
                   , UserId
                   , readFile, writeFile
                   , Attachment(..)
                   )

import Options.Applicative
import Data.Semigroup((<>))
import Text.Blaze (ToValue(..))
import Text.Blaze.Html5 as H ( Html
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
addrHtml us s x = H.span . toHtml
                $ fromMaybe "Unknown User"
                $ flip lookup us
                $ messageAuthor s x where

unixTimeHtml :: UnixTime -> Html
unixTimeHtml = H.span . toHtml . unpack . formatUnixTimeGMT webDateFormat

attachmentHtml :: Attachment -> Html
attachmentHtml (Other x) = H.span ! style "border: 1px solid grey;" $ H.pre ! style "white-space: pre-wrap;" $ toHtml $ Data.ByteString.Lazy.Char8.unpack x
attachmentHtml (Photo xs) = H.span $ H.img ! src (toValue $ snd $ Prelude.head $ reverse $ sort $ xs)

messageHtml :: [(UserId, String)] -> UserId -> Message -> Html
messageHtml us s (Message {..}) = do
  H.div ! style ("border: 1px " `mappend` (if isMessageTo mAddr then "dashed" else "solid") `mappend` " black; background-color: #ddd; margin: 1px; padding: 2px;") $ do
    H.div $ do
      addrHtml us s mAddr
      H.span ! style "display: inline-block; width: 0.5cm;" $ toHtml ("" :: String)
      unixTimeHtml mDate
    p $ toHtml mBody
    H.div $ mapM_ attachmentHtml mAtt
    H.div ! style "padding-left: 10px;" $
      forM_ mFwd $ messageHtml us s

dialogHtml :: [(UserId, String)] -> UserId -> [Message] -> Html
dialogHtml us s ms = docTypeHtml $ do
  H.head $ do
    title "Dialog"
    H.meta ! charset "UTF-8"
  H.body ! style "font-family: Verdana, Sans-Serif; font-size: 14.4px;"
    $ H.div ! style "width: 500px; word-wrap: break-word;"
    $ mapM_ (messageHtml us s) ms

class Urlable a where
  urlFor :: a -> String

-- Type returned by this function is not exported by Blaze, therefore this
-- function has no signature
hrefFor = href . toValue . urlFor

instance Urlable MessageGroup where
  urlFor = (++ ".html") . show

groupCaption :: [(UserId, String)] -> Message -> String
groupCaption us g = case messageGroup $ mAddr $ g of
  x@(MessageChat _) -> show x
  (MessageDialog x) -> fromMaybe "Unknown user" $ lookup x us

mainHtml :: [(UserId, String)] -> UserId -> [Message] -> Html
mainHtml us self ms = docTypeHtml $ do
  H.head $ do
    H.title $ do
      toHtml ("Messages of " :: String)
      toHtml $ fromMaybe "Unknown User" $ lookup self us
    H.meta ! charset "UTF-8"
  body $ do
    forM_ ms $ \g -> H.div $ a ! hrefFor (messageGroup $ mAddr $ g) $ toHtml $ groupCaption us g

optparser :: IO FilePath
optparser = execParser opts
  where
    opts = info (inFile <**> helper)
      ( fullDesc
     <> progDesc "View messages fetched by mess-fetch")
    inFile = argument str $
              metavar "FILE"
           <> help "Input file"

main = do
  inFile <- optparser
  (Snapshot ms self users) <- decode <$> readFile inFile
  writeFile "index.html" $ renderHtml $ mainHtml users self $ map fst ms
  forM_ ms $ \(d, m) -> do
    let g = messageGroup $ mAddr d
    writeFile (urlFor g) $ renderHtml $ dialogHtml users self $ sortOn mDate m
