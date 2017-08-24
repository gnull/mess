{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (readFile, putStrLn, writeFile)

import Control.Monad (forM_)
import Data.Binary (encode, decode)
import Data.ByteString.Lazy.Char8 (readFile, putStrLn, writeFile)
import Data.ByteString.Char8 (unpack)

import Data.Function (on)
import Data.List (nub, sortOn, groupBy)
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
                   )

import Options.Applicative
import Data.Semigroup((<>))
import Text.Blaze (ToValue(..))
import Text.Blaze.Html5 as H ( Html
                             , docTypeHtml, head, title
                             , body, hr, div, p, span, a
                             , toHtml
                             , (!)
                             )
import Text.Blaze.Html5.Attributes (src, style, href)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

addrHtml :: [(UserId, String)] -> UserId -> MessageAddr -> Html
addrHtml us s x = H.span . toHtml
                $ fromMaybe "Unknown User"
                $ flip lookup us
                $ messageAuthor s x where

unixTimeHtml :: UnixTime -> Html
unixTimeHtml = H.span . toHtml . unpack . formatUnixTimeGMT webDateFormat

messageHtml :: [(UserId, String)] -> UserId -> Message -> Html
messageHtml us s (Message {..}) = do
  H.div ! style "border: 1px solid black; background-color: #ddd; margin: 1px; padding: 2px;" $ do
    H.div $ do
      addrHtml us s mAddr
      H.span ! style "display: inline-block; width: 0.5cm;" $ toHtml ("" :: String)
      unixTimeHtml mDate
    p $ toHtml mBody

dialogHtml :: [(UserId, String)] -> UserId -> [Message] -> Html
dialogHtml us s ms = H.body ! style "font-family: Verdana, Sans-Serif; font-size: 14.4px;"
              $ H.div ! style "width: 500px; word-wrap: break-word;"
              $ mapM_ (messageHtml us s) ms

class Urlable a where
  urlFor :: a -> String

-- Type returned by this function is not exported by Blaze, therefore this
-- function has no signature
hrefFor = href . toValue . urlFor

instance Urlable MessageGroup where
  urlFor = (++ ".html") . show

groupCaption :: [(UserId, String)] -> [Message] -> String
groupCaption us g = case messageGroup $ mAddr $ Prelude.head g of
  x@(MessageChat _) -> show x
  (MessageDialog x) -> fromMaybe "Unknown user" $ lookup x us

mainHtml :: [(UserId, String)] -> [[Message]] -> Html
mainHtml us gs = docTypeHtml $ do
  H.head $ do
    H.title "My title"
  body $ do
    forM_ gs $ \g -> H.div $ a ! hrefFor (messageGroup $ mAddr $ Prelude.head g) $ toHtml $ groupCaption us g

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
  (Snapshot ms self users) <- decode <$> readFile inFile
  let cs = groupBy (on (==) (messageGroup . mAddr)) $ sortOn (messageGroup . mAddr) ms
  writeFile "index.html" $ renderHtml $ mainHtml users cs
  forM_ cs $ \c -> do
    let g = messageGroup $ mAddr $ Prelude.head c
    writeFile (urlFor g) $ renderHtml $ dialogHtml users self $ sortOn mDate c
