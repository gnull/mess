{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Html.VkMess
  ( indexHtml
  , indexHtmlStandalone
  , conversationHtml
  , conversationHtmlStandalone
  , messagesHtml
  , standalone
  , convPath
  ) where
    
import Control.Monad (forM_, when)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.Aeson (decode, Object)
import Data.Aeson.Encode.Pretty (encodePretty)

import Data.List (sort, intersperse, groupBy)
import Data.Function (on)
import Data.Bool (bool)
import Data.Foldable (fold, toList)
import Data.Maybe (fromMaybe, fromJust)
import Data.Set (difference, fromList)
import Data.Monoid (Sum(..))
import Control.Arrow ((&&&))

import Data.UnixTime (UnixTime, formatUnixTimeGMT, webDateFormat)
import Data.VkMess
  ( Message(..)
  , UserId
  , ChatId
  , ChatRecord(..)
  , Attachment(..)
  , DialogStats(..), getDialogStats
  , Conversation(..)
  , convTitle
  )
import Text.Html.VkMess.Static (globalCSS)

import Data.Semigroup((<>))
import Text.Blaze (ToValue(..), Attribute)
import Text.Blaze.Html5 as H
  ( Html
  , docTypeHtml, head, title
  , body, div, p, span, a
  , toHtml
  , preEscapedToHtml
  , style
  , table, tr, td, th
  , (!)
  , meta
  , pre
  , img
  , audio
  , source
  , summary, details
  )

import Text.Blaze.Html5.Attributes (src, class_, href, charset, controls, type_)
import Text.Blaze.Internal (stringValue)

userHtml :: [(UserId, String)] -> UserId -> Html
userHtml us x = H.a ! href url $ name
  where
    name = toHtml $ fromMaybe "Unknown User" $ lookup x us
    url = toValue $ "https://vk.com/id" ++ show x

usersHtml :: [(UserId, String)] -> [UserId] -> Html
usersHtml us = fold . intersperse (stringToHtml ", ") . map (userHtml us) . sort

unixTimeHtml :: UnixTime -> Html
unixTimeHtml = H.span . toHtml . unpack . formatUnixTimeGMT webDateFormat

shortUnixTimeHtml :: UnixTime -> Html
shortUnixTimeHtml = H.span . toHtml . unpack . formatUnixTimeGMT "%b %Y"

wallUrl :: Int -> Int -> FilePath
wallUrl o i = "https://vk.com/wall" ++ show o ++ "_" ++ show i

videoUrl :: Int -> Int -> FilePath
videoUrl o i = "https://vk.com/video" ++ show o ++ "_" ++ show i

attachmentHtml :: Attachment -> Html
attachmentHtml (Photo x) = H.div ! class_ "attachment" $ a ! href (toValue x) $ H.img ! class_ "attachmentPhoto" ! src (toValue x)
attachmentHtml (Sticker x) = H.div ! class_ "attachment" $ H.img ! (src $ stringValue x)
attachmentHtml (Link u t d) = H.div ! class_ "attachment" $ do
  H.a ! (href $ stringValue u) $ toHtml t
  H.p $ toHtml d
attachmentHtml (AudioMsg u) = H.div ! class_ "attachment"
                            $ H.audio ! controls ""
                            $ do source ! src (stringValue u) ! type_ "audio/mpeg"
                                 toHtml ("Your browser does not support the audio element." :: String)
attachmentHtml (Document t u d) = H.div ! class_ "attachment" $ do
  a ! href (toValue u) $ do
    stringToHtml "Document: "
    unixTimeHtml d
    stringToHtml " "
    toHtml t
attachmentHtml (Wall o i d t) = H.div ! class_ "attachment" $ do
  a ! href (toValue $ wallUrl o i) $ stringToHtml "Wall :" <> unixTimeHtml d
  H.p $ toHtml t
attachmentHtml (Video o i t d) = H.div ! class_ "attachment" $ do
  a ! href (toValue $ videoUrl o i) $ stringToHtml "Video: " <> toHtml t
  H.p $ toHtml d
attachmentHtml (Other x) = H.div ! class_ "attachment"
                         $ spoiler "Attachment" $ prettyJsonHtml x

messageStyle :: Bool -> Attribute
messageStyle isTo =
  class_ $ "messageContainer " `mappend` bool "messageFrom" "messageTo" isTo

spoiler :: String -> Html -> Html
spoiler s b = H.details $ H.summary (toHtml s) <> b

prettyJsonHtml :: ByteString -> Html
prettyJsonHtml = H.pre . toHtml . decodeUtf8
               . (encodePretty :: Object -> ByteString) . fromJust . decode

messageHtml :: [(UserId, String)] -> UserId -> Message -> Html
messageHtml us s (Message {..}) = do
  H.div ! messageStyle mOut $ do
    H.div $ do
      userHtml us $ if mOut then s else mUser
      H.span ! class_ "gap" $ mempty
      unixTimeHtml mDate
    H.p $ toHtml mBody
    H.div $ mapM_ attachmentHtml mAtt
    H.div ! class_ "forwardedContainer" $
      forM_ mFwd $ messageHtml us s
    H.div ! class_ "rawContainer" $
      spoiler "Raw JSON" $ prettyJsonHtml $ mJson

conversationHtmlStandalone :: [(UserId, String)] -> UserId -> (Conversation, [Message]) -> Html
conversationHtmlStandalone us s convs = standalone tit $ conversationHtml us s convs
  where tit = "«" ++ convTitle (fst convs) ++ "» — " ++ fromJust (lookup s us)

conversationHtml :: [(UserId, String)] -> UserId -> (Conversation, [Message]) -> Html
conversationHtml us s (conv, ms) = docTypeHtml $ do
  H.head $ do
    title $ toHtml
          $ "«" ++ convTitle conv ++ "» — " ++ fromJust (lookup s us)
    H.meta ! charset "UTF-8"
    H.style $ preEscapedToHtml globalCSS
  H.body ! class_ "dialogBody"
    $ H.div ! class_ "dialogContainer"
    $ mapM_ (messageHtml us s) $ reverse ms

convPath :: Conversation -> FilePath
convPath (ConvUser i _ _) = "user-" ++ show i ++ ".html"
convPath (ConvChat i _) = "chat-" ++ show i ++ ".html"
convPath (ConvGroup i _) = "group-" ++ show i ++ ".html"
convPath (ConvEmail i) = "email-" ++ show i ++ ".html"

hrefFor :: Conversation -> Attribute
hrefFor = href . toValue . convPath

statsHtml :: DialogStats -> Html
statsHtml ds = do
    H.p $ do
      stringToHtml "attachments: "
      toHtml $ show $ getSum $ attachmentCount ds
    H.p $ do
      stringToHtml "forwarded: "
      toHtml $ (show $ getSum $ sentCount ds) ++ "/" ++ (show $ getSum $ totalCount ds)

groupCaptionHtml :: Conversation -> Html
groupCaptionHtml g =  case g of
  (ConvUser _ n i) -> img ! src (toValue i) ! class_ "profileAvatar" <> wrap n
  (ConvChat _ t) ->  stringToHtml "[chat] " <> wrap t
  (ConvGroup _ n) -> stringToHtml "[group] " <> wrap n
  (ConvEmail i) -> wrap $ "Email #" ++ show i
  where wrap = (a ! hrefFor g) . toHtml

groupUsers :: [(ChatId, ChatRecord)] -> Conversation -> [UserId]
groupUsers cs g = case g of
  (ConvChat x _) -> cUsers $ fromJust $ lookup x cs
  _ -> []

-- Users which should be hidden from «Mentioned» list
mentionedToHide :: UserId -> [(ChatId, ChatRecord)] -> Conversation -> [UserId]
mentionedToHide self cs g = self : case g of
  (ConvChat x _) -> cUsers $ fromJust $ lookup x cs -- Oops, this is duplicate of groupUsers
  (ConvUser x _ _) -> [x]
  _ -> []

-- Non-polymorphic helper
stringToHtml :: String -> Html
stringToHtml = toHtml

indexHtmlStandalone :: [(UserId, String)] -> [(ChatId, ChatRecord)] -> UserId -> [(Conversation, [Message])] -> Html
indexHtmlStandalone us cs self items = standalone tit $ indexHtml us cs self items
  where tit = fromJust $ lookup self us

indexHtml :: [(UserId, String)] -> [(ChatId, ChatRecord)] -> UserId -> [(Conversation, [Message])] -> Html
indexHtml us cs self items = do
    H.a ! href "messages.html" $ stringToHtml "All messages in chronological order"
    H.table $ do
      tr $ do
        H.th ! class_ "captionColumn" $ stringToHtml "Conversation"
        H.th ! class_ "statsColumn" $ do
          H.p $ stringToHtml "Stats"
        H.th ! class_ "datesColumn" $ stringToHtml "Activity period"
        H.th ! class_ "usersColumn" $ do
          H.p $ stringToHtml "Users"
      forM_ items $ \(conv, ms) -> H.tr $ do
        let ds = getDialogStats ms
        let start = shortUnixTimeHtml $ mDate $ last ms
        let end = shortUnixTimeHtml $ mDate $ Prelude.head ms
        let cap = groupCaptionHtml conv
        let members = groupUsers cs conv
        let mentioned = toList
                      $ (`difference` (fromList $ mentionedToHide self cs conv))
                      $ usersSeen ds
        H.td cap
        H.td $ statsHtml ds
        H.td $ start <> stringToHtml " … " <> end
        H.td $ do
          when (not $ null members)
            $ H.p $ stringToHtml "Members: " <> usersHtml us members
          when (not $ null mentioned)
            $ H.p $ stringToHtml "Mentioned: " <> usersHtml us mentioned

standalone :: String -> Html -> Html
standalone title_ body_ = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml title_
    H.meta ! charset "UTF-8"
    H.style $ preEscapedToHtml globalCSS
  H.body body_

messagesHtml :: [(UserId, String)] -> UserId -> [(Conversation, Message)] -> Html
messagesHtml us self items = do
  let gs = map (fst . Prelude.head &&& map snd) $ groupBy ((==) `on` fst) items
  H.div ! class_ "dialogContainer" $ do
    forM_ gs $ \(conv, ms) ->
      H.div ! class_ "convContainer" $ do
        H.div ! class_ "groupCaption" $ groupCaptionHtml conv
        H.div $ mapM_ (messageHtml us self) ms
