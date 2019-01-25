{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Html.VkMess where
    
import Control.Monad (forM_, when)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.Aeson (decode, Object)
import Data.Aeson.Encode.Pretty (encodePretty)

import Data.List (sort, intersperse)
import Data.Bool (bool)
import Data.Foldable (fold, toList)
import Data.Maybe (fromMaybe, fromJust)
import Data.Set (difference, fromList)
import Data.Monoid (Sum(..))

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

attachmentHtml :: Attachment -> Html
attachmentHtml (Photo xs) = H.span $ a ! href url $ H.img ! class_ "attachmentPhoto" ! src url
  where url = (toValue $ snd $ Prelude.head $ reverse $ sort $ xs)
attachmentHtml (Sticker x) = H.span $ H.img ! (src $ stringValue x)
attachmentHtml (Link u t d) = H.span $ do
  H.a ! (href $ stringValue u) $ toHtml t
  H.p $ toHtml d
attachmentHtml (AudioMsg u) = H.span
                            $ H.audio ! controls ""
                            $ do source ! src (stringValue u) ! type_ "audio/mpeg"
                                 toHtml ("Your browser does not support the audio element." :: String)
attachmentHtml (Other x) = H.span ! class_ "attachmentOther"
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

dialogHtml :: [(UserId, String)] -> [(ChatId, ChatRecord)] -> UserId -> (Conversation, [Message]) -> Html
dialogHtml us _ s (conv, ms) = docTypeHtml $ do
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

mentionedToHide :: UserId -> [(ChatId, ChatRecord)] -> Conversation -> [UserId]
mentionedToHide self cs g = self : case g of
  (ConvChat x _) -> cUsers $ fromJust $ lookup x cs -- Oops, this is duplicate of groupUsers
  (ConvUser x _ _) -> [x]
  _ -> []

-- Non-polymorphic helper
stringToHtml :: String -> Html
stringToHtml = toHtml

mainHtml :: [(UserId, String)] -> [(ChatId, ChatRecord)] -> UserId -> [(Conversation, [Message])] -> Html
mainHtml us cs self items = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml $ fromJust $ lookup self us
    H.meta ! charset "UTF-8"
    H.style $ preEscapedToHtml globalCSS
  body $ H.table $ do
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
