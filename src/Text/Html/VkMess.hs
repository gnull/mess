{-# OPTIONS_GHC -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Html.VkMess where
    
import Control.Monad (forM_)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Text (unpack)

import Data.List (sort, intersperse)
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe, fromJust)

import Data.UnixTime (UnixTime, formatUnixTimeGMT, webDateFormat)
import Data.VkMess
  ( Message(..)
  , MessageAddr(..)
  , MessageGroup(..)
  , messageGroup
  , isMessageTo
  , messageAuthor
  , UserId
  , ChatId
  , ChatRecord(..)
  , Attachment(..)
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
  , details, summary
  )

import Text.Blaze.Html5.Attributes (src, class_, href, charset, controls, type_)
import Text.Blaze.Internal (stringValue)

addrHtml :: [(UserId, String)] -> UserId -> MessageAddr -> Html
addrHtml us s x = H.span . (a ! href url) . toHtml
                $ fromMaybe "Unknown User"
                $ flip lookup us
                $ messageAuthor s x where
  url = toValue $ "https://vk.com/id" ++ show (messageAuthor s x)

unixTimeHtml :: UnixTime -> Html
unixTimeHtml = H.span . toHtml . unpack . formatUnixTimeGMT webDateFormat

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
attachmentHtml (Other x) = H.span ! class_ "attachmentOther" $ H.pre $ toHtml $ Data.ByteString.Lazy.Char8.unpack x

messageStyle :: Bool -> Attribute
messageStyle isTo =
  class_ $ "messageContainer " `mappend` bool "messageFrom" "messageTo" isTo

messageHtml :: [(UserId, String)] -> UserId -> Message -> Html
messageHtml us s (Message {..}) = do
  H.div ! messageStyle (isMessageTo mAddr) $ do
    H.div $ do
      addrHtml us s mAddr
      H.span ! class_ "gap" $ toHtml ("" :: String)
      unixTimeHtml mDate
    p $ toHtml mBody
    H.div $ mapM_ attachmentHtml mAtt
    H.div ! class_ "forwardedContainer" $
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
    H.style $ preEscapedToHtml globalCSS
  H.body ! class_ "dialogBody"
    $ H.div ! class_ "dialogContainer"
    $ mapM_ (messageHtml us s) ms

class Urlable a where
  urlFor :: a -> String

hrefFor :: MessageGroup -> Attribute
hrefFor = href . toValue . urlFor

instance Urlable MessageGroup where
  urlFor = (++ ".html") . show

spoiler :: String -> Html -> Html
spoiler s b = H.details $ H.summary (toHtml s) <> b

groupCaption :: [(UserId, String)] -> [(ChatId, ChatRecord)] -> Message -> (Html, Html)
groupCaption us cs g =  case messageGroup $ mAddr $ g of
  (MessageChat x) -> let tit = wrap $ ("☭ " ++) $ Data.Text.unpack $ cTitle $ fromJust $ lookup x cs
                         f u = toHtml $ fromMaybe "Unknown User" $ lookup u us
                     in  (tit, spoiler "Open" $ fold $ intersperse (toHtml (", " :: String)) $ map f (cUsers $ fromJust $ lookup x cs))
  (MessageDialog x) -> (wrap $ fromMaybe "Unknown user" $ lookup x us, mempty)
  where wrap = (a ! hrefFor (messageGroup $ mAddr $ g)) . toHtml

mainHtml :: [(UserId, String)] -> [(ChatId, ChatRecord)] -> UserId -> [Message] -> Html
mainHtml us cs self ms = docTypeHtml $ do
  H.head $ do
    H.title $ do
      toHtml ("«" :: String)
      toHtml $ fromJust $ lookup self us
      toHtml ("»" :: String)
    H.meta ! charset "UTF-8"
    H.style $ preEscapedToHtml globalCSS
  body $ H.table $ do
    tr $ do
      H.th ! class_ "captionColumn" $ mempty
      H.th ! class_ "usersColumn" $ mempty
    forM_ ms $ row . groupCaption us cs
  where row (x, y) = H.tr $ H.td x <> H.td y
