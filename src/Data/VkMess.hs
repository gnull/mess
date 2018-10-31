{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VkMess
  ( Message(..)
  , MessageAddr(..)
  , Snapshot(..)
  , MessageGroup(..)
  , messageGroup
  , isMessageTo
  , UserId
  , ChatId
  , ChatRecord(..)
  , messageAuthor
  , Dialog(..)
  , readFile, writeFile
  , vkImageSizes, Attachment(..)
  ) where

import Prelude hiding (readFile, writeFile)

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.Aeson (encode)
import qualified Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (forM)
import Data.Text (Text, unpack, pack)
import Data.UnixTime (fromEpochTime, UnixTime, formatUnixTimeGMT, webDateFormat)
import Foreign.C.Types (CTime(CTime))

import Data.Binary
import GHC.Generics (Generic)

type MessageId    = Int
type UserId       = Int
type ChatId       = Int

data ChatRecord = ChatRecord
  { cTitle :: Text, cAdmin :: UserId, cUsers :: [UserId] } deriving (Show)

instance FromJSON ChatRecord where
  parseJSON = withObject "chat" $ \v -> do
    cTitle <- v .: "title"
    cAdmin <- v .: "admin_id"
    cUsers <- v .: "users"
    return $ ChatRecord {..}

-- | Id of chat or dialog this message belongs to
data MessageAddr = MessageToChat ChatId
                 | MessageFromChat UserId ChatId -- This one contains source user id
                 | MessageToDialog UserId
                 | MessageFromDialog UserId deriving (Ord, Eq, Generic, Show)

isMessageTo :: MessageAddr -> Bool
isMessageTo (MessageToChat   _) = True
isMessageTo (MessageToDialog _) = True
isMessageTo _ = False

-- Given self uid and message addr, return uid of message author
messageAuthor :: UserId -> MessageAddr -> UserId
messageAuthor s (MessageToChat _    ) = s
messageAuthor s (MessageToDialog _  ) = s
messageAuthor _ (MessageFromChat x _) = x
messageAuthor _ (MessageFromDialog x) = x

data MessageGroup = MessageChat ChatId | MessageDialog UserId deriving (Ord, Eq)

instance Show MessageGroup where
  show (MessageChat   x) = "c" ++ show x
  show (MessageDialog x) = "id" ++ show x

messageGroup :: MessageAddr -> MessageGroup
messageGroup (MessageToChat     x) = MessageChat x
messageGroup (MessageFromChat _ x) = MessageChat x
messageGroup (MessageToDialog   x) = MessageDialog x
messageGroup (MessageFromDialog x) = MessageDialog x

data Attachment = Photo [(Int, FilePath)] | Other ByteString deriving (Generic, Show)

vkImageSizes = [2560, 1280, 807, 604, 130, 75]
instance FromJSON Attachment where
  parseJSON = withObject "attachment" $ \v -> do
    t <- v .: "type"
    if t /= ("photo" :: Text) then
      pure $ Other $ Data.Aeson.encode v
    else do
      v' <- v .: "photo"
      x <- forM vkImageSizes $ \s -> do
        url <- v' .:? ("photo_" `mappend` pack (show s))
        pure $ case url of
          (Just u) -> Just (s, u)
          Nothing  -> Nothing
      pure $ Photo $ catMaybes x

data Message = Message {
                 mBody :: Text
               , mDate :: UnixTime
               , mAddr :: MessageAddr
               , mFwd  :: [Message]
               , mAtt  :: [Attachment]
               } deriving (Generic, Show)

instance FromJSON UnixTime where
  parseJSON = fmap (fromEpochTime . CTime) . parseJSON

instance FromJSON Message where
  parseJSON = withObject "message" $ \v -> do
    mBody <- v .: "body"
    mDate <- v .: "date"
    out <- (/= (0 :: Int)) <$> fromMaybe 0 <$> v .:? "out"
    chatId <- v .:? "chat_id"
    let uid = v .: "user_id"
    mAddr <- case chatId of
                  (Just chatId') -> if out
                    then MessageToChat   <$> return chatId'
                    else MessageFromChat <$> uid <*> return chatId'
                  Nothing -> if out
                    then MessageToDialog   <$> uid
                    else MessageFromDialog <$> uid
    mFwd <- fromMaybe [] <$> v .:? "fwd_messages"
    mAtt <- fromMaybe [] <$> v .:? "attachments"
    return $ Message {..}

data Dialog = Dialog {dMess :: Message}

instance FromJSON Dialog where
  parseJSON = withObject "dialog" $ \v -> do
    x <- v .: "message"
    return $ Dialog x

data Snapshot = Snapshot { sDialogs :: [(Message, [Message])]
                         , sSelf :: UserId
                         , sUsers :: [(UserId, String)]
                         } deriving (Generic, Show)

instance Binary MessageAddr
instance Binary Attachment
instance Binary Message
instance Binary Snapshot
