{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VkMess ( Message(..)
                   , MessageAddr(..)
                   , Snapshot(..)
                   , MessageGroup(..)
                   , messageGroup
                   , isMessageTo
                   ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.ByteString.Char8 (unpack)
import Data.Text (Text, unpack)
import Data.UnixTime (fromEpochTime, UnixTime, formatUnixTimeGMT, webDateFormat)
import Foreign.C.Types (CTime(CTime))

import Data.Binary
import GHC.Generics (Generic)

type MessageId    = Int
type UserId       = Int
type ChatId       = Int

-- | Id of chat or dialog this message belongs to
data MessageAddr = MessageToChat ChatId
                 | MessageFromChat UserId ChatId -- This one contains source user id
                 | MessageToDialog UserId
                 | MessageFromDialog UserId deriving (Ord, Eq, Generic, Show)

isMessageTo :: MessageAddr -> Bool
isMessageTo (MessageToChat   _) = True
isMessageTo (MessageToDialog _) = True
isMessageTo _ = False

data MessageGroup = MessageChat ChatId | MessageDialog UserId deriving (Show, Ord, Eq)

messageGroup :: MessageAddr -> MessageGroup
messageGroup (MessageToChat     x) = MessageChat x
messageGroup (MessageFromChat _ x) = MessageChat x
messageGroup (MessageToDialog   x) = MessageDialog x
messageGroup (MessageFromDialog x) = MessageDialog x

data Message = Message {
                 mId   :: MessageId
               , mBody :: Text
               , mDate :: UnixTime
               , mRead :: Bool
               , mAddr :: MessageAddr
               } deriving (Generic, Show)

instance FromJSON UnixTime where
  parseJSON = fmap (fromEpochTime . CTime) . parseJSON

instance FromJSON Message where
  parseJSON = withObject "message" $ \v -> do
    mId <- v .: "id"
    mBody <- v .: "body"
    mDate <- v .: "date"
    mRead <- (/= (0 :: Int)) <$> v .: "read_state"
    out <- (/= (0 :: Int)) <$> v .: "out"
    chatId <- v .:? "chat_id"
    let uid = v .: "user_id"
    mAddr <- case chatId of
                  (Just chatId') -> if out
                    then MessageToChat   <$> return chatId'
                    else MessageFromChat <$> uid <*> return chatId'
                  Nothing -> if out
                    then MessageToDialog   <$> uid
                    else MessageFromDialog <$> uid
    return $ Message {..}

data Snapshot = Snapshot [Message] deriving (Generic)

instance Binary MessageAddr
instance Binary Message
instance Binary Snapshot
