{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VkMess ( Message(..)
                   , MessageAddr(..)
                   , Snapshot(..)
                   , MessageGroup(..)
                   , messageGroup
                   , isMessageTo
                   , UserId
                   , messageAuthor
                   ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.ByteString.Char8 (unpack)
import Data.Maybe (fromMaybe)
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

data Message = Message {
                 mBody :: Text
               , mDate :: UnixTime
               , mAddr :: MessageAddr
               , mFwd  :: [Message]
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
    return $ Message {..}

data Snapshot = Snapshot { sMessages :: [Message]
                         , sSelf :: UserId
                         , sUsers :: [(UserId, String)]
                         } deriving (Generic)

instance Binary MessageAddr
instance Binary Message
instance Binary Snapshot
