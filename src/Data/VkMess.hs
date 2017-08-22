{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VkMess ( Message(..)
                   , addrEq
                   , whateverId
                   ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.ByteString.Char8 (unpack)
import Data.Text (Text, unpack)
import Data.UnixTime (fromEpochTime, UnixTime, formatUnixTimeGMT, webDateFormat)
import Foreign.C.Types (CTime(CTime))

type MessageId    = Int
type UserId       = Int
type ChatId       = Int

-- | Id of chat or dialog this message belongs to
data MessageAddr = MessageToChat ChatId
                 | MessageFromChat UserId ChatId -- This one contains source user id
                 | MessageToDialog UserId
                 | MessageFromDialog UserId deriving (Ord, Eq)

instance Show MessageAddr where
  show (MessageToChat x)     = "[c] -> " ++ show x
  show (MessageFromChat y x) = "[c] <- " ++ show x ++ " by " ++ show y
  show (MessageToDialog x)   = "[d] -> " ++ show x
  show (MessageFromDialog x) = "[d] <- " ++ show x

whateverId :: MessageAddr -> Int
whateverId (MessageToChat a)     = a
whateverId (MessageFromChat _ a) = a
whateverId (MessageToDialog a)   = a
whateverId (MessageFromDialog a) = a

addrEq :: MessageAddr -> MessageAddr -> Bool
addrEq a b = isChat a == isChat b && whateverId a == whateverId b
  where
    isChat (MessageToChat _)     = True
    isChat (MessageFromChat _ _) = True
    isChat _ = False

data Message = Message {
                 mId   :: MessageId
               , mBody :: Text
               , mDate :: UnixTime
               , mRead :: Bool
               , mAddr :: MessageAddr
               }

instance Show Message where
  show (Message {..}) = Data.ByteString.Char8.unpack (formatUnixTimeGMT webDateFormat mDate)
                     ++ ": " ++ show mAddr ++ ": " ++ unpack mBody

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
