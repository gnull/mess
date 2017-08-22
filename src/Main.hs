{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Arrow ((***))
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.List (groupBy)
import Data.Text (Text, pack)
import qualified Data.Text.IO
import Data.UnixTime (fromEpochTime, UnixTime)
import Foreign.C.Types (CTime(CTime))

import Web.VKHS (runVK, defaultOptions, apiSimple, API, MonadAPI)
import Web.VKHS.API.Types (Sized(..))

type MessageId    = Int
type UserId       = Int
type ChatId       = Int

-- | Id of chat or dialog this message belongs to
data MessageAddr = MessageToChat ChatId
                 | MessageFromChat UserId ChatId -- This one contains source user id
                 | MessageToDialog UserId
                 | MessageFromDialog UserId deriving (Show)

addrEq :: MessageAddr -> MessageAddr -> Bool
addrEq a b = isChat a == isChat b && whateverId a == whateverId b
  where
    isChat (MessageToChat _)     = True
    isChat (MessageFromChat _ _) = True
    isChat _ = False
    whateverId (MessageToChat a)     = a
    whateverId (MessageFromChat _ a) = a
    whateverId (MessageToDialog a)   = a
    whateverId (MessageFromDialog a) = a

data Message = Message {
                 mId   :: MessageId
               , mBody :: Text
               , mDate :: UnixTime
               , mRead :: Bool
               , mAddr :: MessageAddr
               } deriving (Show)

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

getMessagesR :: (MonadAPI m x s) => Bool -> MessageId -> Int -> API m x (Sized [Message])
getMessagesR out from count = apiSimple
  "messages.get"
  [ ("count", pack $ show count)
  , ("offset", pack $ show from)
  , ("out", if out then "1" else "0")
  ]

main :: IO ()
main = do
  x <- runVK defaultOptions $ getMessagesR True 0 20
  case x of
    (Left e) ->  putStrLn $ show e
    (Right a) -> putStrLn $ show
      $ groupBy (curry $ uncurry addrEq . (mAddr *** mAddr))
      $ m_items (a :: Sized [Message])
