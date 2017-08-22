{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.List (groupBy, sortOn)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO
import Data.UnixTime (fromEpochTime, UnixTime)
import Foreign.C.Types (CTime(CTime))

import Web.VKHS (runVK, defaultOptions, apiSimple, API, MonadAPI, GenericOptions(..))
import Web.VKHS.API.Types (Sized(..))

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
               }

instance Show Message where
  show (Message {..}) = show mAddr ++ ": " ++ unpack mBody

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

getMessagesR :: (MonadAPI m x s) => Bool -> Int -> Int -> API m x (Sized [Message])
getMessagesR out from count = apiSimple
  "messages.get"
  [ ("count", pack $ show count)
  , ("offset", pack $ show from)
  , ("out", if out then "1" else "0")
  ]

getAllMessagesFrom :: (MonadAPI m x s) => Bool -> Int -> API m x [Message]
getAllMessagesFrom out from = do
  mss <- m_items <$> getMessagesR out from 200
  if length mss < 200
      then return mss
      else do
        mss' <- getAllMessagesFrom out (from + 200)
        return $ mss ++ mss'

getAllMessages :: (MonadAPI m x s) => Bool -> API m x [Message]
getAllMessages = flip getAllMessagesFrom 0

main :: IO ()
main = do
  x <- runVK defaultOptions
    $ liftA2 (++) (getAllMessages False) (getAllMessages True)
  case x of
    (Left e) ->  putStrLn $ show e
    (Right a) -> putStrLn
      $ unlines $ map show $ concat
      $ groupBy (curry $ uncurry addrEq . (mAddr *** mAddr))
      $ sortOn mAddr a
