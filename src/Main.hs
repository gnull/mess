{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Aeson (FromJSON(..), Value(..), (.:), withObject)
import Data.Text (Text, pack)
import qualified Data.Text.IO
import Data.UnixTime (fromEpochTime, UnixTime)
import Foreign.C.Types (CTime(CTime))

import Web.VKHS (runVK, defaultOptions, apiSimple, API, MonadAPI)
import Web.VKHS.API.Types (Sized(..))

type MessageId   = Int

data Message = Message {
                 mId   :: MessageId
               , mBody :: Text
               , mDate :: UnixTime
               , mRead :: Bool
               , mOut  :: Bool
               } deriving (Show)

instance FromJSON UnixTime where
  parseJSON = fmap (fromEpochTime . CTime) . parseJSON

instance FromJSON Message where
  parseJSON = withObject "message" $ \v -> do
    mId <- v .: "id"
    mBody <- v .: "body"
    mDate <- v .: "date"
    mRead <- (/= (0 :: Int)) <$> v .: "read_state"
    mOut <- (/= (0 :: Int)) <$> v .: "out"
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
  x <- runVK defaultOptions $ getMessagesR True 0 4
  case x of
    (Left e) ->  putStrLn $ show e
    (Right a) -> putStrLn $ show (a :: Sized [Message])
