{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Data.Text (Text)
import qualified Data.Text.IO
import Data.UnixTime (fromEpochTime, UnixTime)
import Foreign.C.Types (CTime(CTime))

import Web.VKHS (runVK, defaultOptions, apiSimple)
import Web.VKHS.API.Types (Sized)

type MessageId   = Int

data Message = Message {
                 id   :: MessageId
               , body :: Text
               , date :: UnixTime
               , read :: Bool
               , out  :: Bool
               } deriving (Show)

instance FromJSON UnixTime where
  parseJSON = fmap (fromEpochTime . CTime) . parseJSON

instance FromJSON Message where
  parseJSON (Object v) =
      Message <$> v .: "id"
              <*> v .: "body"
              <*> v .: "date"
              <*> ((/= (0 :: Int)) <$> v .: "read_state")
              <*> ((/= (0 :: Int)) <$> v .: "out")
  parseJSON _ = mzero

main :: IO ()
main = do
  x <- runVK defaultOptions $ apiSimple "messages.get" [("count", "2")]
  case x of
    (Left e) ->  putStrLn $ show e
    (Right a) -> putStrLn $ show (a :: Sized [Message])
