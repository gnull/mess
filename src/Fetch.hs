{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (writeFile)

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.Binary (encode, decode)
import Data.ByteString.Lazy (writeFile)
import Data.List (groupBy, sortOn, intersperse)
import Data.Text (Text, pack, unpack, concat)
import qualified Data.Text.IO

import Options.Applicative
import Data.Semigroup((<>))

import Web.VKHS (runVK, defaultOptions, apiSimple, API, MonadAPI, GenericOptions(..), UserRecord, getCurrentUser)
import Web.VKHS.API.Types (Sized(..), UserRecord(..))

import Data.VkMess (Message(..), Snapshot(..), UserId)

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

-- users.get allows up to 1000 ids, make sure you do no pass more
getUsers :: MonadAPI m x s => [UserId] -> API m x [UserRecord]
getUsers [] = return []
getUsers us = apiSimple "users.get" [("user_ids", ids)] where
  ids = Data.Text.concat $ map pack $ intersperse "," $ map show us

getNames :: MonadAPI m x s => [UserId] -> API m x [String]
getNames us = map extractName <$> getUsers us where
  extractName u = unpack (ur_first_name u) ++ " " ++ unpack (ur_last_name u)

optparser :: IO FilePath
optparser = execParser opts
  where
    opts = info (outFile <**> helper)
      ( fullDesc
     <> progDesc "Fetch all messages from a vk.com profile")
    outFile = argument str $
              metavar "FILE"
           <> help "Output file"

main :: IO ()
main = do
  outFile <- optparser
  x <- runVK defaultOptions
    $ liftA2 (++) (getAllMessages False) (getAllMessages True)
  case x of
    (Left e)  -> putStrLn $ show e
    (Right a) -> writeFile outFile $ encode $ Snapshot $ sortOn mDate a
