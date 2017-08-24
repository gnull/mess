{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (writeFile)

import Control.Applicative (liftA2)
import Control.Arrow ((***), (&&&))
import Data.Binary (encode, decode)
import Data.ByteString.Lazy (writeFile)
import Data.List (groupBy, sortOn, intersperse, nub)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack, concat)
import qualified Data.Text.IO

import Options.Applicative
import Data.Semigroup((<>))

import Web.VKHS (runVK, defaultOptions, apiSimple, API, MonadAPI, GenericOptions(..), UserRecord, getCurrentUser)
import Web.VKHS.API.Types (Sized(..), UserRecord(..))

import Data.VkMess (Message(..), Snapshot(..), UserId, MessageAddr(..))

myOptions = defaultOptions {o_max_request_rate_per_sec = 1}

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

getNames :: MonadAPI m x s => [UserId] -> API m x [(UserId, String)]
getNames us = map (fromInteger . ur_id &&& extractName) <$> getUsers us where
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

getAllAddressees :: [Message] -> [UserId]
getAllAddressees = nub . catMaybes . map f where
  f m = case mAddr m of
    (MessageToChat     _) -> Nothing
    (MessageFromChat x _) -> Just x
    (MessageToDialog   x) -> Just x
    (MessageFromDialog x) -> Just x

main :: IO ()
main = do
  outFile <- optparser
  x <- runVK myOptions
    $ liftA2 (++) (getAllMessages False) (getAllMessages True)
  case x of
    (Left e)  -> putStrLn $ show e
    (Right a) -> do
      (Right (self, names)) <- runVK myOptions $ do
        self <- fromInteger <$> ur_id <$> getCurrentUser
        let ids = nub $ self : getAllAddressees a
        names <- getNames ids
        return (self, names)
      writeFile outFile $ encode $ Snapshot a self names
