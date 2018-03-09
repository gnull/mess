{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (writeFile)

import Control.Applicative (liftA2)
import Control.Arrow ((***), (&&&))
import Control.Monad (forM)
import Data.Binary (encode, decode)
import Data.List (groupBy, sortOn, intersperse, nub, sort)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack, concat)
import qualified Data.Text.IO

import Options.Applicative
import Data.Semigroup((<>))

import Web.VKHS (runVK, defaultOptions, apiSimple, API, MonadAPI, GenericOptions(..), UserRecord, getCurrentUser)
import Web.VKHS.API.Types (Sized(..), UserRecord(..))

import Data.VkMess (Message(..), Snapshot(..), UserId, MessageAddr(..), Dialog(..), MessageGroup(..), messageGroup, writeFile, readFile)

myOptions = defaultOptions {o_max_request_rate_per_sec = 1.5}

getDialogR :: (MonadAPI m x s) => Int -> Int -> Int -> API m x (Sized [Message])
getDialogR peer from count = apiSimple
  "messages.getHistory"
  [ ("count", pack $ show count)
  , ("offset", pack $ show from)
  , ("peer_id", pack $ show peer)
  ]

getDialogsR :: (MonadAPI m x s) => Int -> Int -> API m x (Sized [Message])
getDialogsR from count = do
  x <- apiSimple
    "messages.getDialogs"
    [ ("count", pack $ show count)
    , ("offset", pack $ show from)
    ]
  pure $ x {m_items = map dMess $ m_items x}

getWholeDialog :: (MonadAPI m x s) => Int -> API m x [Message]
getWholeDialog peer = f 0 where
  f from = do
    mss <- m_items <$> getDialogR peer from 200
    if length mss < 200
        then return mss
        else do
          mss' <- f (from + 200)
          return $ mss ++ mss'

getAllDialogs :: (MonadAPI m x s) => API m x [Message]
getAllDialogs = f 0 where
  f from = do
    mss <- m_items <$> getDialogsR from 200
    if length mss < 200
        then return mss
        else do
          mss' <- f (from + 200)
          return $ mss ++ mss'

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

isDialog :: MessageGroup -> Bool
isDialog (MessageDialog _) = True
isDialog _ = False

peerByMessageGroup :: MessageGroup -> Int
peerByMessageGroup (MessageChat x) = 2000000000 + x
peerByMessageGroup (MessageDialog x) = x

getAllAddressees :: [Message] -> [UserId]
getAllAddressees = nub . catMaybes . map f where
  f m = case mAddr m of
    (MessageToChat     _) -> Nothing
    (MessageFromChat x _) -> Just x
    (MessageToDialog   x) -> Just x
    (MessageFromDialog x) -> Just x

main = do
  outFile <- optparser
  x <- runVK myOptions $ do
    ds <- getAllDialogs
    ms <- forM ds $ \d -> do
      ms <- getWholeDialog $ peerByMessageGroup $ messageGroup $ mAddr d
      pure (d, ms)
    self <- fromInteger <$> ur_id <$> getCurrentUser
    let ids = nub $ sort $ self : (getAllAddressees $ Prelude.concat $ map snd ms)
    names <- getNames ids
    pure $ Snapshot { sDialogs = ms, sSelf = self, sUsers = names }
  case x of
    (Left e) -> putStrLn $ show e
    (Right s) -> writeFile outFile $ encode s
