{-# OPTIONS_GHC -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module Main where

import Prelude hiding (writeFile)

import Control.Arrow ((&&&))
import Control.Monad (forM)
import Data.Binary (encode)
import Data.List (intersperse, group, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (pack, unpack, concat)

import Options.Applicative
import Data.Semigroup((<>))

import Web.VKHS
  ( runVK
  , defaultOptions
  , apiSimple
  , API
  , MonadAPI
  , GenericOptions(..)
  , UserRecord
  , getCurrentUser
  )
import Web.VKHS.API.Types (Sized(..), UserRecord(..))

import Data.VkMess
  ( Message(..)
  , Snapshot(..)
  , UserId
  , ChatId
  , ChatRecord(..)
  , MessageAddr(..)
  , Dialog(..)
  , MessageGroup(..)
  , messageGroup
  , writeFile
  )

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
getUsers us | length us <= 1000 =
    let ids = Data.Text.concat $ map pack $ intersperse "," $ map show us
    in  apiSimple "users.get" [("user_ids", ids)]
            | otherwise         =
    let (l, r) = splitAt 1000 us
    in  do { l' <- getUsers l; r' <- getUsers r; return $ l' ++ r'}

getChats :: MonadAPI m x s => [ChatId] -> API m x [ChatRecord]
getChats [] = return []
getChats us | length us <= 1000 =
    let ids = Data.Text.concat $ map pack $ intersperse "," $ map show us
    in  apiSimple "messages.getChat" [("chat_ids", ids)]
            | otherwise         =
    let (l, r) = splitAt 1000 us
    in  do { l' <- getChats l; r' <- getChats r; return $ l' ++ r'}

getNames :: MonadAPI m x s => [UserId] -> API m x [(UserId, String)]
getNames us = map (fromInteger . ur_id &&& extractName) <$> getUsers us where
  extractName u = unpack (ur_first_name u) ++ " " ++ unpack (ur_last_name u)

data Options = Options
  { outFile  :: FilePath
  , email    :: Maybe String
  , pass     :: Maybe String
  , verb     :: Bool
  }

sample :: Parser Options
sample = do
  outFile <- argument str $
              metavar "FILE"
          <> help "Output file"
  verb <- switch $
             long "verbose"
          <> short 'v'
          <> help "Enable verbose output"
  email <- optional $ strOption $
             long "login"
          <> short 'l'
          <> help "User login"
  pass <- optional $ strOption $
             long "pass"
          <> short 'p'
          <> help "User password"
  pure Options {..}

optparser :: IO Options
optparser = execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Fetch all messages from a vk.com profile")

isDialog :: MessageGroup -> Bool
isDialog (MessageDialog _) = True
isDialog _ = False

extractChat :: MessageGroup -> Maybe ChatId
extractChat (MessageChat x) = Just x
extractChat _ = Nothing

peerByMessageGroup :: MessageGroup -> Int
peerByMessageGroup (MessageChat x) = 2000000000 + x
peerByMessageGroup (MessageDialog x) = x

getAllAddressees :: [Message] -> [UserId]
getAllAddressees = concatMap f where
  f m  = getAllAddressees (mFwd m)
      ++ case mAddr m of
           (MessageToChat     _) -> []
           (MessageFromChat x _) -> [x]
           (MessageToDialog   x) -> [x]
           (MessageFromDialog x) -> [x]

-- This nub is like the usual, but works in O(n log n) instead of O(n²)
nub' :: Ord a => [a] -> [a]
nub' = map head . group . sort

main :: IO ()
main = do
  Options {..} <- optparser
  let myOptions = defaultOptions {o_max_request_rate_per_sec = 1.5} { o_verbose = verb,
    l_username = fromMaybe "" email, l_password = fromMaybe "" pass}
  x <- runVK myOptions $ do
    ds <- getAllDialogs
    ms <- forM ds $ \d -> do
      ms <- getWholeDialog $ peerByMessageGroup $ messageGroup $ mAddr d
      pure (d, ms)
    let cIds = mapMaybe (extractChat . messageGroup . mAddr . fst) ms
    chats <- map (fromInteger . cId &&& id) <$> getChats cIds
    self <- fromInteger <$> ur_id <$> getCurrentUser
    let ids = nub'  $ self
                    : (getAllAddressees $ Prelude.concat $ map snd ms)
                   ++ (chats >>= \(_, x) -> cAdmin x : cUsers x)
    names <- getNames ids
    pure $ Snapshot { sDialogs = ms, sSelf = self, sUsers = names, sChats = chats }
  case x of
    (Left e) -> putStrLn $ show e
    (Right s) -> writeFile outFile $ encode s
