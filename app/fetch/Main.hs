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
import Data.Text (pack, unpack, concat, Text)

import Options.Applicative
import Data.Semigroup((<>))

import Data.Aeson (FromJSON)
import Web.VKHS
  ( runVK
  , defaultOptions
  , apiSimple
  , api1
  , API
  , MonadAPI
  , GenericOptions(..)
  , UserRecord
  , MethodName
  , getCurrentUser
  , Verbosity(..)
  , uid_id
  )
import Web.VKHS.API.Types (Sized(..), UserRecord(..))

import Data.VkMess
  ( Message(..)
  , Snapshot(..)
  , UserId
  , ChatId
  , ChatRecord(..)
  , writeFile
  , Conversation(..)
  , Conversations(..)
  , convExtId
  )

apiSimpleNew :: (MonadAPI m x s, FromJSON a) => MethodName -> [(String, Text)] -> API m x a
apiSimpleNew nm args = api1 nm (("v", "5.92"):args)

getDialogR :: (MonadAPI m x s) => Int -> Int -> Int -> API m x (Sized [Message])
getDialogR peer from count = apiSimple
  "messages.getHistory"
  [ ("count", pack $ show count)
  , ("offset", pack $ show from)
  , ("peer_id", pack $ show peer)
  ]

getConversationsR :: (MonadAPI m x s) => Int -> Int -> API m x [Conversation]
getConversationsR from count = getConversations <$> apiSimpleNew
    "messages.getConversations"
    [ ("count", pack $ show count)
    , ("offset", pack $ show from)
    , ("extended", "1")
    ]

getWholeDialog :: (MonadAPI m x s) => Int -> API m x [Message]
getWholeDialog peer = f 0 where
  f from = do
    mss <- m_items <$> getDialogR peer from 200
    if length mss < 200
        then return mss
        else do
          mss' <- f (from + 200)
          return $ mss ++ mss'

getAllConversations :: (MonadAPI m x s) => API m x [Conversation]
getAllConversations = f 0 where
  f from = do
    mss <- getConversationsR from 200
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
getNames us = map (fromInteger . (uid_id . ur_uid) &&& extractName) <$> getUsers us where
  extractName u = unpack (ur_first_name u) ++ " " ++ unpack (ur_last_name u)

data Options = Options
  { outFile  :: FilePath
  , email    :: Maybe String
  , pass     :: Maybe String
  , verb     :: Verbosity
  }

sample :: Parser Options
sample = do
  outFile <- argument str $
              metavar "FILE"
          <> help "Output file"
  verb <- (flag' Debug $
               long "debug"
            <> short 'd'
            <> help "Enable debug output")
          <|> (flag Normal Trace $
               long "verbose"
            <> short 'v'
            <> help "Enable verbose (trace) output")
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

extractChat :: Conversation -> Maybe ChatId
extractChat (ConvChat x _) = Just x
extractChat _ = Nothing

getAllAddressees :: Message -> [UserId]
getAllAddressees m = mUser m : concatMap getAllAddressees (mFwd m)

-- This nub is like the usual, but works in O(n log n) instead of O(n²)
nub' :: Ord a => [a] -> [a]
nub' = map head . group . sort

main :: IO ()
main = do
  Options {..} <- optparser
  let myOptions = defaultOptions {o_max_request_rate_per_sec = 1.5} { o_verbosity = verb,
    l_username = fromMaybe "" email, l_password = fromMaybe "" pass}
  x <- runVK myOptions $ do
    ds <- getAllConversations
    ms <- forM ds $ \d -> do
      ms <- getWholeDialog $ convExtId d
      pure (d, ms)
    let cIds = mapMaybe (extractChat . fst) ms
    chats <- map (fromInteger . cId &&& id) <$> getChats cIds
    self <- fromInteger <$> (uid_id . ur_uid) <$> getCurrentUser
    let ids = nub'  $ self
                    : (concatMap getAllAddressees $ Prelude.concat $ map snd ms)
                   ++ (chats >>= \(_, x) -> cAdmin x : cUsers x)
    names <- getNames ids
    pure $ Snapshot { sDialogs = ms, sSelf = self, sUsers = names, sChats = chats }
  case x of
    (Left e) -> putStrLn $ show e
    (Right s) -> writeFile outFile $ encode s
