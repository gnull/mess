{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module App.Fetch (fetch) where
    
import Control.Arrow ((&&&))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse, group, sort, genericLength)
import Data.Maybe (mapMaybe)
import Data.Text (pack, unpack, concat, Text)

import Data.Aeson (FromJSON)
import Web.VKHS
  ( apiSimple
  , api1
  , API
  , MonadAPI
  , UserRecord
  , MethodName
  , getCurrentUser
  , uid_id
  )
import Web.VKHS.API.Types (Sized(..), UserRecord(..))

import Data.VkMess
  ( Message(..)
  , Snapshot(..)
  , UserId
  , ChatId
  , ChatRecord(..)
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

-- users.get and messages.getChat allow up to 1000 ids to be passed in a single
-- request. But we make sure we do not pass more than 200, because at 1000 the
-- vk.com is giving error on request URL being too long. Maybe that's a
-- limitation of HTTP protocol or particular HTTP software at vk.com servers.

getUsers :: MonadAPI m x s => [UserId] -> API m x [UserRecord]
getUsers [] = return []
getUsers us | length us <= 200 =
    let ids = Data.Text.concat $ map pack $ intersperse "," $ map show us
    in  apiSimple "users.get" [("user_ids", ids)]
            | otherwise         =
    let (l, r) = splitAt 200 us
    in  do { l' <- getUsers l; r' <- getUsers r; return $ l' ++ r'}

getChats :: MonadAPI m x s => [ChatId] -> API m x [ChatRecord]
getChats [] = return []
getChats us | length us <= 200 =
    let ids = Data.Text.concat $ map pack $ intersperse "," $ map show us
    in  apiSimple "messages.getChat" [("chat_ids", ids)]
            | otherwise         =
    let (l, r) = splitAt 200 us
    in  do { l' <- getChats l; r' <- getChats r; return $ l' ++ r'}

getNames :: MonadAPI m x s => [UserId] -> API m x [(UserId, String)]
getNames us = map (fromInteger . (uid_id . ur_uid) &&& extractName) <$> getUsers us where
  extractName u = unpack (ur_first_name u) ++ " " ++ unpack (ur_last_name u)

extractChat :: Conversation -> Maybe ChatId
extractChat (ConvChat x _) = Just x
extractChat _ = Nothing

getAllAddressees :: Message -> [UserId]
getAllAddressees m = mUser m : concatMap getAllAddressees (mFwd m)

-- This nub is like the usual, but works in O(n log n) instead of O(n²)
nub' :: Ord a => [a] -> [a]
nub' = map head . group . sort

fetch :: MonadAPI m x s
      => (Integer -> IO a) -- Initialize progress bar of given width
      -> (a -> IO ())              -- «Tick» — move progress bar by one width unit
      -> API m x Snapshot
fetch initPB tick = do
  ds <- getAllConversations
  pb <- liftIO $ initPB $ genericLength ds
  ms <- forM ds $ \d -> do
    ms <- getWholeDialog $ convExtId d
    liftIO $ tick pb
    pure (d, ms)
  let cIds = mapMaybe (extractChat . fst) ms
  chats <- map (fromInteger . cId &&& id) <$> getChats cIds
  self <- fromInteger <$> (uid_id . ur_uid) <$> getCurrentUser
  let ids = nub'  $ self
                  : (concatMap getAllAddressees $ Prelude.concat $ map snd ms)
                 ++ (chats >>= \(_, x) -> cAdmin x : cUsers x)
  names <- getNames ids
  pure $ Snapshot { sDialogs = ms, sSelf = self, sUsers = names, sChats = chats }
