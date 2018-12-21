{-# OPTIONS_GHC -Wall -fno-warn-orphans  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.VkMess
  ( Message(..)
  , Snapshot(..)
  , UserId
  , ChatId
  , ChatRecord(..)
  , readFile, writeFile
  , vkImageSizes, Attachment(..)
  , DialogStats(..), getDialogStats
  , Conversation(..), Conversations(..)
  , convTitle, convExtId
  ) where

import Prelude hiding (readFile, writeFile)

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject, withArray, Value)
import Data.Aeson.Types (Parser, explicitParseFieldMaybe)
import qualified Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Maybe (fromMaybe, fromJust, catMaybes)
import Data.Bool (bool)
import Data.Monoid (Sum(..))
import Data.Foldable (toList)
import Data.Set (Set, singleton)
import Control.Monad (forM, liftM)
import Data.Text (Text, pack)
import Data.UnixTime (fromEpochTime, UnixTime)
import Foreign.C.Types (CTime(CTime))

import Data.Binary
import GHC.Generics (Generic)

type UserId       = Int
type ChatId       = Int
type GroupId      = Int
type EmailId      = Int

-- TODO Add a field email address here
data Conversation = ConvUser UserId String FilePath  -- user + name + photo
                  | ConvChat ChatId String           -- chat + title
                  | ConvGroup GroupId String         -- group + title
                  | ConvEmail EmailId                -- email
  deriving (Show, Generic)

convTitle :: Conversation -> String
convTitle (ConvUser _ n _) = n
convTitle (ConvChat _ n) = n
convTitle (ConvGroup _ n) = n
convTitle (ConvEmail i) = "Email #" ++ show i -- TODO: Fix this!

convExtId :: Conversation -> Int
convExtId (ConvUser i _ _) = i
convExtId (ConvChat i _) = i - 2000000000
convExtId (ConvGroup i _) = -i
convExtId (ConvEmail i) = -(i + 2000000000)

newtype Conversations = Conversations { getConversations :: [Conversation] }

instance FromJSON Conversations where
  parseJSON = withObject "conversations" $ \v -> do
    profs <- fromMaybe [] <$> explicitParseFieldMaybe getProfiles v "profiles"
    groups <- fromMaybe [] <$> explicitParseFieldMaybe getGroups v "groups"
    items <- v .: "items"
    liftM Conversations $ forM items $ \i -> do
      c <- i .: "conversation"
      p <- c .: "peer"
      t <- p .: "type"
      case t :: String of
        "chat" -> do
          cs <- c .: "chat_settings"
          ConvChat <$> p .: "local_id" <*> cs .: "title"
        "user" -> do
          i' <- p .: "local_id"
          return $ uncurry (ConvUser i') <$> fromJust $ lookup i' profs
        "group" -> do
          i' <- p .: "local_id"
          return $ ConvGroup i' $ fromJust $ lookup i' groups
        "email" -> ConvEmail <$> p .: "local_id"
        _ -> fail "Unexpected conversation type"
    where
      getProfiles :: Value -> Parser [(UserId, (String, FilePath))]
      getProfiles = withArray "Profiles List" $ \v -> do
        forM (toList v) $ withObject "Profile" $ \i -> do
          i' <- i .: "id"
          f <- i .: "first_name"
          l <- i .: "last_name"
          p <- i .: "photo_100"
          return (i', (f ++ l, p))
      getGroups :: Value -> Parser [(GroupId, String)]
      getGroups = withArray "Groups List" $ \v -> do
        forM (toList v) $ withObject "Group"
                        $ \i -> (,) <$> (i .: "id") <*> (i .: "name")

data ChatRecord = ChatRecord
  { cId :: Integer
  , cTitle :: Text
  , cAdmin :: UserId
  , cUsers :: [UserId]
  } deriving (Generic, Show)

instance FromJSON ChatRecord where
  parseJSON = withObject "chat" $ \v -> do
    cId    <- v .: "id"
    cTitle <- v .: "title"
    cAdmin <- v .: "admin_id"
    cUsers <- v .: "users"
    return $ ChatRecord {..}

data Attachment = Photo [(Int, FilePath)]
                | Sticker FilePath
                | Link FilePath Text Text -- url title description
                | AudioMsg FilePath
                | Other ByteString deriving (Generic, Show)

vkImageSizes :: [Int]
vkImageSizes = [2560, 1280, 807, 604, 130, 75]

instance FromJSON Attachment where
  parseJSON = withObject "attachment" $ \v -> do
    t <- v .: "type"
    let other = pure $ Other $ Data.Aeson.encode v
    case t :: Text of
      "photo" -> do
        v' <- v .: "photo"
        x <- forM vkImageSizes $ \s -> do
          url <- v' .:? ("photo_" `mappend` pack (show s))
          pure $ (,) s <$> url
        pure $ Photo $ catMaybes x
      "sticker" -> do
        v' <- v .: "sticker"
        l <- forM [256, 128, 64, 512 :: Int] $ \s -> v' .:? ("photo_" `mappend` pack (show s))
        return $ Sticker $ head $ catMaybes l
      "link" -> do
        v' <- v .: "link"
        Link <$> v' .: "url" <*> v' .: "title" <*> (fromMaybe "" <$> v' .:? "description")
      "doc" -> do
          v' <- v .: "doc"
          p <- v' .:? "preview"
          case p of
              Nothing -> other
              Just p' -> do
                  a <- p' .:? "audio_msg"
                  case a of
                      Nothing -> other
                      Just a' -> AudioMsg <$> a' .: "link_mp3"
      _ -> other

data Message = Message {
                 mBody :: Text
               , mDate :: UnixTime
               , mOut  :: Bool
               , mUser :: UserId
               , mFwd  :: [Message]
               , mAtt  :: [Attachment]
               , mJson :: ByteString
               } deriving (Generic, Show)

instance FromJSON UnixTime where
  parseJSON = fmap (fromEpochTime . CTime) . parseJSON

instance FromJSON Message where
  parseJSON = withObject "message" $ \v -> do
    mBody <- v .: "body"
    mDate <- v .: "date"
    mOut <- (/= (0 :: Int)) <$> fromMaybe 0 <$> v .:? "out"
    mUser <- v .: "user_id"
    mFwd <- fromMaybe [] <$> v .:? "fwd_messages"
    mAtt <- fromMaybe [] <$> v .:? "attachments"
    let mJson = Data.Aeson.encode v
    return $ Message {..}

data Snapshot = Snapshot { sDialogs :: [(Conversation, [Message])]
                         , sSelf :: UserId
                         , sUsers :: [(UserId, String)]
                         , sChats :: [(ChatId, ChatRecord)]
                         } deriving (Generic, Show)

instance Binary Conversation
instance Binary Attachment
instance Binary Message
instance Binary ChatRecord
instance Binary Snapshot

data DialogStats = DialogStats
                  { attachmentCount :: Int
                  , sentCount :: Int
                  , totalCount :: Int
                  , usersSeen :: Set UserId
                  }

instance Monoid DialogStats where
    DialogStats a b c d `mappend` DialogStats a' b' c' d' =
                                  DialogStats (getSum $ Sum a `mappend` Sum a')
                                              (getSum $ Sum b `mappend` Sum b')
                                              (getSum $ Sum c `mappend` Sum c')
                                              (d `mappend` d')
    mempty = DialogStats (getSum mempty)
                         (getSum mempty)
                         (getSum mempty)
                         mempty

getDialogStats :: Conversation -> [Message] -> DialogStats
getDialogStats conv = foldMap f
  where
    f :: Message -> DialogStats
    f = do
      attachmentCount <- length <$> mAtt
      sentCount <- bool 0 1 <$> mOut
      let totalCount = 1
      usersSeen <- singleton <$> mUser
      sub <- getDialogStats conv <$> mFwd
      return $ mappend sub $ DialogStats {..}
