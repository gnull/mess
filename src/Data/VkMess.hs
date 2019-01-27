{-# OPTIONS_GHC -fno-warn-orphans  #-}
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
  , getSnapshotUrls
  , replaceSnapshotUrls
  , listToWriter
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

import Data.Map as M (fromList, lookup)

import Control.Monad.Writer (Writer, tell)

-- For deriving Monod instances
import Generics.Deriving.Monoid (memptydefault, mappenddefault)

import Data.Foldable (toList)
import Data.Set (Set, singleton, fromList)
import Data.List (sort, maximumBy, deleteBy)
import Data.Ord (comparing)
import Data.Function (on)
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
  deriving (Show, Eq, Generic)

convTitle :: Conversation -> String
convTitle (ConvUser _ n _) = n
convTitle (ConvChat _ n) = n
convTitle (ConvGroup _ n) = n
convTitle (ConvEmail i) = "Email #" ++ show i -- TODO: Fix this!

convExtId :: Conversation -> Int
convExtId (ConvUser i _ _) = i
convExtId (ConvChat i _) = i + 2000000000
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
          return $ uncurry (ConvUser i') <$> fromJust $ Prelude.lookup i' profs
        "group" -> do
          i' <- p .: "local_id"
          return $ ConvGroup i' $ fromJust $ Prelude.lookup i' groups
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
          return (i', (f ++ " " ++ l, p))
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
               , mUser :: UserId -- Beware! This field is incorrect for sent messages! TODO: Fix this.
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
                         -- ^- Messages go in the original vk order — the most
                         -- recent ones first
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
                  { attachmentCount :: Sum Int
                  , sentCount :: Sum Int
                  , totalCount :: Sum Int
                  , usersSeen :: Set UserId
                  } deriving (Generic)

instance Monoid DialogStats where
  mappend = mappenddefault
  mempty = memptydefault

getDialogStats :: [Message] -> DialogStats
getDialogStats = foldMap f
  where
    f :: Message -> DialogStats
    f = do
      attachmentCount <- Sum <$> length <$> mAtt
      sentCount <- Sum <$> bool 0 1 <$> mOut
      let totalCount = Sum 1
      usersSeen <- singleton <$> mUser
      sub <- getDialogStats <$> mFwd
      return $ mappend sub $ DialogStats {..}

getSnapshotUrls :: Snapshot -> [FilePath]
getSnapshotUrls (Snapshot {..})
  = toList $ Data.Set.fromList $ flip concatMap sDialogs
  $ \(d, ms) -> convUrl d ++ concatMap messageUrls ms
  where
    convUrl (ConvUser _ _ p) = [p]
    convUrl _ = []
    messageUrls m = (concatMap attachmentUrls $ mAtt m)
                 ++ (concatMap messageUrls $ mFwd m)
    attachmentUrls (Photo xs) = [snd $ head $ reverse $ sort $ xs]
    attachmentUrls (Sticker x) = [x]
    attachmentUrls (Link _ _ _) = []
    attachmentUrls (AudioMsg x) = [x]
    attachmentUrls (Other _) = []

listToWriter :: [(FilePath, FilePath)] -> FilePath -> Writer [FilePath] FilePath
listToWriter xs k = case M.lookup k m of
    Just x -> pure x
    Nothing -> tell [k] >> pure k
  where
    m = M.fromList xs

replaceSnapshotUrls :: (FilePath -> Writer [FilePath] FilePath) -> Snapshot -> Writer [FilePath] Snapshot
replaceSnapshotUrls m ss = do
    sDialogs' <- forM (sDialogs ss) $ \(conv, ms) -> do
      conv' <- replaceConv conv
      ms' <- mapM replaceMessage ms
      pure (conv', ms')
    pure $ ss { sDialogs = sDialogs' }
  where
    replaceConv (ConvUser i n p) = ConvUser i n <$> m p
    replaceConv x = pure x
    replaceMessage ms = do
      fwd <- mapM replaceMessage $ mFwd ms
      att <- mapM replaceAtt $ mAtt ms
      pure $ ms { mFwd = fwd, mAtt = att }
    replaceAtt (Photo x) = do
      let (i, v) = maximumBy (comparing fst) x
      v' <- m v
      pure $ Photo $ (i, v') : deleteBy ((==) `on` fst) (i, undefined) x
    replaceAtt (Sticker x) = Sticker <$> m x
    replaceAtt (AudioMsg x) = AudioMsg <$> m x
    replaceAtt x = pure x
