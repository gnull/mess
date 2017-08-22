{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.List (groupBy, sortOn, intersperse)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO

import Web.VKHS (runVK, defaultOptions, apiSimple, API, MonadAPI, GenericOptions(..))
import Web.VKHS.API.Types (Sized(..))

import Data.VkMess (Message(..), addrEq, whateverId)

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

main :: IO ()
main = do
  x <- runVK defaultOptions
    $ liftA2 (++) (getAllMessages False) (getAllMessages True)
  case x of
    (Left e) ->  putStrLn $ show e
    (Right a) -> putStrLn
      $ unlines $ intersperse "---------" $ map unlines
      $ map (map show) $ map (sortOn mDate)
      $ groupBy (curry $ uncurry addrEq . (mAddr *** mAddr))
      $ sortOn (whateverId . mAddr) a
