{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module Main where

import Prelude hiding (readFile)

import Data.Maybe (fromJust)
import Data.List (genericLength)
import Control.Monad (forM_, forM, replicateM)
import Control.Arrow ((&&&))
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)

import qualified System.Console.AsciiProgress as AP
  ( ProgressBar(..)
  , Options(..)
  , tick
  , newProgressBar
  , displayConsoleRegions
  )

import Data.Default (Default(..))

import Network.Wreq (responseBody)
import Network.Wreq.Session (get, Session, newAPISession)
import Control.Lens ((^.))
import Network.URI (uriRegName, uriPort, uriAuthority, uriPath, parseAbsoluteURI)
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Binary (decode)

import qualified Control.Concurrent.MSem as MSem (new, with)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_)

import Data.VkMess
  ( readFile
  , getSnapshotUrls
  )

import Options.Applicative
import Data.Semigroup((<>))

data Options = Options
  { outDir :: FilePath
  , inFile :: FilePath
  , jobs   :: Int
  }

sample :: Parser Options
sample = do
  inFile <- argument str $
              metavar "DUMP"
          <> help "Input file with mess database"
  outDir <- argument str $
              metavar "DIR"
          <> help "Output directory for cached files"
  jobs <- option auto $
             long "jobs"
          <> short 'j'
          <> help "The number of parallel download jobs"
  pure Options {..}

optparser :: IO Options
optparser = execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Cache all media files refered by a mess database obtained with mess-fetch")

getHTTP :: Session -> FilePath -> IO ByteString
getHTTP ses url = do
  r <- get ses url
  pure $ r ^. responseBody

getURIPath :: FilePath -> FilePath
getURIPath p = (uriRegName a ++ uriPort a) </> (dropWhile (== '/') $ uriPath u)
  where
    u = fromJust $ parseAbsoluteURI p
    a = fromJust $ uriAuthority u

processUrl :: AP.ProgressBar -> FilePath -> FilePath -> Session -> IO ()
processUrl pb wd u ses = do
  b <- getHTTP ses u
  let f = wd </> getURIPath u
  let d = takeDirectory f
  createDirectoryIfMissing True d
  Data.ByteString.Lazy.writeFile f b
  AP.tick pb

pushMVar :: a -> MVar [a] -> IO ()
pushMVar x = flip modifyMVar_ $ pure . (x:)

popMVar :: MVar [a] -> IO a
popMVar = flip modifyMVar $ pure . (tail &&& head)

withMVar :: MVar [a] -> (a -> IO b) -> IO b
withMVar m a = do
  v <- popMVar m
  res <- a v
  pushMVar v m
  pure res

main :: IO ()
main = do
  Options {..} <- optparser
  urls <- getSnapshotUrls <$> decode <$> readFile inFile
  let index = unlines $ flip map urls $ \u -> u ++ " " ++ getURIPath u
  Prelude.writeFile (outDir </> "index.txt") index
  sem <- MSem.new jobs
  ses <- newMVar =<< replicateM jobs newAPISession
  AP.displayConsoleRegions $ do
    pb <- AP.newProgressBar $ def { AP.pgTotal = genericLength urls }
    as <- forM urls $ async . MSem.with sem . withMVar ses . processUrl pb outDir
    forM_ as wait
