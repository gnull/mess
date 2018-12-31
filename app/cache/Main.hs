{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module Main where

import Prelude hiding (readFile)

import Data.Maybe (fromJust)
import Control.Monad (forM_)
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Network.URI (uriRegName, uriPort, uriAuthority, uriPath, parseAbsoluteURI)
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Binary (decode)

import Control.Concurrent.MSem (new, with)

import Data.VkMess
  ( readFile
  , getSnapshotUrls
  )

import Options.Applicative
import Data.Semigroup((<>))

data Options = Options
  { outDir :: FilePath
  , inFile :: FilePath
  }

sample :: Parser Options
sample = do
  inFile <- argument str $
              metavar "DUMP"
          <> help "Input file with mess database"
  outDir <- argument str $
              metavar "DIR"
          <> help "Output directory for cached files"
  pure Options {..}

optparser :: IO Options
optparser = execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Cache all media files refered by a mess database obtained with mess-fetch")

getHTTP :: FilePath -> IO ByteString
getHTTP url = do
  r <- get url
  pure $ r ^. responseBody

getURIPath :: FilePath -> FilePath
getURIPath p = (uriRegName a ++ uriPort a) </> (dropWhile (== '/') $ uriPath u)
  where
    u = fromJust $ parseAbsoluteURI p
    a = fromJust $ uriAuthority u

main :: IO ()
main = do
  Options {..} <- optparser
  urls <- getSnapshotUrls <$> decode <$> readFile inFile
  let index = unlines $ flip map urls $ \u -> u ++ " " ++ getURIPath u
  Prelude.writeFile (outDir </> "index.txt") index
  forM_ urls $ \u -> do
    b <- getHTTP u
    let f = outDir </> getURIPath u
    let d = takeDirectory f
    createDirectoryIfMissing True d
    Data.ByteString.Lazy.writeFile f b
