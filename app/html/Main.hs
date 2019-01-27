{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Prelude hiding (writeFile)

import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad (forM_, when)
import Control.Monad.Writer (runWriter)
import Control.Arrow (second)
import Data.Binary (decode)

import System.FilePath ((</>))

import Data.VkMess
  ( Snapshot(..)
  , readFile
  , writeFile
  , mDate
  , listToWriter
  , replaceSnapshotUrls
  )

import Options.Applicative
import Data.Semigroup((<>))

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Html.VkMess
  ( mainHtml
  , convPath
  , dialogHtml
  , messagesHtml
  , standalone
  )

data Options = Options
  { inFile :: FilePath
  , cacheDir :: Maybe FilePath
  }

sample :: Parser Options
sample = do
  inFile <- argument str $
              metavar "DUMP"
           <> help "Input file"
  cacheDir <- optional $ strOption $
              long "cache"
           <> short 'c'
           <> help "Directory containing cache produced by mess-cache"
  pure $ Options {..}

optparser :: IO Options
optparser = execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Render messages fetched by mess-fetch as html")

main :: IO ()
main = do
  Options {..} <- optparser
  mm <- case cacheDir of
    Just d -> listToWriter <$> map (second (d </>)) <$> map (\[k, v] -> (k, v))
          <$> map words <$> lines <$> Prelude.readFile (d </> "index.txt")
    Nothing -> pure pure
  (Snapshot ms self users chats, res) <- runWriter
    <$> replaceSnapshotUrls mm
    <$> decode <$> Data.VkMess.readFile inFile
  when (not $ null res) $ putStrLn $ "warning: " ++ show (length res) ++ " urls weren't found in cache"
  writeFile "index.html" $ renderHtml $ mainHtml users chats self ms
  writeFile "messages.html" $ renderHtml $ standalone "All messages"
                            $ messagesHtml users self
                            $ sortBy (comparing $ mDate . snd)
                            $ concatMap (\(d, m) -> map ((,) d) m) ms
  forM_ ms $ \(d, m) -> writeFile (convPath d) $ renderHtml
                      $ dialogHtml users chats self (d, m)
