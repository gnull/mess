{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Prelude hiding (writeFile)

import Data.List (sortBy)
import Data.Set (fromList)
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
  ( indexHtmlStandalone
  , convPath
  , convPathNumbered
  , chopBy
  , conversationHtmlStandalone
  , messagesWithConvHtmlStandalone
  , contentsTableHtmlStandalone
  )

data Options = Options
  { inFile :: FilePath
  , cacheDir :: Maybe FilePath
  , pageLimit :: Int
  }

sample :: Parser Options
sample = do
  inFile <- argument str $
              metavar "DUMP"
           <> help "Input file"
           <> action "file"
  cacheDir <- optional $ strOption $
              long "cache"
           <> short 'c'
           <> help "Directory containing cache produced by mess-cache"
           <> action "directory"
  pageLimit <- (fmap read) $ strOption $
              long "limit"
           <> short 'l'
           <> help "Maximum allowed number of messages on a single page (conversations longer than this will be split into multiple files)"
           <> value "8000"
           <> showDefault
           <> completeWith ["5000", "8000", "10000"]
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
  when (not $ null res) $ putStrLn $ "warning: " ++ show (length $ fromList res) ++ " urls weren't found in cache"
  writeFile "index.html" $ renderHtml $ indexHtmlStandalone users chats self ms
  writeFile "messages.html" $ renderHtml
                            $ messagesWithConvHtmlStandalone users self
                            $ sortBy (comparing $ mDate . snd)
                            $ concatMap (\(d, m) -> map ((,) d) m) ms
  forM_ ms $ \(d, m) -> do
    let ms' = chopBy pageLimit m
    case ms' of
      [m'] -> writeFile (convPath d) $ renderHtml $ conversationHtmlStandalone users self d m'
      _ -> do
        let paths = map (flip convPathNumbered d) [1..]
        let z = zip paths ms'
        forM_ z $ \(path, m') ->
          writeFile path $ renderHtml $ conversationHtmlStandalone users self d m'
        writeFile (convPath d) $ renderHtml $ contentsTableHtmlStandalone z
