{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module Main where

import Prelude hiding (writeFile)

import Control.Monad.IO.Class (liftIO)
import Data.Binary (encode)
import Data.Maybe (fromMaybe)

import Options.Applicative
import Data.Semigroup((<>))

import qualified System.Console.AsciiProgress as AP
  ( tick
  , Options(..)
  , newProgressBar
  , displayConsoleRegions
  )

import Data.Default (Default(..))

import Web.VKHS
  ( runVK
  , defaultOptions
  , GenericOptions(..)
  , Verbosity(..)
  )

import Data.VkMess (writeFile)
import App.Fetch (fetch)

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

main :: IO ()
main = do
  Options {..} <- optparser
  let myOptions = defaultOptions {o_max_request_rate_per_sec = 1.5} { o_verbosity = verb,
    l_username = fromMaybe "" email, l_password = fromMaybe "" pass}
  x <- AP.displayConsoleRegions $ runVK myOptions $ fetch initPB AP.tick
  case x of
    (Left e) -> putStrLn $ show e
    (Right s) -> writeFile outFile $ encode s
  where
    initPB n = liftIO $ AP.newProgressBar
           $ def { AP.pgTotal = n
                 , AP.pgFormat = "Fetching conversations :percent [:bar] :current/:total (for :elapsed, :eta remaining)"
                 }
