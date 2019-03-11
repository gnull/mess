{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ApplicativeDo              #-}

module Main where

import Prelude hiding (writeFile)

import Control.Monad.IO.Class (liftIO)
import Data.Binary (encode)

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
  , AppID(..)
  , defaultOptions
  , GenericOptions(..)
  , Verbosity(..)
  )

import Data.VkMess (writeFile)
import App.Fetch (fetch)

data Options = Options
  { outFile  :: FilePath
  , creds    :: Either (String, String) String -- (email, pass) or token
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
    creds <- (Left <$> emailPass) <|> (Right <$> token)
    pure Options {..}
  where
    emailPass = do
      email <- strOption $ long "login" <> short 'l' <> help "User login"
      pass <- strOption $ long "pass" <> short 'p' <> help "User password"
      pure $ (email, pass)
    token = strOption $ long "token" <> short 't' <> help "API access token"

optparser :: IO Options
optparser = execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Fetch all messages from a vk.com profile")

main :: IO ()
main = do
  Options {..} <- optparser
  let myOptions' = defaultOptions {o_max_request_rate_per_sec = 1.5, o_verbosity = verb, l_appid = AppID "2685278" }
  let myOptions = case creds of
                    Left (email, pass) -> myOptions' {l_username = email, l_password = pass}
                    Right token -> myOptions' { l_access_token = token }
  x <- AP.displayConsoleRegions $ runVK myOptions $ fetch initPB AP.tick
  case x of
    (Left e) -> putStrLn $ show e
    (Right s) -> writeFile outFile $ encode s
  where
    initPB n = liftIO $ AP.newProgressBar
           $ def { AP.pgTotal = n
                 , AP.pgFormat = "Fetching conversations :percent [:bar] :current/:total (for :elapsed, :eta remaining)"
                 }
