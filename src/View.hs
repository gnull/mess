{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (readFile, putStrLn)

import Control.Monad (mapM_)
import Data.Binary (encode, decode)
import Data.ByteString.Lazy.Char8 (readFile, putStrLn)
import Data.VkMess (Message(..), Snapshot(..), addrEq, whateverId)

import Options.Applicative
import Data.Semigroup((<>))

import Text.Blaze.Html5 as H ( Html
                             , docTypeHtml, head, title
                             , img, body, p, ul, li, h4
                             , toHtml
                             , (!)
                             )
import Text.Blaze.Html5.Attributes (src, style)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

messageHtml :: Message -> Html
messageHtml (Message {..}) = do
  h4 $ toHtml $ show $ mAddr
  (p $ toHtml mBody) ! style "border: 1px solid black;"

dialogHtml :: [Message] -> Html
dialogHtml = mapM_ messageHtml

mainHtml :: Snapshot -> Html
mainHtml (Snapshot ms) = docTypeHtml $ do
  H.head $ do
    H.title "My title"
  body $ do
    dialogHtml ms

optparser :: IO FilePath
optparser = execParser opts
  where
    opts = info (inFile <**> helper)
      ( fullDesc
     <> progDesc "View messages fetched by mess-fetch")
    inFile = argument str $
              metavar "FILE"
           <> help "Input file"

-- TODO: Implement simple program to render these messages as an HTML page
--    See: https://hackage.haskell.org/package/blaze-html
--         https://jaspervdj.be/blaze/tutorial.html
main :: IO ()
main = do
  inFile <- optparser
  ms <- decode <$> readFile inFile
  putStrLn $ renderHtml $ mainHtml ms
