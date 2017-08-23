{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (readFile, putStrLn, writeFile)

import Control.Monad (forM_)
import Data.Binary (encode, decode)
import Data.ByteString.Lazy.Char8 (readFile, putStrLn, writeFile)
import Data.ByteString.Char8 (unpack)

import Data.Function (on)
import Data.List (nub, sortOn, groupBy)

import Data.UnixTime (UnixTime, formatUnixTimeGMT, webDateFormat)
import Data.VkMess ( Message(..)
                   , MessageAddr(..)
                   , Snapshot(..)
                   , MessageGroup(..)
                   , messageGroup
                   , isMessageTo
                   )

import Options.Applicative
import Data.Semigroup((<>))
import Text.Blaze (ToValue(..))
import Text.Blaze.Html5 as H ( Html
                             , docTypeHtml, head, title
                             , body, hr, div, p, span, a
                             , toHtml
                             , (!)
                             )
import Text.Blaze.Html5.Attributes (src, style, href)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

addrHtml :: MessageAddr -> Html
addrHtml x = H.span . toHtml $ field ++ show (messageGroup x) where
  field = if isMessageTo x then "To: " else "From: "

unixTimeHtml :: UnixTime -> Html
unixTimeHtml = H.span . toHtml . unpack . formatUnixTimeGMT webDateFormat

messageHtml :: Message -> Html
messageHtml (Message {..}) = do
  H.div ! style "border: 1px solid black; background-color: #ddd; margin: 1px; padding: 2px;" $ do
    H.div $ do
      addrHtml mAddr
      H.span ! style "display: inline-block; width: 0.5cm;" $ toHtml ("" :: String)
      unixTimeHtml mDate
    p $ toHtml mBody

dialogHtml :: [Message] -> Html
dialogHtml ms = H.body ! style "font-family: Verdana, Sans-Serif; font-size: 14.4px;" $ H.div ! style "width: 500px; word-wrap: break-word;" $ mapM_ messageHtml ms

class Urlable a where
  urlFor :: a -> String

-- Type returned by this function is not exported by Blaze, therefore this
-- function has no signature
hrefFor = href . toValue . urlFor

instance Urlable MessageGroup where
  urlFor = (++ ".html") . show

mainHtml :: [MessageGroup] -> Html
mainHtml gs = docTypeHtml $ do
  H.head $ do
    H.title "My title"
  body $ do
    forM_ gs $ \g -> H.div $ a (toHtml $ urlFor g) ! hrefFor g

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
  (Snapshot ms) <- decode <$> readFile inFile
  writeFile "index.html" $ renderHtml $ mainHtml $ nub $ map (messageGroup . mAddr) ms
  let cs = groupBy (on (==) (messageGroup . mAddr)) $ sortOn (messageGroup . mAddr) ms
  forM_ cs $ \c -> do
    let g = messageGroup $ mAddr $ Prelude.head c
    writeFile (urlFor g) $ renderHtml $ dialogHtml $ sortOn mDate c
