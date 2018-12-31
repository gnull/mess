{-# LANGUAGE TemplateHaskell #-}

module Text.Html.VkMess.Static (globalCSS) where
import Data.FileEmbed (embedStringFile)

globalCSS :: String
globalCSS = $(embedStringFile "static/global.css")
