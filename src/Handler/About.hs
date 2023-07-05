{-# LANGUAGE TemplateHaskell #-}

module Handler.About (getAboutR) where

import Foundation (Handler, AppMessage (MsgAboutUs))
import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setTitleI)

import Settings (widgetFile)


getAboutR :: Handler Html
getAboutR = defaultLayout $ do
    setTitleI MsgAboutUs
    $(widgetFile "about/about")
