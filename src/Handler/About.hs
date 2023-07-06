{-# LANGUAGE TemplateHaskell #-}

module Handler.About (getAboutR) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setTitleI, setUltDestCurrent)
import Yesod.Auth (Route (LoginR))

import Settings (widgetFile)

import Foundation
    ( Handler
    , Route (AuthR)
    , AppMessage (MsgAboutUs, MsgAdd)
    )

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
    setUltDestCurrent
    setTitleI MsgAboutUs
    $(widgetFile "about/about")
