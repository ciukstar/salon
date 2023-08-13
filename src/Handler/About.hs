{-# LANGUAGE TemplateHaskell #-}

module Handler.About (getAboutR) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setTitleI, setUltDestCurrent)
import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)

import Settings (widgetFile)

import Database.Persist (Entity(Entity))
import Foundation
    ( Handler
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage (MsgAboutUs, MsgPhoto, MsgLogout)
    )

getAboutR :: Handler Html
getAboutR = do
    muid <- maybeAuth
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "about/about")
