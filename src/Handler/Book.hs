{-# LANGUAGE TemplateHaskell #-}

module Handler.Book (getBookR) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Settings (widgetFile)

import Database.Persist ( Entity(Entity) )

import Foundation
    ( Handler
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage (MsgBook, MsgPhoto, MsgLogout)
    )


getBookR :: Handler Html
getBookR = do
    muid <- maybeAuth
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgBook
        $(widgetFile "book/book")
