{-# LANGUAGE TemplateHaskell #-}

module Handler.Services (getServicesR) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setTitleI, setUltDestCurrent, getMessages)
import Yesod.Auth (Route (LoginR))

import Foundation
    ( Handler
    , Route (AuthR)
    , AppMessage
      ( MsgServices, MsgAdd
      )
    )

import Settings (widgetFile)

getServicesR :: Handler Html
getServicesR = do
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgServices
        $(widgetFile "services/services")
