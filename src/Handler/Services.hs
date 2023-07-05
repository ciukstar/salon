{-# LANGUAGE TemplateHaskell #-}

module Handler.Services (getServicesR) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setTitleI)

import Foundation
    ( Handler
    , AppMessage
      ( MsgServices, MsgAdd
      )
    )

import Settings (widgetFile)

getServicesR :: Handler Html
getServicesR = defaultLayout $ do
    setTitleI MsgServices
    $(widgetFile "services/services")
