{-# LANGUAGE TemplateHaskell #-}

module Handler.Resources (getDocsR) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Core.Handler (getMessages)
import Settings (widgetFile)
import Database.Persist (Entity (Entity))
import Yesod.Auth (maybeAuth, Route (LoginR))
import Foundation
    ( Handler
    , Route
      ( StaticR, AuthR, PhotoPlaceholderR, AccountPhotoR
      , ProfileR
      )
    , AppMessage
      ( MsgDocumentation, MsgErdDiagram, MsgPhoto
      , MsgAppName, MsgDoc001, MsgBookingStateDiagram
      )
    )
    
import Settings.StaticFiles (img_Salon_ERD_svg, img_Booking_State_Diagram_svg)

import Menu (menu)


getDocsR :: Handler Html
getDocsR = do
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDocumentation
        $(widgetFile "resources/docs")
