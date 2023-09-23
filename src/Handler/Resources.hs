{-# LANGUAGE TemplateHaskell #-}

module Handler.Resources (getDocsR) where

import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getYesod, languages
    , getUrlRender, preEscapedToMarkup
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Core.Handler (getMessages)
import Settings (widgetFile)
import Database.Persist (Entity (Entity))
import Yesod.Auth (maybeAuth, Route (LoginR))
import Foundation
    ( Handler
    , Route
      ( StaticR, AuthR, PhotoPlaceholderR, AccountPhotoR
      , ProfileR, AdminR
      )
    , AdminR (AdmServicesR)
    , AppMessage
      ( MsgDocumentation, MsgErdDiagram, MsgBookingStateDiagram, MsgPhoto
      , MsgAppName, MsgOverview, MsgDoc001, MsgDoc002
      )
    )

import Model (Services (Services))
    
import Settings.StaticFiles (img_Salon_ERD_svg, img_Booking_State_Diagram_svg)

import Menu (menu)


getDocsR :: Handler Html
getDocsR = do
    app <- getYesod
    langs <- languages
    rndr <- getUrlRender
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgDocumentation
        $(widgetFile "resources/docs")
