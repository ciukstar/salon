{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Resources (getDocsR) where

import Data.Text (pack)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getYesod, languages
    , getUrlRender, preEscapedToMarkup
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Core.Handler (getMessages, newIdent)
import Settings (widgetFile)
import Database.Persist (Entity (Entity))
import Yesod.Auth (maybeAuth, Route (LoginR))
import Foundation
    ( Handler
    , Route
      ( StaticR, AuthR, PhotoPlaceholderR, AccountPhotoR, AccountR
      , ProfileR, AdminR, HomeR, ServicesR, BookOffersR
      )
    , AdminR
      ( AdmServicesR, BusinessR, UsersR, BusinessAboutR, BusinessContactR
      , BusinessHoursR, BrandR
      )
    , AppMessage
      ( MsgDocumentation, MsgPhoto, MsgNavigationMenu, MsgLogin, MsgUserProfile
      , MsgErdDiagram, MsgBookingStateDiagram, MsgAppointmentStateDiagram
      , MsgBasicEntities, MsgBusiness, MsgUser, MsgPaymentGateway, MsgOnlineMaps
      , MsgAppName, MsgOverview, MsgStaff, MsgService, MsgOffer, MsgInvoice
      , MsgEmail
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004, MsgDoc005, MsgDoc0061
      , MsgDoc0062, MsgDoc0063, MsgDoc0064, MsgDoc0065, MsgDoc0066, MsgDoc0067
      , MsgDoc0068, MsgDoc007, MsgDoc008, MsgDoc009, MsgDoc010, MsgDoc011
      , MsgDoc012 , MsgDoc013, MsgDoc014, MsgDoc015, MsgDoc016, MsgDoc017
      )
    )

import Yesod.Persist.Core (runDB)
import Database.Esqueleto.Experimental (selectOne, from, table)

import Model (Services (Services), Business)

import Settings.StaticFiles
    ( img_Salon_ERD_svg, img_Booking_State_Diagram_svg
    , img_Appointment_State_Transition_svg
    )

import Menu (menu)


getDocsR :: Handler Html
getDocsR = do
    app <- getYesod
    langs <- languages
    rndr <- getUrlRender
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    topAppBar <- newIdent

    business <- runDB $ selectOne $ from $ table @Business

    defaultLayout $ do
        setTitleI MsgDocumentation
        $(widgetFile "resources/docs")
