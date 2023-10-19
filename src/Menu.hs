{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Menu (menu) where

import Data.Text (pack)
import Yesod.Core (liftHandler)
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity (Entity))

import Database.Esqueleto.Experimental
    ( selectOne, from, table )
    
import Foundation
    ( Widget
    , ResourcesR (DocsR)
    , Route
      ( ResourcesR, AdminR, ContactR, AboutUsR, AppointmentsR
      , BookOffersR, RequestsR, ServicesR, HomeR
      )
    , AdminR
      ( BrandR, UsersR, AdmContactsR, AdmAboutR, AdmStaffR, AdmServicesR
      , BusinessR
      )
    , AppMessage
      ( MsgSourceCode, MsgDocumentation, MsgBrand, MsgContactUs
      , MsgAboutUs, MsgMyAppointments, MsgServices, MsgBookAppointment
      , MsgWelcome, MsgSalon, MsgUsers, MsgContact, MsgStaff, MsgData
      , MsgResources, MsgRequests, MsgCalendar, MsgBusiness, MsgClose
      )
    )

import Model
    ( BookStatus (BookStatusRequest), Services (Services), Business (Business)
    , Assignees (AssigneesMe)    
    )

import Settings (widgetFile)

menu :: Widget
menu = do
    business <- liftHandler $ runDB $ selectOne $ from $ table @Business
    curr <- getCurrentRoute
    $(widgetFile "menu")
