{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Menu (menu) where

import Data.Text (pack)
import Yesod.Core.Handler (getCurrentRoute)
import Foundation
    ( Widget
    , ResourcesR (DocsR)
    , Route
      ( ResourcesR, AdminR, ContactR, AboutUsR, AppointmentsR
      , BookOffersR, RequestsR, ServicesR, HomeR, StaticR
      )
    , AdminR
      ( BrandR, UsersR, AdmContactsR, AdmAboutR, AdmStaffR, AdmServicesR
      )
    , AppMessage
      ( MsgSourceCode, MsgDocumentation, MsgBrand, MsgContactUs
      , MsgAboutUs, MsgMyAppointments, MsgServices, MsgBookAppointment
      , MsgWelcome, MsgSalon, MsgUsers, MsgContact, MsgStaff, MsgData
      , MsgResources, MsgRequests, MsgCalendar
      )
    )

import Model (BookStatus (BookStatusRequest), Services (Services))

import Settings (widgetFile)
import Settings.StaticFiles (img_salon_svg)

menu :: Widget
menu = do
    curr <- getCurrentRoute
    $(widgetFile "menu")
