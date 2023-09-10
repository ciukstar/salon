{-# LANGUAGE TemplateHaskell #-}

module Menu (menu) where

import Yesod.Core.Handler (getCurrentRoute)
import Foundation
    ( Widget
    , ResourcesR (DocsR)
    , Route
      ( ResourcesR, AdminR, ContactR, AboutUsR, AppointmentsR
      , V2BookOffersR, BookStartR
      , ServicesR, HomeR, StaticR
      )
    , AdminR
      ( BrandR, UsersR, AdmContactsR, AdmAboutR, AdmStaffR, AdmServicesR
      )
    , AppMessage
      ( MsgSourceCode, MsgDocumentation, MsgBrand, MsgContactUs
      , MsgAboutUs, MsgMyAppointments, MsgBook, MsgServices
      , MsgWelcome, MsgSalon, MsgUsers, MsgContact, MsgStaff, MsgData
      , MsgResources
      )
    )

import Model (Services (Services))

import Settings (widgetFile)
import Settings.StaticFiles (img_salon_svg)

menu :: Widget
menu = do
    curr <- getCurrentRoute
    $(widgetFile "menu")
