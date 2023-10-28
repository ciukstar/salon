{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Menu (menu) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Data.Time.Clock (utctDay, getCurrentTime)
import Yesod.Core (liftHandler)
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity (Entity), entityKey)

import Database.Esqueleto.Experimental
    ( selectOne, from, table )
    
import Foundation
    ( Widget
    , ResourcesR (DocsR)
    , Route
      ( ResourcesR, AdminR, ContactR, AboutUsR, AppointmentsR
      , BookOffersR, RequestsR, ServicesR, HomeR, StatsR
      )
    , StatsR (PopOffersR, WorkloadsR)
    , AdminR
      ( BrandR, UsersR, AdmContactsR, AdmAboutR, AdmStaffR, AdmServicesR
      , BusinessR, BusinessHoursR, BusinessCalendarR
      )
    , AppMessage
      ( MsgSourceCode, MsgDocumentation, MsgBrand, MsgContactUs, MsgAboutUs
      , MsgMyAppointments, MsgServices, MsgBookAppointment, MsgWelcome, MsgSalon
      , MsgUsers, MsgContact, MsgStaff, MsgData, MsgResources, MsgRequests
      , MsgCalendar, MsgBusiness, MsgClose, MsgAnalytics, MsgPopularOffers
      , MsgWorkload
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
    today <- utctDay <$> liftIO getCurrentTime
    curr <- getCurrentRoute
    $(widgetFile "menu")
