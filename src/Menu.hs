{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module Menu (menu) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Data.Time.Calendar (toGregorian, periodLastDay, periodFirstDay)
import Data.Time.Calendar.Month (pattern YearMonth)
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
    , StatsR (PopOffersR, WorkloadsR, StatsAovR)
    , AdminR
      ( BrandR, UsersR, AdmStaffR, AdmServicesR
      , BusinessR, BusinessHoursR, BusinessCalendarR, BusinessAboutR
      , BusinessContactR
      )
    , AppMessage
      ( MsgSourceCode, MsgDocumentation, MsgBrand, MsgContactUs, MsgAboutUs
      , MsgMyAppointments, MsgServices, MsgBookAppointment, MsgWelcome, MsgSalon
      , MsgUsers, MsgStaff, MsgData, MsgResources, MsgRequests
      , MsgBusiness, MsgClose, MsgAnalytics, MsgWorkload, MsgCustomerRanking
      , MsgServiceRanking
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
    
    let (y,m,_) = toGregorian today
        month = YearMonth y m
        firstDay = periodFirstDay month
        lastDay = periodLastDay month
        
    curr <- getCurrentRoute
    $(widgetFile "menu")
