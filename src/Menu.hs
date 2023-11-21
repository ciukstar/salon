{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module Menu (menu) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.Time.Calendar (toGregorian, periodLastDay, periodFirstDay)
import Data.Time.Calendar.Month (pattern YearMonth)
import Data.Time.Clock (utctDay, getCurrentTime)
import Yesod.Auth (maybeAuth)
import Yesod.Core (liftHandler)
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity (Entity), entityKey)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, val, where_, isNothing_, not_
    , (^.), (==.)
    )
    
import Foundation
    ( Widget
    , ResourcesR (DocsR)
    , Route
      ( ResourcesR, AdminR, ContactR, AboutUsR, AppointmentsR
      , BookOffersR, ServicesR, HomeR, StatsR, BookingsCalendarR
      , TasksCalendarR, InvoicesR, BillingR
      )
    , StatsR (PopOffersR, WorkloadsR, StatsAovR)
    , AdminR
      ( BrandR, UsersR, AdmStaffR, AdmServicesR
      , BusinessR, BusinessHoursR, BusinessCalendarR, BusinessAboutR
      , BusinessContactR
      )
    , AppMessage
      ( MsgSourceCode, MsgDocumentation, MsgContactUs, MsgAboutUs, MsgSalon
      , MsgMyAppointments, MsgServices, MsgBookAppointment, MsgWelcome, MsgUsers
      , MsgStaff, MsgData, MsgResources, MsgRequests, MsgBusiness, MsgClose
      , MsgAnalytics, MsgWorkload, MsgCustomerRanking, MsgServiceRanking
      , MsgInvoices, MsgBilling, MsgPaymentSettings, MsgSettings
      )
    )

import Model
    ( BookStatus (BookStatusRequest, BookStatusApproved), Services (Services)
    , Business (Business), Assignees (AssigneesMe), EntityField (StaffUser)
    , Staff (Staff)
    )

import Settings (widgetFile)

menu :: Widget
menu = do
    user <- maybeAuth

    empl <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ not_ $ isNothing_ $ x ^. StaffUser
        where_ $ x ^. StaffUser ==. val (entityKey <$> user)
        return x
    
    business <- liftHandler $ runDB $ selectOne $ from $ table @Business
    today <- utctDay <$> liftIO getCurrentTime
    
    let (y,m,_) = toGregorian today
        month = YearMonth y m
        firstDay = periodFirstDay month
        lastDay = periodLastDay month
        
    curr <- getCurrentRoute
    $(widgetFile "menu")

  where
      status, assignee :: Text
      status   = "status"
      assignee = "assignee"
