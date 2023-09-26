{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Appointments
  ( getAppointmentsR
  , getAppointmentR
  ) where

import Data.Text (unpack, intercalate)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getYesod, languages
    , preEscapedToMarkup
    )
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Fields (unTextarea)
import Yesod.Persist (Entity (Entity), YesodPersist(runDB))
import Settings (widgetFile)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, innerJoin, on, where_, val
    , (:&)((:&)), (^.), (==.), (?.)
    , orderBy, desc, leftJoin, just
    )

import Foundation
    ( Handler
    , Route
      ( ProfileR, AppointmentsR, AppointmentR, BookOffersR, AuthR
      , PhotoPlaceholderR, AccountPhotoR, ServiceThumbnailR, AdminR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgMyAppointments, MsgLogin, MsgLogout, MsgPhoto
      , MsgLoginToSeeYourAppointments, MsgNoAppointmentsYet
      , MsgBookAppointment, MsgAppointment, MsgDuration
      , MsgSymbolHour, MsgSymbolMinute
      , MsgService, MsgReschedule, MsgCancel
      , MsgMeeting, MsgMeetingTime, MsgMeetingLocation
      )
    )

import Model
    ( BookId, Book(Book), Offer (Offer), Service (Service), Role (Role)
    , Staff (Staff), Thumbnail (Thumbnail), User (User)
    , EntityField
      ( BookOffer, OfferId, BookUser, BookId, OfferService, ServiceId
      , BookDay, BookTime, BookRole, RoleId, RoleStaff, StaffId, ThumbnailService
      , ContentsSection
      ), Contents (Contents)
    )

import Menu (menu)
import Handler.Contacts (section)


getAppointmentR :: BookId -> Handler Html
getAppointmentR bid = do
    app <- getYesod
    langs <- languages
    user <- maybeAuth
    location <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    book <- runDB $ selectOne $ do
        x :& o :& s :& t :& r :& e <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& _ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
            `leftJoin` table @Role `on` (\(x :& _ :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
        where_ $ x ^. BookId ==. val bid
        case user of
          Just (Entity uid _) -> where_ $ x ^. BookUser ==. val uid
          Nothing -> where_ $ val False
        orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
        return (x,o,s,t,r,e)
    defaultLayout $ do
        setTitleI MsgAppointment
        $(widgetFile "appointments/appointment")


getAppointmentsR :: Handler Html
getAppointmentsR = do
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    case user of
      Nothing -> defaultLayout $ do
          setTitleI MsgMyAppointments
          $(widgetFile "appointments/login")
      Just (Entity uid _) -> do
          app <- getYesod
          langs <- languages
          books <- runDB $ select $ do
              x :& _ :& s <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
              where_ $ x ^. BookUser ==. val uid
              orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
              return (x,s)
          defaultLayout $ do
              setTitleI MsgMyAppointments
              $(widgetFile "appointments/appointments")
