{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Appointments
  ( getAppointmentsR
  , getAppointmentR
  , postAppointmentR
  , postAppointmentCancelR
  , getAppointmentHistR
  , getAppointmentRescheduleR
  ) where

import Data.Text (unpack, intercalate, Text)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Clock (getCurrentTime)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getYesod, languages
    , preEscapedToMarkup, redirect, addMessageI, MonadIO (liftIO)
    , whamlet
    )
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Types (MForm, FormResult (FormSuccess, FormFailure, FormMissing))
import Yesod.Form.Functions (generateFormPost, runFormPost)
import Yesod.Form.Fields (unTextarea)
import Yesod.Persist
    ( Entity (Entity), YesodPersist(runDB), PersistStoreWrite (insert_) )
import Settings (widgetFile)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, innerJoin, on, where_, val
    , (:&)((:&)), (^.), (==.), (?.), (=.)
    , orderBy, desc, leftJoin, just, update, set, exists
    )

import Foundation
    ( Handler, Widget
    , Route
      ( ProfileR, AppointmentsR, AppointmentR, BookOffersR, AuthR
      , PhotoPlaceholderR, AccountPhotoR, ServiceThumbnailR, AdminR
      , AppointmentCancelR, AppointmentHistR, AppointmentRescheduleR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgMyAppointments, MsgLogin, MsgLogout, MsgPhoto
      , MsgLoginToSeeYourAppointments, MsgNoAppointmentsYet
      , MsgBookAppointment, MsgAppointment, MsgDuration
      , MsgSymbolHour, MsgSymbolMinute, MsgAppoinmentStatus
      , MsgService, MsgReschedule, MsgMeetingTime, MsgHistory
      , MsgMeetingLocation, MsgAwaitingApproval, MsgApproved
      , MsgCancelled, MsgPaid, MsgCustomer, MsgCancelAppointment
      , MsgPleaseConfirm, MsgAcquaintance, MsgCancelAppointmentReally
      , MsgNo, MsgYes, MsgLoginToPerformAction, MsgEntityNotFound
      , MsgNoHistoryYet, MsgStatus, MsgTimezone, MsgTime, MsgDay
      , MsgInvalidFormData, MsgMissingForm, MsgSave
      )
    )

import Model
    ( BookStatus (BookStatusRequest, BookStatusApproved, BookStatusCancelled, BookStatusPaid)
    , BookId, Book(Book), Offer (Offer), Service (Service), Role (Role), Hist (Hist)
    , Staff (Staff), Thumbnail (Thumbnail), User (User), Contents (Contents)
    , EntityField
      ( BookOffer, OfferId, BookUser, BookId, OfferService, ServiceId
      , BookDay, BookTime, BookRole, RoleId, RoleStaff, StaffId, ThumbnailService
      , ContentsSection, BookStatus, HistBook, HistLogtime
      )
    )

import Menu (menu)
import Handler.Contacts (section)
import Data.Time.Calendar (Day)
import Data.Time (TimeOfDay, TimeZone)


getAppointmentRescheduleR :: BookId -> Handler Html
getAppointmentRescheduleR bid = do
    (fw,et) <- generateFormPost formReschedule
    defaultLayout $ do
        setTitleI MsgReschedule
        $(widgetFile "appointments/reschedule")


formReschedule :: Html -> MForm Handler (FormResult (Day, TimeOfDay, TimeZone), Widget)
formReschedule extra = return (FormMissing,[whamlet|#{extra}|])


getAppointmentHistR :: BookId -> Handler Html
getAppointmentHistR bid = do
    user <- maybeAuth
    case user of
      Just (Entity uid _) -> do
          hist <- runDB $ select $ do
              x <- from $ table @Hist
              where_ $ x ^. HistBook ==. val bid
              where_ $ exists $ do
                  b <- from $ table @Book
                  where_ $ b ^. BookId ==. x ^. HistBook
                  where_ $ b ^. BookUser ==. val uid
              orderBy [desc (x ^. HistLogtime)]
              return x
          defaultLayout $ do
              setTitleI MsgHistory
              $(widgetFile "appointments/hist")
      Nothing -> do
          addMessageI "warn" MsgLoginToPerformAction
          redirect $ AppointmentR bid


postAppointmentCancelR :: BookId -> Handler Html
postAppointmentCancelR bid = do
    ((fr,_),_) <- runFormPost formCancel
    user <- maybeAuth
    case (fr, user) of
      (FormSuccess _, Just (Entity uid _)) -> do
          book <- runDB $ selectOne $ do
              x <- from $ table @Book
              where_ $ x ^. BookId ==. val bid
              where_ $ x ^. BookUser ==. val uid
              return x
          case book of
            Just (Entity bid' (Book _ _ _ day time tz status)) -> do
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid' now day time tz status
                runDB $ update $ \x -> do
                    set x [BookStatus =. val BookStatusCancelled]
                    where_ $ x ^. BookId ==. val bid
                    where_ $ x ^. BookUser ==. val uid
                redirect $ AppointmentR bid

            Nothing -> do
                addMessageI "warn" MsgEntityNotFound
                redirect $ AppointmentR bid
      (FormFailure _, _) -> do
          addMessageI "warn" MsgInvalidFormData
          redirect $ AppointmentR bid
      (FormMissing, _) -> do
          addMessageI "warn" MsgMissingForm
          redirect $ AppointmentR bid
      (_, Nothing) -> do
          addMessageI "warn" MsgLoginToPerformAction
          redirect $ AppointmentR bid

postAppointmentR :: BookId -> Handler Html
postAppointmentR bid = do
    ((fr,fw),et) <- runFormPost formReschedule
    case fr of
      FormSuccess (day,time,tz) -> undefined
      _ -> defaultLayout $ do
          setTitleI MsgReschedule
          $(widgetFile "appointments/reschedule")


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
    msgs <- getMessages
    (fw,et) <- generateFormPost formCancel
    defaultLayout $ do
        setTitleI MsgAppointment
        $(widgetFile "appointments/appointment")


formCancel :: Html -> MForm Handler (FormResult (), Widget)
formCancel extra = return (FormSuccess (),[whamlet|#{extra}|])


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


resolve :: BookStatus -> (Text, Text, AppMessage)
resolve BookStatusRequest = ("orange", "hourglass_top", MsgAwaitingApproval)
resolve BookStatusApproved = ("green", "verified", MsgApproved)
resolve BookStatusCancelled = ("red", "block", MsgCancelled)
resolve BookStatusPaid = ("green", "paid", MsgPaid)
