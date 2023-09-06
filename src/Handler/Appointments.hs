{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Appointments
  ( getAppointmentsR
  , getAppointmentR
  ) where

import Text.Hamlet (Html)

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core (Yesod(defaultLayout), getMessages)
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist (Entity (Entity), YesodPersist(runDB))
import Settings (widgetFile)

import Database.Esqueleto.Experimental
    ( select, from, table, innerJoin, leftJoin, on, where_, val
    , (:&)((:&)), (^.), (?.), (==.), selectOne, isNothing_
    )

import Foundation
    ( Handler
    , Route (ProfileR, AppointmentsR, AppointmentR, BookR, AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage
      ( MsgMyAppointments, MsgLogin, MsgLogout, MsgPhoto
      , MsgLoginToSeeYourAppointments, MsgNoAppointmentsYet
      , MsgBookAppointment, MsgAppointment, MsgCancelAppointment
      , MsgRescheduleAppointment
      )
    )

import Model
    ( BookId, Book(Book), Offer (Offer), User (User)
    , EntityField (BookOffer, OfferId, BookUser, UserId, BookId)
    )


getAppointmentR :: BookId -> Handler Html
getAppointmentR bid = do
    user <- maybeAuth
    book <- runDB $ selectOne $ do
        x <- from $ table @Book
        where_ $ x ^. BookId ==. val bid
        case user of
          Just (Entity uid _) -> where_ $ x ^. BookUser ==. val uid
          Nothing -> where_ $ val False
        return x
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
          books <- runDB $ select $ do
              x <- from $ table @Book
              where_ $ x ^. BookUser ==. val uid
              return x
          defaultLayout $ do
              setTitleI MsgMyAppointments
              $(widgetFile "appointments/appointments")
