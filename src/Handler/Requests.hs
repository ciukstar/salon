{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Requests (getRequestsR) where

import Data.Text (intercalate, unpack, Text)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages
    , getYesod, languages
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth ( Route(LoginR), maybeAuth ) 
import Settings (widgetFile)

import Foundation
    ( Handler
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR)
    , AppMessage
      ( MsgRequests, MsgPhoto, MsgLogin, MsgSymbolHour, MsgSymbolMinute
      , MsgAwaitingApproval, MsgApproved, MsgCancelled, MsgPaid
      , MsgNoRequestsYet
      )
    )

import Database.Persist (Entity (Entity))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, innerJoin, on, where_, val
    , (:&)((:&)), (==.), (^.)
    , orderBy, desc
    )
    
import Model
    ( Book(Book), Offer, Service (Service)
    , BookStatus (BookStatusRequest, BookStatusApproved, BookStatusCancelled, BookStatusPaid)
    , EntityField
      ( BookOffer, OfferId, OfferService, ServiceId, BookUser, BookDay, BookTime
      )
    )

import Menu (menu)


getRequestsR :: Handler Html
getRequestsR = do
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    case user of
      Nothing -> defaultLayout $ do
          setTitleI MsgLogin
          $(widgetFile "requests/login")
      Just (Entity uid _) -> do
          app <- getYesod
          langs <- languages
          requests <- runDB $ select $ do
              x :& _ :& s <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
              where_ $ x ^. BookUser ==. val uid
              orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
              return (x,s)
          defaultLayout $ do
              setTitleI MsgRequests
              $(widgetFile "requests/requests")


resolve :: BookStatus -> (Text, Text, AppMessage)
resolve BookStatusRequest = ("orange", "hourglass_top", MsgAwaitingApproval)
resolve BookStatusApproved = ("green", "verified", MsgApproved)
resolve BookStatusCancelled = ("red", "block", MsgCancelled)
resolve BookStatusPaid = ("green", "paid", MsgPaid)
