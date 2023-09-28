{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Requests (getRequestsR, getRequestR) where

import Data.Maybe (mapMaybe)
import Data.Text (intercalate, unpack, Text, pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (renderMessage)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages
    , getYesod, languages, preEscapedToMarkup, whamlet, getRequest
    , YesodRequest (reqGetParams), newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth ( Route(LoginR, LogoutR), maybeAuth )
import Yesod.Form.Fields (Textarea(unTextarea))
import Yesod.Form.Types (MForm, FormResult (FormSuccess))
import Yesod.Form.Functions (generateFormPost)
import Settings (widgetFile)

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR, RequestsR, RequestR
      , AppointmentCancelR, AppointmentHistR, AppointmentRescheduleR, AdminR
      , ServiceThumbnailR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgRequests, MsgPhoto, MsgLogin, MsgLogout, MsgSymbolHour
      , MsgSymbolMinute, MsgAwaitingApproval, MsgApproved, MsgCancelled
      , MsgPaid, MsgLoginToSeeTheRequests, MsgRequest, MsgStatus, MsgAssignee
      , MsgYes, MsgNo, MsgCancelAppointmentReally, MsgPleaseConfirm, MsgHistory
      , MsgReschedule, MsgMeetingLocation, MsgCustomer, MsgSelect, MsgCancel
      , MsgService, MsgMeetingTime, MsgAcquaintance, MsgAppoinmentStatus
      , MsgDuration, MsgApprove, MsgNoPendingRequestsYet, MsgShowAll
      )
    )

import Database.Persist (Entity (Entity))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, innerJoin, leftJoin, on, where_, val
    , (:&)((:&)), (==.), (^.), (?.)
    , orderBy, desc, just, selectOne, valList, in_
    )
    
import Model
    ( Book (Book), BookId, Offer, Service (Service), Role (Role), Staff (Staff)
    , Offer (Offer), Thumbnail (Thumbnail), User (User), Contents (Contents)
    , BookStatus
      ( BookStatusRequest, BookStatusApproved, BookStatusCancelled
      , BookStatusPaid
      )
    , EntityField
      ( BookOffer, OfferId, OfferService, ServiceId, BookDay, BookTime
      , BookRole, RoleId, RoleStaff, StaffId, StaffUser, ContentsSection, BookId
      , ThumbnailService, BookUser, UserId, BookStatus
      )
    )

import Menu (menu)
import Handler.Contacts (section)


getRequestR :: BookId -> Handler Html
getRequestR bid = do
    stati <- filter ((== "status") . fst) . reqGetParams <$> getRequest
    app <- getYesod
    langs <- languages
    user <- maybeAuth
    location <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    request <- runDB $ selectOne $ do
        x :& o :& s :& t :& r :& e :& c <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& _ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
            `innerJoin` table @Role `on` (\(x :& _ :& _ :& _ :& r) -> x ^. BookRole ==. just (r ^. RoleId))
            `innerJoin` table @Staff `on` (\(_ :& _ :& _ :& _ :& r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
            `innerJoin` table @User `on` (\(x :& _ :& _ :& _ :& _ :& _ :& c) -> x ^. BookUser ==. c ^. UserId)
        where_ $ x ^. BookId ==. val bid
        case user of
          Just (Entity uid _) -> where_ $ e ^. StaffUser ==. just (val uid)
          Nothing -> where_ $ val False
        return (x,o,s,t,r,e,c)
    msgs <- getMessages
    (fw,et) <- generateFormPost formCancel
    defaultLayout $ do
        setTitleI MsgRequest
        $(widgetFile "requests/request")


formCancel :: Html -> MForm Handler (FormResult (), Widget)
formCancel extra = return (FormSuccess (),[whamlet|#{extra}|])


getRequestsR :: Handler Html
getRequestsR = do
    stati <- filter ((== "status") . fst) . reqGetParams <$> getRequest
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
          dlgStatusList <- newIdent
          let statusList = [ (BookStatusRequest, MsgRequest)
                           , (BookStatusApproved, MsgApproved)
                           , (BookStatusCancelled, MsgCancelled)
                           , (BookStatusPaid, MsgPaid)
                           ]
          let states = mapMaybe (readMaybe . unpack . snd) stati
          dlgAssignee <- newIdent
          requests <- runDB $ select $ do
              x :& _ :& s :& _ :& e <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
                  `innerJoin` table @Role `on` (\(x :& _ :& _ :& r) -> x ^. BookRole ==. just (r ^. RoleId))
                  `innerJoin` table @Staff `on` (\(_ :& _ :& _ :& r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
              where_ $ e ^. StaffUser ==. just (val uid)
              case states of
                [] -> return ()
                xs -> where_ $ x ^. BookStatus `in_` valList xs
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
