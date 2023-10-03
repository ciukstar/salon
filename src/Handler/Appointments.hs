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
  , postAppointmentApproveR
  ) where

import Data.Maybe (isJust)
import Data.Text (unpack, intercalate, Text, pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Calendar (Day)
import Data.Time
    ( TimeOfDay, TimeZone (timeZoneMinutes), LocalTime (LocalTime)
    , utcToLocalTime, minutesToTimeZone
    )
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage, SomeMessage (SomeMessage))

import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getYesod, languages
    , preEscapedToMarkup, redirect, addMessageI, MonadIO (liftIO)
    , whamlet, newIdent
    )
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess, FormFailure, FormMissing)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvErrors, fvLabel, fvInput, fvId)
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq, checkM)
import Yesod.Form.Fields (unTextarea, intField, timeField, dayField)
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
      , AppointmentApproveR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgMyAppointments, MsgLogin, MsgLogout, MsgPhoto
      , MsgLoginToSeeYourAppointments, MsgNoAppointmentsYet
      , MsgBookAppointment, MsgAppointment, MsgDuration, MsgAdjusted
      , MsgSymbolHour, MsgSymbolMinute, MsgAppoinmentStatus
      , MsgService, MsgReschedule, MsgMeetingTime, MsgHistory
      , MsgMeetingLocation, MsgAwaitingApproval, MsgApproved
      , MsgCancelled, MsgPaid, MsgCustomer, MsgCancelAppointment
      , MsgPleaseConfirm, MsgAcquaintance, MsgCancelAppointmentReally
      , MsgNo, MsgYes, MsgLoginToPerformAction, MsgEntityNotFound
      , MsgNoHistoryYet, MsgStatus, MsgTimezone, MsgTime, MsgDay
      , MsgInvalidFormData, MsgMissingForm, MsgSave, MsgCancel, MsgApprove
      , MsgAppointmentTimeIsInThePast, MsgAppointmentDayIsInThePast, MsgMinutes
      )
    )

import Model
    ( BookStatus
      ( BookStatusRequest, BookStatusApproved, BookStatusCancelled, BookStatusPaid
      , BookStatusAdjusted
      )
    , BookId, Book(Book), Offer (Offer), Service (Service), Role (Role), Hist (Hist)
    , Staff (Staff), Thumbnail (Thumbnail), User (User), Contents (Contents)
    , EntityField
      ( BookOffer, OfferId, BookUser, BookId, OfferService, ServiceId
      , BookDay, BookTime, BookRole, RoleId, RoleStaff, StaffId, ThumbnailService
      , ContentsSection, BookStatus, HistBook, HistLogtime, BookTz
      )
    )

import Menu (menu)
import Handler.Contacts (section)


postAppointmentApproveR :: BookId -> Handler Html
postAppointmentApproveR bid = do
    ((fr,_),_) <- runFormPost formApprove
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
                    set x [BookStatus =. val BookStatusApproved]
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


formApprove :: Html -> MForm Handler (FormResult (), Widget)
formApprove extra = return (FormSuccess (),[whamlet|#{extra}|])


getAppointmentRescheduleR :: BookId -> Handler Html
getAppointmentRescheduleR bid = do
    tz <- (minutesToTimeZone <$>) <$> runInputGet ( iopt intField "tz" )
    (fw,et) <- generateFormPost $ formReschedule tz
    defaultLayout $ do
        setTitleI MsgReschedule
        $(widgetFile "appointments/reschedule")


formReschedule :: Maybe TimeZone -> Html -> MForm Handler (FormResult (Day, TimeOfDay, TimeZone), Widget)
formReschedule tz extra = do

    (dayR,dayV) <- mreq futureDayField FieldSettings
        { fsLabel = SomeMessage MsgDay
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing

    (tzR,tzV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgTimezone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (timeZoneMinutes <$> tz)

    (timeR,timeV) <- mreq (futureTimeField dayR (minutesToTimeZone <$> tzR)) FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing

    let r = (,,) <$> dayR <*> timeR <*> (minutesToTimeZone <$> tzR)

    let w = [whamlet|
#{extra}
$forall (v,icon) <- [(dayV,"event"),(timeV,"schedule")]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled.mdc-text-field--with-trailing-icon data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      <button.mdc-icon-button.mdc-text-field__icon.mdc-text-field__icon--trailing.material-symbols-outlined
        tabindex=0 role=button onclick="document.getElementById('#{fvId v}').showPicker()"
        style="position:absolute;right:2px;background-color:inherit">
        <span.mdc-icon-button__ripple>
        <span.mdc-icon-button__focus-ring>
        #{pack icon}
      <div.mdc-line-ripple>
    $maybe errs <- fvErrors v
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}
$forall (v,icon) <- [(tzV,"language")]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled.mdc-text-field--with-trailing-icon data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      <button.mdc-icon-button.mdc-text-field__icon.mdc-text-field__icon--trailing.material-symbols-outlined
        tabindex=0 role=button onclick="document.getElementById('#{fvId v}').showPicker()"
        style="position:absolute;right:2px;background-color:inherit">
        <span.mdc-icon-button__ripple>
        <span.mdc-icon-button__focus-ring>
        #{pack icon}
      <div.mdc-line-ripple>
    <div.mdc-text-field-helper-line>
      $maybe errs <- fvErrors v
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}
      $nothing
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--persistent>
          _{MsgMinutes}
|]
    return (r,w)
  where

      futureTimeField dayR tzR = checkM (futureTime dayR tzR) timeField

      futureTime :: FormResult Day -> FormResult TimeZone -> TimeOfDay -> Handler (Either AppMessage TimeOfDay)
      futureTime dayR tzR t = do
          now <- liftIO getCurrentTime
          return $ case (dayR,tzR) of
            (FormSuccess d, FormSuccess z) -> if LocalTime d t < utcToLocalTime z now
                then Left MsgAppointmentTimeIsInThePast
                else Right t
            _ -> Right t


      futureDayField = checkM futureDay dayField

      futureDay :: Day -> Handler (Either AppMessage Day)
      futureDay d = do
          today <- liftIO $ utctDay <$> getCurrentTime
          return $ if d < today
              then Left MsgAppointmentDayIsInThePast
              else Right d


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
    user <- maybeAuth
    ((fr,fw),et) <- runFormPost $ formReschedule Nothing
    case (fr,user) of
      (FormSuccess (day,time,tz), Just (Entity uid _)) -> do
          book <- runDB $ selectOne $ do
              x <- from $ table @Book
              where_ $ x ^. BookId ==. val bid
              where_ $ x ^. BookUser ==. val uid
              return x
          case book of
            Just (Entity bid' (Book _ _ _ day' time' tz' status')) -> do
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid' now day' time' tz' status'
                runDB $ update $ \x -> do
                    set x [ BookDay =. val day
                          , BookTime =. val time
                          , BookTz =. val tz
                          , BookStatus =. val BookStatusRequest
                          ]
                    where_ $ x ^. BookId ==. val bid
                    where_ $ x ^. BookUser ==. val uid
                redirect $ AppointmentR bid

            Nothing -> do
                addMessageI "warn" MsgEntityNotFound
                redirect $ AppointmentR bid
      (FormMissing, _) -> do
          addMessageI "warn" MsgMissingForm
          redirect $ AppointmentR bid
      (_, Nothing) -> do
          addMessageI "warn" MsgLoginToPerformAction
          redirect $ AppointmentR bid
      (FormFailure _, _) -> defaultLayout $ do
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
    dlgCancelAppointment <- newIdent
    formAppoitmentCancel <- newIdent
    formGetAppointmentReschedule <- newIdent
    formGetAppointmentApprove <- newIdent
    msgs <- getMessages
    (fwCancel,etCancel) <- generateFormPost formCancel
    (fwApprove,etApprove) <- generateFormPost formApprove
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
resolve BookStatusAdjusted = ("blue", "reply_all", MsgAdjusted)
resolve BookStatusApproved = ("green", "verified", MsgApproved)
resolve BookStatusCancelled = ("red", "block", MsgCancelled)
resolve BookStatusPaid = ("green", "paid", MsgPaid)
