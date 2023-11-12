{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Handler.Appointments
  ( getAppointmentsR
  , getAppointmentR
  , postAppointmentR
  , postAppointmentCancelR
  , getAppointmentHistR
  , getAppointmentRescheduleR
  , postAppointmentApproveR
  , resolveBookStatus
  , getAppointmentsSearchR
  , getBookingsCalendarR
  , getBookingsDayListR
  , getBookingItemR
  ) where

import Data.Bifunctor (Bifunctor(bimap, second))
import qualified Data.Map.Lazy as ML (fromListWith, lookup, toList)
import Control.Monad (unless)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (unpack, intercalate, Text, pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Calendar
    ( Day, toGregorian, weekFirstDay, DayOfWeek (Monday)
    , DayPeriod (periodFirstDay, periodLastDay), addDays
    )
import Data.Time.Calendar.Month (Month, pattern YearMonth, addMonths)
import Data.Time
    ( TimeOfDay, TimeZone (timeZoneMinutes), LocalTime (LocalTime)
    , utcToLocalTime, minutesToTimeZone
    )
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (renderMessage, SomeMessage (SomeMessage))

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, getYesod, languages
    , redirect, addMessageI, MonadIO (liftIO)
    , whamlet, newIdent, getCurrentRoute
    )
import Yesod.Core.Handler (setUltDestCurrent, reqGetParams, getRequest)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess, FormFailure, FormMissing)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvErrors, fvLabel, fvInput, fvId)
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq, checkM)
import Yesod.Form.Fields
    ( Textarea (Textarea), intField, timeField, dayField, textField
    , textareaField, searchField
    )
import Yesod.Persist
    ( Entity (Entity, entityVal), YesodPersist(runDB), PersistStoreWrite (insert_) )
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Settings (widgetFile)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, innerJoin, on, where_, val, like, in_, valList
    , (:&)((:&)), (^.), (==.), (?.), (=.), (||.), (%), (++.)
    , orderBy, desc, leftJoin, just, update, set, exists, upper_
    , justList, unValue, between
    )

import Foundation
    ( Handler, Widget
    , Route
      ( ProfileR, AppointmentsR, AppointmentR, BookOffersR, AuthR
      , PhotoPlaceholderR, AccountPhotoR, ServiceThumbnailR, AdminR
      , AppointmentCancelR, AppointmentHistR, AppointmentRescheduleR
      , AppointmentApproveR, AppointmentsSearchR, BookingsCalendarR
      , BookingsDayListR, BookingItemR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgMyAppointments, MsgLogin, MsgPhoto, MsgNoAppointmentsYet
      , MsgBack, MsgLoginToSeeYourAppointments, MsgBookAppointment
      , MsgAppointment, MsgDuration, MsgAdjusted, MsgSymbolHour
      , MsgSymbolMinute, MsgAppoinmentStatus, MsgService, MsgReschedule
      , MsgMeetingTime, MsgHistory, MsgMeetingLocation, MsgAwaitingApproval
      , MsgApproved, MsgCancelled, MsgPaid, MsgCustomer, MsgCancelAppointment
      , MsgPleaseConfirm, MsgAcquaintance, MsgCancelAppointmentReally, MsgNo
      , MsgYes, MsgLoginToPerformAction, MsgEntityNotFound, MsgNoHistoryYet
      , MsgStatus, MsgTimeZone, MsgTime, MsgDay, MsgInvalidFormData, MsgSave
      , MsgCancel, MsgApprove, MsgMissingForm, MsgAppointmentTimeIsInThePast
      , MsgAppointmentDayIsInThePast, MsgMinutes, MsgTimeZoneOffset, MsgLocation
      , MsgNavigationMenu, MsgUserProfile, MsgUnassigned, MsgAssignee, MsgSearch
      , MsgSelect, MsgRequest, MsgShowAll, MsgNoAppointmentsFound, MsgToday
      , MsgList, MsgCalendar, MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat
      , MsgSun, MsgPending, MsgCompleted
      )
    )

import Model
    ( BookStatus
      ( BookStatusRequest, BookStatusApproved, BookStatusCancelled, BookStatusPaid
      , BookStatusAdjusted
      )
    , BookId, Book (Book, bookDay, bookTz, bookTzo, bookTime, bookAddr)
    , Offer (Offer), Service (Service), Role (Role, roleName), Hist (Hist)
    , Staff (Staff, staffName), Thumbnail (Thumbnail), UserId, User (User)
    , Business, ContactUs (ContactUs)
    , EntityField
      ( BookOffer, OfferId, BookCustomer, BookId, OfferService, ServiceId
      , BookDay, BookTime, BookRole, RoleId, RoleStaff, StaffId, ThumbnailService
      , BookStatus, HistBook, HistLogtime, BookTz, HistUser
      , UserId, BookTzo, BookAddr, StaffUser, ServiceName, ServiceOverview, ServiceDescr
      , RoleName, OfferName, OfferPrefix, OfferSuffix, OfferDescr, StaffName, StaffPhone
      , StaffMobile, StaffEmail, UserName, UserFullName, UserEmail, BookStatus
      , BusinessCurrency
      )
    )

import Menu (menu)


getBookingItemR :: UserId -> Day -> BookId -> Handler Html
getBookingItemR cid day bid = do
    book <- runDB $ selectOne $ do
        x <- from $ table @Book
        where_ $ x ^. BookId ==. val bid
        return x
    defaultLayout $ do
        setTitleI MsgAppointment
        $(widgetFile "appointments/calendar/item")


getBookingsDayListR :: UserId -> Day -> Handler Html
getBookingsDayListR cid day = do
    stati <- reqGetParams <$> getRequest
    let statuses = mapMaybe (readMaybe . unpack . snd) . filter ((== "status") . fst) $ stati
    books <- runDB $ select $ do
        x :& _ :& s <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
        where_ $ x ^. BookCustomer ==. val cid
        where_ $ x ^. BookDay ==. val day
        unless (null statuses) $ where_ $ x ^. BookStatus `in_` valList statuses
        orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
        return (x,s)
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian
    appointmentDay <- newIdent
    defaultLayout $ do
        setTitleI MsgMyAppointments
        $(widgetFile "appointments/calendar/list")


getBookingsCalendarR :: Month -> Handler Html
getBookingsCalendarR month = do
    stati <- reqGetParams <$> getRequest
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    case user of
      Nothing -> defaultLayout $ do
          setTitleI MsgMyAppointments
          $(widgetFile "appointments/login")
      Just (Entity uid _) -> do
          let statuses = mapMaybe (readMaybe . unpack . snd) . filter ((== "status") . fst) $ stati
          -- ML.fromListWith (++) . (bimap (bimap unValue unValue) ((:[]) . unValue) <$>) <$> 
          booksX <- (bimap unValue ((:[]) . bimap unValue unValue) <$>) <$> runDB ( select $ do
              x <- from $ table @Book
              where_ $ x ^. BookCustomer ==. val uid
              where_ $ x ^. BookDay `between` (val (periodFirstDay month), val (periodLastDay month))
              unless (null statuses) $ where_ $ x ^. BookStatus `in_` valList statuses
              orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
              return (x ^. BookDay, (x ^. BookStatus, x ^. BookTime)) )

          let booksY = ML.fromListWith (++) booksX
          let books = ML.fromListWith (++) . (second (:[]) <$>) <$> booksY

          let start = weekFirstDay Monday (periodFirstDay month)
          let end = addDays 41 start
          let page = [start .. end]
          let next = addMonths 1 month
          let prev = addMonths (-1) month

          today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
          curr <- getCurrentRoute
          toolbarTop <- newIdent
          divCalendar <- newIdent
          defaultLayout $ do
              setTitleI MsgMyAppointments
              $(widgetFile "appointments/calendar/page")


getAppointmentsSearchR :: Handler Html
getAppointmentsSearchR = do
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    case user of
      Nothing -> defaultLayout $ do
          setTitleI MsgLogin
          $(widgetFile "appointments/login")
      Just (Entity uid _) -> do
          q <- runInputGet $ iopt (searchField True) "q"
          stati <- filter ((== "status") . fst) . reqGetParams <$> getRequest
          let states = mapMaybe (readMaybe . unpack . snd) stati
          assignees <- filter ((== "assignee") . fst) . reqGetParams <$> getRequest
          let eids = mapMaybe ( (toSqlKey <$>) . readMaybe . unpack . snd ) assignees

          employees <- runDB $ select $ do
              e <- from $ table @Staff
              where_ $ exists $ do
                  b :& _ <- from $ table @Book
                      `innerJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. just (r ^. RoleId))
                  where_ $ b ^. BookCustomer ==. val uid
              return e

          appointments <- runDB $ select $ do
              x :& o :& s :& r :& e :& c <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
                  `leftJoin` table @Role `on` (\(x :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
                  `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
                  `leftJoin` table @User `on` (\(_ :& _ :& _ :& _ :& e :& c) -> e ?. StaffUser ==. just (c ?. UserId))

              where_ $ x ^. BookCustomer ==. val uid

              case q of
                Just query -> where_ $ (upper_ (s ^. ServiceName) `like` ((%) ++. upper_ (val query) ++. (%)))
                  ||. (upper_ (s ^. ServiceOverview) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (s ^. ServiceDescr) `like` ((%) ++. upper_ (just (val (Textarea query))) ++. (%)))
                  ||. (upper_ (r ?. RoleName) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (o ^. OfferName) `like` ((%) ++. upper_ (val query) ++. (%)))
                  ||. (upper_ (o ^. OfferPrefix) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (o ^. OfferSuffix) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (o ^. OfferDescr) `like` ((%) ++. upper_ (just (val (Textarea query))) ++. (%)))
                  ||. (upper_ (e ?. StaffName) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (e ?. StaffPhone) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                  ||. (upper_ (e ?. StaffMobile) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                  ||. (upper_ (e ?. StaffEmail) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                  ||. (upper_ (c ?. UserName) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (c ?. UserFullName) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                  ||. (upper_ (c ?. UserEmail) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                Nothing -> return ()

              case states of
                [] -> return ()
                xs -> where_ $ x ^. BookStatus `in_` valList xs

              unless (null eids) $ where_ $ e ?. StaffId `in_` justList ( valList eids )

              orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
              return (x,s)

          formSearch <- newIdent
          dlgStatusList <- newIdent
          dlgAssignee <- newIdent
          defaultLayout $ do
              setTitleI MsgSearch
              $(widgetFile "appointments/search/search")


postAppointmentApproveR :: BookId -> Handler Html
postAppointmentApproveR bid = do
    ((fr,_),_) <- runFormPost formApprove
    user <- maybeAuth
    case (fr, user) of
      (FormSuccess _, Just (Entity uid _)) -> do
          book <- runDB $ selectOne $ do
              b :& r :& e <- from $ table @Book
                  `leftJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. r ?. RoleId)
                  `leftJoin` table @Staff `on` (\(_ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
              where_ $ b ^. BookId ==. val bid
              where_ $ b ^. BookCustomer ==. val uid
              return (b,r,e)
          case book of
            Just (Entity _ (Book _ _ _ day time addr tzo tz _),role,empl) -> do
                runDB $ update $ \x -> do
                    set x [BookStatus =. val BookStatusApproved]
                    where_ $ x ^. BookId ==. val bid
                    where_ $ x ^. BookCustomer ==. val uid
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusApproved
                    (roleName . entityVal <$> role) (staffName . entityVal <$> empl)
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
    book <- runDB $ selectOne $ do
        x <- from $ table @Book
        where_ $ x ^. BookId ==. val bid
        return x
    (fw,et) <- generateFormPost $ formReschedule book
    defaultLayout $ do
        setTitleI MsgReschedule
        $(widgetFile "appointments/reschedule/reschedule")


formReschedule :: Maybe (Entity Book)
               -> Html -> MForm Handler (FormResult (Day,TimeOfDay,Textarea,TimeZone,Text),Widget)
formReschedule book extra = do

    (dayR,dayV) <- mreq futureDayField FieldSettings
        { fsLabel = SomeMessage MsgDay
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (bookDay . entityVal <$> book)

    (tzR,tzV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTimeZone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("readonly","readonly")]
        } (bookTz . entityVal <$> book)

    (tzoR,tzoV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgTimeZoneOffset
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("readonly","readonly")]
        } (timeZoneMinutes . bookTzo . entityVal <$> book)

    (timeR,timeV) <- mreq (futureTimeField dayR (minutesToTimeZone <$> tzoR)) FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (bookTime . entityVal <$> book)

    (addrR,addrV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgLocation
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [ ("class","mdc-text-field__input")
                    , ("readonly","readonly")
                    ]
        } (bookAddr . entityVal <$> book)

    sectionTime <- newIdent
    sectionLocation <- newIdent
    return ( (,,,,) <$> dayR <*> timeR <*> addrR <*> (minutesToTimeZone <$> tzoR) <*> tzR
           , $(widgetFile "appointments/reschedule/form")
           )
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
              h :& u <- from $ table @Hist `innerJoin` table @User
                  `on` (\(h :& u) ->  h ^. HistUser ==. u ^. UserId)
              where_ $ h ^. HistBook ==. val bid
              where_ $ exists $ do
                  b <- from $ table @Book
                  where_ $ b ^. BookId ==. h ^. HistBook
                  where_ $ b ^. BookCustomer ==. val uid
              orderBy [desc (h ^. HistLogtime)]
              return (h,u)
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
              b :& r :& e <- from $ table @Book
                  `leftJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. r ?. RoleId)
                  `leftJoin` table @Staff `on` (\(_ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
              where_ $ b ^. BookId ==. val bid
              where_ $ b ^. BookCustomer ==. val uid
              return (b,r,e)
          case book of
            Just (Entity _ (Book _ _ _ day time addr tzo tz _),role,empl) -> do
                runDB $ update $ \x -> do
                    set x [BookStatus =. val BookStatusCancelled]
                    where_ $ x ^. BookId ==. val bid
                    where_ $ x ^. BookCustomer ==. val uid
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusCancelled
                    (roleName . entityVal <$> role) (staffName . entityVal <$> empl)
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
      (FormSuccess (day,time,_,_,_), Just (Entity uid _)) -> do
          book <- runDB $ selectOne $ do
              b :& r :& e <- from $ table @Book
                  `leftJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. r ?. RoleId)
                  `leftJoin` table @Staff `on` (\(_ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
              where_ $ b ^. BookId ==. val bid
              where_ $ b ^. BookCustomer ==. val uid
              return (b,r,e)
          case book of
            Just (Entity _ (Book _ _ _ _ _ addr tzo tz _),role,empl) -> do
                runDB $ update $ \x -> do
                    set x [ BookDay =. val day
                          , BookTime =. val time
                          , BookAddr =. val addr
                          , BookTzo =. val tzo
                          , BookTz =. val tz
                          , BookStatus =. val BookStatusRequest
                          ]
                    where_ $ x ^. BookId ==. val bid
                    where_ $ x ^. BookCustomer ==. val uid
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusRequest
                    (roleName . entityVal <$> role) (staffName . entityVal <$> empl)
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
          $(widgetFile "appointments/reschedule/reschedule")


getAppointmentR :: BookId -> Handler Html
getAppointmentR bid = do
    app <- getYesod
    langs <- languages
    user <- maybeAuth

    currency <- (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Business
        return $ x ^. BusinessCurrency )

    location <- runDB $ selectOne $ from $ table @ContactUs

    book <- runDB $ selectOne $ do
        x :& o :& s :& t :& r :& e <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& _ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
            `leftJoin` table @Role `on` (\(x :& _ :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
        where_ $ x ^. BookId ==. val bid
        case user of
          Just (Entity uid _) -> where_ $ x ^. BookCustomer ==. val uid
          Nothing -> where_ $ val False
        orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
        return (x,o,s,t,r,e)
    dlgCancelAppointment <- newIdent
    formAppoitmentCancel <- newIdent
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
    stati <- reqGetParams <$> getRequest
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    case user of
      Nothing -> defaultLayout $ do
          setTitleI MsgMyAppointments
          $(widgetFile "appointments/login")
      Just (Entity uid _) -> do
          let statuses = mapMaybe (readMaybe . unpack . snd) . filter ((== "status") . fst) $ stati
          books <- runDB $ select $ do
              x :& _ :& s <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
              where_ $ x ^. BookCustomer ==. val uid
              unless (null statuses) $ where_ $ x ^. BookStatus `in_` valList statuses
              orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
              return (x,s)
          today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
          curr <- getCurrentRoute
          toolbarTop <- newIdent
          defaultLayout $ do
              setTitleI MsgMyAppointments
              $(widgetFile "appointments/appointments")


fstsec :: a -> b -> (a,b)
fstsec x = (x,)


resolveBookStatus :: BookStatus -> (Text, Text, AppMessage, AppMessage)
resolveBookStatus BookStatusRequest = ("orange", "hourglass_top", MsgPending, MsgAwaitingApproval)
resolveBookStatus BookStatusAdjusted = ("blue", "reply_all", MsgAdjusted, MsgAdjusted)
resolveBookStatus BookStatusApproved = ("green", "verified", MsgApproved, MsgApproved)
resolveBookStatus BookStatusCancelled = ("red", "block", MsgCancelled, MsgCancelled)
resolveBookStatus BookStatusPaid = ("green", "paid", MsgPaid, MsgCompleted)


statusList :: [(BookStatus, AppMessage)]
statusList = [ (BookStatusRequest, MsgRequest)
             , (BookStatusApproved, MsgApproved)
             , (BookStatusCancelled, MsgCancelled)
             , (BookStatusPaid, MsgPaid)
             ]
