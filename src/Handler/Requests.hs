{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Handler.Requests
  ( getRequestsR
  , getRequestR
  , postRequestR
  , getRequestsSearchR
  , postRequestApproveR
  , postRequestFinishR
  , postRequestAssignR
  , getRequestRescheduleR
  , getRequestHistR
  , getTasksCalendarR
  , getTasksDayListR
  , getTaskItemR
  , getTaskHistR
  ) where

import Control.Monad (unless)
import Data.Bifunctor (Bifunctor(bimap, second))
import qualified Data.Map.Lazy as ML (fromListWith, toList, lookup)
import Data.Maybe (mapMaybe, isJust, isNothing, fromMaybe)
import Data.Text (intercalate, unpack, Text, pack)
import Data.Time.Calendar
    ( Day, DayPeriod (periodFirstDay), weekFirstDay
    , DayOfWeek (Monday), addDays, toGregorian
    )
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
    ( TimeZone (timeZoneMinutes), TimeOfDay, LocalTime (LocalTime)
    , minutesToTimeZone, utcToLocalTime
    )
import Text.Hamlet (Html)
import Text.Julius (julius, RawJS (rawJS))
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (renderMessage, SomeMessage (SomeMessage))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages
    , getYesod, languages, whamlet, getRequest
    , YesodRequest (reqGetParams), newIdent, MonadIO (liftIO)
    , addMessageI, getCurrentRoute, redirectUltDest
    )
import Yesod.Core.Widget
    ( setTitleI, addScriptRemote, addStylesheetRemote, toWidget )
import Yesod.Auth ( Route(LoginR), maybeAuth )
import Yesod.Form.Fields
    ( Textarea(Textarea), searchField, intField, timeField
    , dayField, textField, textareaField
    )
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess, FormFailure, FormMissing)
    , FieldSettings (FieldSettings, fsLabel, fsId, fsTooltip, fsName, fsAttrs)
    , FieldView (fvInput, fvErrors, fvLabel, fvId)
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq, checkM)
import Yesod.Form.Input (runInputGet, iopt)
import Settings (widgetFile, AppSettings (appMapboxPk))

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR, RequestsR, RequestR
      , AdminR, ServiceThumbnailR, RequestsSearchR, RequestsSearchR
      , RequestApproveR, RequestFinishR, RequestRescheduleR, RequestHistR
      , RequestAssignR, TasksCalendarR, TasksDayListR, TaskItemR, TaskHistR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgRequests, MsgPhoto, MsgLogin, MsgSymbolHour, MsgBack, MsgShowAllMine
      , MsgSymbolMinute, MsgApproved, MsgCancelled, MsgPaid, MsgAssignee
      , MsgRequest, MsgStatus, MsgMeetingLocation, MsgSortDescending
      , MsgPleaseConfirm, MsgHistory, MsgReschedule, MsgService, MsgMeetingTime
      , MsgCustomer, MsgSelect, MsgCancel, MsgAcquaintance, MsgAppoinmentStatus
      , MsgNoPendingRequestsYet, MsgShowAll, MsgAssignedToMe, MsgWithoutAssignee
      , MsgSearch, MsgNoRequestsFound, MsgFromCoworkers, MsgApproveAppointmentConfirm
      , MsgDate, MsgTime, MsgLocation, MsgEntityNotFound, MsgInvalidFormData
      , MsgMissingForm, MsgBook, MsgRequestFinish, MsgDuration, MsgDay, MsgTimeZone
      , MsgMinutes, MsgSave, MsgFinishAppointmentConfirm, MsgNoHistoryYet
      , MsgAppointmentTimeIsInThePast, MsgAppointmentDayIsInThePast, MsgAssignToMe
      , MsgNoAssigneeRequestApprove, MsgTimeZoneOffset, MsgNavigationMenu
      , MsgUserProfile, MsgNonexpert, MsgAreYouSureYouWantToTakeOnThisTask
      , MsgApprove, MsgUnassigned, MsgList, MsgCalendar, MsgMon, MsgTue, MsgWed
      , MsgThu, MsgFri, MsgSat, MsgSun, MsgToday, MsgSortAscending, MsgAddress
      , MsgNoRequestsFoundForThisDay, MsgNoBusinessAddressFound, MsgRecordEdited
      ), App (appSettings)
    )

import Database.Persist (Entity (Entity, entityKey, entityVal), PersistStoreWrite (insert_))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, innerJoin, leftJoin, on, where_, val
    , (:&)((:&)), (==.), (^.), (?.), (%), (++.), (||.), (&&.), (=.)
    , orderBy, desc, just, selectOne, valList, in_, upper_, like, isNothing_
    , not_, update, set, exists, unValue, asc
    )

import Model
    ( Book (Book, bookDay, bookTz, bookTzo, bookTime, bookAddr)
    , BookId, Offer, ServiceId, Service (Service), Role (Role, roleName)
    , StaffId, Staff (Staff, staffName), Offer (Offer), Thumbnail (Thumbnail)
    , UserId, User (User)
    , BookStatus
      ( BookStatusRequest, BookStatusApproved, BookStatusCancelled
      , BookStatusPaid, BookStatusAdjusted
      )
    , Assignees (AssigneesMe, AssigneesNone, AssigneesOthers)
    , Business (Business), Hist (Hist), ContactUs (ContactUs)
    , EntityField
      ( BookOffer, OfferId, OfferService, ServiceId, BookDay, BookTime
      , BookRole, RoleId, RoleStaff, StaffId, StaffUser
      , BookId, ThumbnailService, BookCustomer, UserId, BookStatus, ServiceName
      , ServiceDescr, ServiceOverview, RoleName, OfferName, OfferPrefix
      , OfferSuffix, OfferDescr, StaffName, StaffPhone, StaffMobile, StaffEmail
      , UserName, UserFullName, UserEmail, BookTz, HistBook, HistLogtime
      , RoleService, HistUser, BookTzo, BookAddr, BusinessCurrency, BusinessAddr, BookPayMethod
      ), SortOrder (SortOrderDesc, SortOrderAsc)
    )

import Menu (menu)
import Handler.Appointments (resolveBookStatus)


getTaskHistR ::  UserId -> StaffId -> Day -> BookId -> Handler Html
getTaskHistR uid eid day bid = do
    stati <- reqGetParams <$> getRequest
    hist <- runDB $ select $ do
        h :& u <- from $ table @Hist `innerJoin` table @User
            `on` (\(h :& u) -> h ^. HistUser ==. u ^. UserId)
        where_ $ h ^. HistBook ==. val bid
        where_ $ exists $ do
            b <- from $ table @Book
            where_ $ b ^. BookId ==. h ^. HistBook
        orderBy [desc (h ^. HistLogtime)]
        return (h, u)
    defaultLayout $ do
        setTitleI MsgHistory
        $(widgetFile "requests/calendar/hist")


getTaskItemR ::  UserId -> StaffId -> Day -> BookId -> Handler Html
getTaskItemR uid eid day bid = do
    stati <- reqGetParams <$> getRequest

    currency <- (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Business
        return $ x ^. BusinessCurrency )

    app <- getYesod
    langs <- languages
    business <- runDB $ selectOne $ from $ table @Business
    location <- runDB $ selectOne $ from $ table @ContactUs

    address <- case location of
      Just (Entity _ (ContactUs _ _ True _ _ _ _)) -> do
         (unValue <$>) <$> runDB ( selectOne $ do
                x <- from $ table @Business
                return (x ^. BusinessAddr) )
      _ -> return Nothing

    request <- runDB $ selectOne $ do
        x :& o :& s :& t :& r :& e :& c <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& _ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
            `leftJoin` table @Role `on` (\(x :& _ :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
            `innerJoin` table @User `on` (\(x :& _ :& _ :& _ :& _ :& _ :& c) -> x ^. BookCustomer ==. c ^. UserId)
        where_ $ x ^. BookId ==. val bid
        where_ $ x ^. BookDay ==. val day
        return (x,(o,s,t,r,e,c))
    msgs <- getMessages
    dlgConfirmApprove <- newIdent
    formRequestApprove <- newIdent
    dlgConfirmFinish <- newIdent
    formAppoitmentFinish <- newIdent
    formAssignToMe <- newIdent
    (fwa,eta) <- generateFormPost formAssign
    (fw,et) <- generateFormPost $ formApprove (fst <$> request)
    dlgAssignToMeConfirm <- newIdent
    detailsLocation <- newIdent
    locationHtmlContainer <- newIdent
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgRequest
        case location of
          Just (Entity _ (ContactUs _ _ _ _ True (Just lng) (Just lat))) -> do
              mapboxPk <- appMapboxPk . appSettings <$> getYesod
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-language/v1.0.0/mapbox-gl-language.js"
              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css"
              toWidget [julius|
const main = document.getElementById(#{detailsLocation});
const mapgl = document.createElement('div');
mapgl.style.height = '300px';
mapgl.style.width = '100%';
main.appendChild(mapgl);
const map = new mapboxgl.Map({
  accessToken: #{mapboxPk},
  attributionControl: false,
  container: mapgl,
  style: 'mapbox://styles/mapbox/streets-v11',
  center: [#{rawJS $ show lng}, #{rawJS $ show lat}],
  zoom: 15
});
map.addControl(new MapboxLanguage());
map.addControl(new mapboxgl.NavigationControl());
const loc = new mapboxgl.Marker().setLngLat(
  [#{rawJS $ show lng}, #{rawJS $ show lat}]
).addTo(map);
|]
          Just (Entity _ (ContactUs _ _ _ _ True _ _)) -> do
              mapboxPk <- appMapboxPk . appSettings <$> getYesod
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-language/v1.0.0/mapbox-gl-language.js"
              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css"
              toWidget [julius|
const main = document.getElementById(#{detailsLocation});
const mapgl = document.createElement('div');
mapgl.style.height = '300px';
mapgl.style.width = '100%';
main.appendChild(mapgl);
const map = new mapboxgl.Map({
  accessToken: #{mapboxPk},
  attributionControl: false,
  container: mapgl,
  style: 'mapbox://styles/mapbox/streets-v11',
  center: [0, 0],
  zoom: 0
});
map.addControl(new MapboxLanguage());
map.addControl(new mapboxgl.NavigationControl());
|]
          _ -> return ()
        $(widgetFile "requests/calendar/item")


getTasksDayListR ::  UserId -> StaffId -> Day -> Handler Html
getTasksDayListR uid eid day = do
    stati <- reqGetParams <$> getRequest
    requestDay <- newIdent
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian
    let statuses = mapMaybe (readMaybe . unpack . snd) . filter ((== "status") . fst) $ stati
    owners <- filter ((== "assignee") . fst) . reqGetParams <$> getRequest
    let assignees = mapMaybe (readMaybe . unpack . snd) owners
    sort <- fromMaybe SortOrderDesc . (readMaybe . unpack =<<) <$> runInputGet (iopt textField "sort")
    requests <- runDB $ select $ do
        x :& _ :& s :& _ :& e <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Role `on` (\(x :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)

        where_ $ x ^. BookDay ==. val day

        case statuses of
          [] -> return ()
          xs -> where_ $ x ^. BookStatus `in_` valList xs

        let ors = [ ( AssigneesMe `elem` assignees
                    , e ?. StaffUser ==. just (just (val uid))
                    )
                  , ( AssigneesNone `elem` assignees
                    , isNothing_ $ x ^. BookRole
                    )
                  , ( AssigneesOthers `elem` assignees
                    , not_ (isNothing_ $ e ?. StaffUser) &&. not_ (e ?. StaffUser ==. just (just (val uid)))
                    )
                  ]

        case snd <$> filter fst ors of
          [] -> return ()
          xs -> where_ $ foldr (||.) (val False) xs

        orderBy $ case sort of
          SortOrderAsc -> [asc (x ^. BookDay), asc (x ^. BookTime)]
          _ -> [desc (x ^. BookDay), desc (x ^. BookTime)]

        return (x,s)
    defaultLayout $ do
        setTitleI MsgRequests
        $(widgetFile "requests/calendar/list")


getTasksCalendarR :: UserId -> StaffId -> Month -> Handler Html
getTasksCalendarR uid eid month = do
    stati <- reqGetParams <$> getRequest
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    let statuses = mapMaybe (readMaybe . unpack . snd) . filter ((== "status") . fst) $ stati
    owners <- filter ((== "assignee") . fst) . reqGetParams <$> getRequest
    let assignees = mapMaybe (readMaybe . unpack . snd) owners

    requests <- do
        xs <- (bimap unValue ((:[]) . bimap unValue unValue) <$>) <$> runDB ( select $ do
            x :& _ :& _ :& e <- from $ table @Book
                `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                `leftJoin` table @Role `on` (\(x :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
                `leftJoin` table @Staff `on` (\(_ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)

            let ors = [ ( AssigneesMe `elem` assignees
                        , e ?. StaffUser ==. just (just (val uid))
                        )
                      , ( AssigneesNone `elem` assignees
                        , isNothing_ $ x ^. BookRole
                        )
                      , ( AssigneesOthers `elem` assignees
                        , not_ (isNothing_ $ e ?. StaffUser) &&. not_ (e ?. StaffUser ==. just (just (val uid)))
                        )
                      ]

            case snd <$> filter fst ors of
              [] -> return ()
              xs -> where_ $ foldr (||.) (val False) xs

            unless (null statuses) $ where_ $ x ^. BookStatus `in_` valList statuses
            orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
            return (x ^. BookDay, (x ^. BookStatus, x ^. BookTime)) )

        return $ ML.fromListWith (++) . (second (:[]) <$>) <$> ML.fromListWith (++) xs

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    curr <- getCurrentRoute
    toolbarTop <- newIdent
    divCalendar <- newIdent
    formSearch <- newIdent
    dlgAssignee <- newIdent
    defaultLayout $ do
        setTitleI MsgRequests
        $(widgetFile "requests/calendar/page")


postRequestAssignR :: UserId -> StaffId -> BookId -> ServiceId -> Handler Html
postRequestAssignR uid eid bid sid = do
    book <- runDB $ selectOne $ do
        b <- from $ table @Book
        where_ $ b ^. BookId ==. val bid
        return b
    case book of
      Just (Entity bid' (Book _ _ _ day' time' addr' tzo' tz' _ status')) -> do

          role <- runDB $ selectOne $ do
              r :& e <- from $ table @Role `innerJoin` table @Staff
                  `on` (\(r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
              where_ $ r ^. RoleService ==. val sid
              where_ $ e ^. StaffUser ==. just (val uid)
              return (r,e)

          case role of
            Just (Entity rid (Role _ _ rname _ _),Entity _ (Staff ename _ _ _ _ _)) -> do
                runDB $ update $ \x -> do
                    set x [ BookRole =. just (val rid) ]
                    where_ $ x ^. BookId ==. val bid'
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day' time' addr' tzo' tz' status'
                    (Just rname) (Just ename)
                addMessageI info MsgRecordEdited
                redirectUltDest $ RequestR uid eid bid
            Nothing -> do
                addMessageI warn MsgNonexpert
                redirectUltDest $ RequestR uid eid bid

      Nothing -> do
          addMessageI warn MsgEntityNotFound
          redirectUltDest $ RequestR uid eid bid


getRequestHistR :: UserId -> StaffId -> BookId -> Handler Html
getRequestHistR uid eid bid = do
    stati <- reqGetParams <$> getRequest
    hist <- runDB $ select $ do
        h :& u <- from $ table @Hist `innerJoin` table @User
            `on` (\(h :& u) -> h ^. HistUser ==. u ^. UserId)
        where_ $ h ^. HistBook ==. val bid
        where_ $ exists $ do
            b <- from $ table @Book
            where_ $ b ^. BookId ==. h ^. HistBook
        orderBy [desc (h ^. HistLogtime)]
        return (h, u)
    defaultLayout $ do
        setTitleI MsgHistory
        $(widgetFile "requests/hist")


getRequestRescheduleR :: UserId -> StaffId -> BookId -> Handler Html
getRequestRescheduleR uid eid bid = do
    book <- runDB $ selectOne $ do
        x <- from $ table @Book
        where_ $ x ^. BookId ==. val bid
        return x
    (fw,et) <- generateFormPost $ formReschedule book
    defaultLayout $ do
        setTitleI MsgReschedule
        $(widgetFile "requests/reschedule/reschedule")


formReschedule :: Maybe (Entity Book)
               -> Html -> MForm Handler (FormResult (Day,TimeOfDay,Textarea,TimeZone,Text), Widget)
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
        , fsAttrs = [("class","mdc-text-field__input"),("readonly","readonly")]
        } (bookAddr . entityVal <$> book)

    sectionTime <- newIdent
    sectionLocation <- newIdent
    return ( (,,,,) <$> dayR <*> timeR <*> addrR <*> (minutesToTimeZone <$> tzoR) <*> tzR
           , $(widgetFile "requests/reschedule/form")
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


postRequestFinishR :: UserId -> StaffId -> BookId -> Handler Html
postRequestFinishR uid eid bid = do
    ((fr,_),_) <- runFormPost $ formApprove Nothing
    case fr of
      FormSuccess _bid -> do
          book <- runDB $ selectOne $ do
              b :& r :& e <- from $ table @Book
                  `leftJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. r ?. RoleId)
                  `leftJoin` table @Staff `on` (\(_ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
              where_ $ b ^. BookId ==. val _bid
              return (b,r,e)
          case book of
            Just (Entity _ (Book _ _ _ day time addr tzo tz _ _),role',empl') -> do
                runDB $ update $ \x -> do
                    set x [BookStatus =. val BookStatusPaid]
                    where_ $ x ^. BookId ==. val bid
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusPaid
                    (roleName . entityVal <$> role') (staffName . entityVal <$> empl')
                redirectUltDest $ RequestR uid eid bid

            Nothing -> do
                addMessageI warn MsgEntityNotFound
                redirectUltDest $ RequestR uid eid bid

      FormFailure _ -> do
          addMessageI warn MsgInvalidFormData
          redirectUltDest $ RequestR uid eid bid
      FormMissing -> do
          addMessageI warn MsgMissingForm
          redirectUltDest $ RequestR uid eid bid


postRequestApproveR :: UserId -> StaffId -> BookId -> Handler Html
postRequestApproveR uid eid bid = do
    ((fr,_),_) <- runFormPost $ formApprove Nothing
    case fr of
      FormSuccess _bid -> do
          book <- runDB $ selectOne $ do
              x :& o :& r :& e <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `leftJoin` table @Role `on` (\(b :& _ :& r) -> b ^. BookRole ==. r ?. RoleId)
                  `leftJoin` table @Staff `on` (\(_ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
              where_ $ x ^. BookId ==. val _bid
              return ((x,o),(r,e))
          role <- runDB $ selectOne $ do
              r :& _ :& u :& s <- from $ table @Role
                  `innerJoin` table @Staff `on` (\(r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
                  `innerJoin` table @User `on` (\(_ :& e :& u) -> e ^. StaffUser ==. just (u ^. UserId))
                  `innerJoin` table @Service `on` (\(r :& _ :& _ :& s) -> r ^. RoleService ==. s ^. ServiceId)
              where_ $ u ^. UserId ==. val uid
              where_ $ case book of
                Just ((_,Entity _ (Offer sid _ _ _ _ _ _ _)),_) -> s ^. ServiceId ==. val sid
                Nothing -> val False
              return r

          case (book,role) of
            (Just ((Entity _ (Book _ _ _ day time addr tzo tz _ _),_),(role',empl')),Just (Entity rid _)) -> do
                runDB $ update $ \x -> do
                    set x [BookRole =. just (val rid), BookStatus =. val BookStatusApproved]
                    where_ $ x ^. BookId ==. val bid
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusApproved
                    (roleName . entityVal <$> role') (staffName . entityVal <$> empl')
                redirectUltDest $ RequestR uid eid bid

            (Nothing,_) -> do
                addMessageI warn MsgEntityNotFound
                redirectUltDest $ RequestR uid eid bid
            (_,Nothing) -> do
                addMessageI warn MsgNonexpert
                redirectUltDest $ RequestR uid eid bid

      FormFailure _ -> do
          addMessageI warn MsgInvalidFormData
          redirectUltDest $ RequestR uid eid bid
      FormMissing -> do
          addMessageI warn MsgMissingForm
          redirectUltDest $ RequestR uid eid bid


getRequestsSearchR :: UserId -> StaffId -> Handler Html
getRequestsSearchR uid eid = do
    setUltDestCurrent
    msgs <- getMessages
    formSearch <- newIdent
    dlgStatusList <- newIdent
    q <- runInputGet $ iopt (searchField True) "q"
    stati <- filter ((== "status") . fst) . reqGetParams <$> getRequest
    let states = mapMaybe (readMaybe . unpack . snd) stati
    owners <- filter ((== "assignee") . fst) . reqGetParams <$> getRequest
    let assignees = mapMaybe (readMaybe . unpack . snd) owners
    dlgAssignee <- newIdent
    requests <- runDB $ select $ do
        x :& o :& s :& r :& e :& c <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Role `on` (\(x :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
            `leftJoin` table @User `on` (\(_ :& _ :& _ :& _ :& e :& c) -> e ?. StaffUser ==. just (c ?. UserId))

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

        let ors = [ ( AssigneesMe `elem` assignees
                    , e ?. StaffUser ==. just (just (val uid))
                    )
                  , ( AssigneesNone `elem` assignees
                    , isNothing_ $ x ^. BookRole
                    )
                  , ( AssigneesOthers `elem` assignees
                    , not_ (isNothing_ $ e ?. StaffUser) &&. not_ (e ?. StaffUser ==. just (just (val uid)))
                    )
                  ]

        case snd <$> filter fst ors of
          [] -> return ()
          xs -> where_ $ foldr (||.) (val False) xs

        orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
        return (x,s)
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "requests/search/search")


postRequestR :: UserId -> StaffId -> BookId -> Handler Html
postRequestR uid eid bid = do
    book <- runDB $ selectOne $ do
        b :& o :& r :& e <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `leftJoin` table @Role `on` (\(b :& _ :& r) -> b ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
        where_ $ b ^. BookId ==. val bid
        return ((b,o),(r,e))
    ((fr,fw),et) <- runFormPost $ formReschedule (fst . fst <$> book)
    case fr of
      FormSuccess (day,time,_,_,_) -> do
          role <- runDB $ selectOne $ do
              r :& _ :& u :& s <- from $ table @Role
                  `innerJoin` table @Staff `on` (\(r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
                  `innerJoin` table @User `on` (\(_ :& e :& u) -> e ^. StaffUser ==. just (u ^. UserId))
                  `innerJoin` table @Service `on` (\(r :& _ :& _ :& s) -> r ^. RoleService ==. s ^. ServiceId)
              where_ $ u ^. UserId ==. val uid
              where_ $ case book of
                Just ((_,Entity _ (Offer sid _ _ _ _ _ _ _)),(_,_)) -> s ^. ServiceId ==. val sid
                Nothing -> val False
              return r

          case (book,role) of
            (Just ( ( Entity _ (Book _ _ _ _ _ addr tzo tz pm _) ,_ ),(role',empl')), Just (Entity rid _) ) -> do
                runDB $ update $ \x -> do
                    set x [ BookRole =. just (val rid)
                          , BookDay =. val day
                          , BookTime =. val time
                          , BookAddr =. val addr
                          , BookTzo =. val tzo
                          , BookTz =. val tz
                          , BookPayMethod =. val pm
                          , BookStatus =. val BookStatusAdjusted
                          ]
                    where_ $ x ^. BookId ==. val bid
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusAdjusted
                    (roleName . entityVal <$> role') (staffName . entityVal <$> empl')
                redirectUltDest $ RequestR uid eid bid
            (Nothing,_) -> do
                addMessageI warn MsgEntityNotFound
                redirectUltDest $ RequestR uid eid bid

            (_,Nothing) -> do
                addMessageI warn MsgNonexpert
                redirectUltDest $ RequestR uid eid bid

      FormMissing -> do
          addMessageI warn MsgMissingForm
          redirectUltDest $ RequestR uid eid bid
      FormFailure _ -> defaultLayout $ do
          setTitleI MsgReschedule
          $(widgetFile "requests/reschedule/reschedule")


getRequestR :: UserId -> StaffId -> BookId -> Handler Html
getRequestR uid eid bid = do
    stati <- reqGetParams <$> getRequest

    currency <- (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Business
        return $ x ^. BusinessCurrency )

    app <- getYesod
    langs <- languages
    business <- runDB $ selectOne $ from $ table @Business
    location <- runDB $ selectOne $ from $ table @ContactUs

    address <- case location of
      Just (Entity _ (ContactUs _ _ True _ _ _ _)) -> do
         (unValue <$>) <$> runDB ( selectOne $ do
                x <- from $ table @Business
                return (x ^. BusinessAddr) )
      _ -> return Nothing

    request <- runDB $ selectOne $ do
        x :& o :& s :& t :& r :& e :& c <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& _ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
            `leftJoin` table @Role `on` (\(x :& _ :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
            `innerJoin` table @User `on` (\(x :& _ :& _ :& _ :& _ :& _ :& c) -> x ^. BookCustomer ==. c ^. UserId)
        where_ $ x ^. BookId ==. val bid
        return (x,(o,s,t,r,e,c))
    msgs <- getMessages
    dlgConfirmApprove <- newIdent
    formRequestApprove <- newIdent
    dlgConfirmFinish <- newIdent
    formAppoitmentFinish <- newIdent
    formAssignToMe <- newIdent
    (fwa,eta) <- generateFormPost formAssign
    (fw,et) <- generateFormPost $ formApprove (fst <$> request)
    dlgAssignToMeConfirm <- newIdent
    detailsLocation <- newIdent
    locationHtmlContainer <- newIdent
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgRequest
        case location of
          Just (Entity _ (ContactUs _ _ _ _ True (Just lng) (Just lat))) -> do
              mapboxPk <- appMapboxPk . appSettings <$> getYesod
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-language/v1.0.0/mapbox-gl-language.js"
              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css"
              toWidget [julius|
const main = document.getElementById(#{detailsLocation});
const mapgl = document.createElement('div');
mapgl.style.height = '300px';
mapgl.style.width = '100%';
main.appendChild(mapgl);
const map = new mapboxgl.Map({
  accessToken: #{mapboxPk},
  attributionControl: false,
  container: mapgl,
  style: 'mapbox://styles/mapbox/streets-v11',
  center: [#{rawJS $ show lng}, #{rawJS $ show lat}],
  zoom: 15
});
map.addControl(new MapboxLanguage());
map.addControl(new mapboxgl.NavigationControl());
const loc = new mapboxgl.Marker().setLngLat(
  [#{rawJS $ show lng}, #{rawJS $ show lat}]
).addTo(map);
|]
          Just (Entity _ (ContactUs _ _ _ _ True _ _)) -> do
              mapboxPk <- appMapboxPk . appSettings <$> getYesod
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-language/v1.0.0/mapbox-gl-language.js"
              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css"
              toWidget [julius|
const main = document.getElementById(#{detailsLocation});
const mapgl = document.createElement('div');
mapgl.style.height = '300px';
mapgl.style.width = '100%';
main.appendChild(mapgl);
const map = new mapboxgl.Map({
  accessToken: #{mapboxPk},
  attributionControl: false,
  container: mapgl,
  style: 'mapbox://styles/mapbox/streets-v11',
  center: [0, 0],
  zoom: 0
});
map.addControl(new MapboxLanguage());
map.addControl(new mapboxgl.NavigationControl());
|]
          _ -> return ()
        $(widgetFile "requests/request")


formAssign :: Html -> MForm Handler (FormResult (), Widget)
formAssign extra = return (FormSuccess (),[whamlet|#{extra}|])


formApprove :: Maybe (Entity Book) -> Html -> MForm Handler (FormResult BookId, Widget)
formApprove book extra = do
    (r,v) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgBook
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (fromIntegral . fromSqlKey . entityKey <$> book)
    return (toSqlKey <$> r,[whamlet|#{extra}^{fvInput v}|])


getRequestsR :: UserId -> StaffId -> Handler Html
getRequestsR uid eid = do
    mbid <- (toSqlKey <$>) <$> runInputGet (iopt intField "bid")
    stati <- reqGetParams <$> getRequest
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    let statuses = mapMaybe (readMaybe . unpack . snd) . filter ((== status) . fst) $ stati
    owners <- filter ((== assignee) . fst) . reqGetParams <$> getRequest
    let assignees = mapMaybe (readMaybe . unpack . snd) owners
    sort <- fromMaybe SortOrderDesc . (readMaybe . unpack =<<) <$> runInputGet (iopt textField "sort")
    requests <- runDB $ select $ do
        x :& _ :& s :& _ :& e <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Role `on` (\(x :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)

        case statuses of
          [] -> return ()
          xs -> where_ $ x ^. BookStatus `in_` valList xs

        let ors = [ ( AssigneesMe `elem` assignees
                    , e ?. StaffUser ==. just (just (val uid))
                    )
                  , ( AssigneesNone `elem` assignees
                    , isNothing_ $ x ^. BookRole
                    )
                  , ( AssigneesOthers `elem` assignees
                    , not_ (isNothing_ $ e ?. StaffUser) &&. not_ (e ?. StaffUser ==. just (just (val uid)))
                    )
                  ]

        case snd <$> filter fst ors of
          [] -> return ()
          xs -> where_ $ foldr (||.) (val False) xs

        orderBy $ case sort of
          SortOrderAsc -> [asc (x ^. BookDay), asc (x ^. BookTime)]
          _ -> [desc (x ^. BookDay), desc (x ^. BookTime)]

        return (x,s)
    curr <- getCurrentRoute
    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    formQuery <- newIdent
    toolbarTop <- newIdent
    buttonSort <- newIdent
    dlgAssignee <- newIdent
    defaultLayout $ do
        setTitleI MsgRequests
        $(widgetFile "requests/requests")
        
  where
      status, assignee :: Text
      status   = "status"
      assignee = "assignee"


statusList :: [(BookStatus, AppMessage)]
statusList = [ (BookStatusRequest, MsgRequest)
             , (BookStatusApproved, MsgApproved)
             , (BookStatusCancelled, MsgCancelled)
             , (BookStatusPaid, MsgPaid)
             ]

assigneeList :: [(Assignees, AppMessage)]
assigneeList = [ (AssigneesMe,MsgAssignedToMe)
               , (AssigneesNone,MsgWithoutAssignee)
               , (AssigneesOthers,MsgFromCoworkers)
               ]

info :: Text
info = "info"

warn :: Text
warn = "warn"
