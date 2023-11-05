{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
  ) where

import Control.Monad (unless)
import Data.Maybe (mapMaybe, isJust, isNothing)
import Data.Text (intercalate, unpack, Text, pack)
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
    ( TimeZone (timeZoneMinutes), TimeOfDay, LocalTime (LocalTime)
    , minutesToTimeZone, utcToLocalTime
    )
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (renderMessage, SomeMessage (SomeMessage))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages
    , getYesod, languages, preEscapedToMarkup, whamlet, getRequest
    , YesodRequest (reqGetParams), newIdent, MonadIO (liftIO), redirect
    , addMessageI, getUrlRender
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth ( Route(LoginR), maybeAuth )
import Yesod.Form.Fields
    ( Textarea(unTextarea, Textarea), searchField, intField, timeField
    , dayField, textField, textareaField
    )
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess, FormFailure, FormMissing)
    , FieldSettings (FieldSettings, fsLabel, fsId, fsTooltip, fsName, fsAttrs)
    , FieldView (fvInput, fvErrors, fvLabel, fvId)
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq, checkM)
import Yesod.Form.Input (runInputGet, iopt)
import Settings (widgetFile)

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR, RequestsR, RequestR
      , AdminR, ServiceThumbnailR, RequestsSearchR, RequestsSearchR
      , RequestApproveR, RequestFinishR, RequestRescheduleR, RequestHistR
      , RequestAssignR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgRequests, MsgPhoto, MsgLogin, MsgSymbolHour, MsgBack
      , MsgSymbolMinute, MsgApproved, MsgCancelled, MsgPaid, MsgAssignee
      , MsgLoginToSeeTheRequests, MsgRequest, MsgStatus, MsgMeetingLocation
      , MsgPleaseConfirm, MsgHistory, MsgReschedule, MsgService, MsgMeetingTime
      , MsgCustomer, MsgSelect, MsgCancel, MsgAcquaintance, MsgAppoinmentStatus
      , MsgNoPendingRequestsYet, MsgShowAll, MsgAssignedToMe, MsgWithoutAssignee
      , MsgSearch, MsgNoRequestsFound, MsgFromCoworkers, MsgApproveAppointmentConfirm
      , MsgDate, MsgTime, MsgLocation, MsgEntityNotFound, MsgInvalidFormData
      , MsgMissingForm, MsgLoginToPerformAction, MsgBook, MsgRequestFinish, MsgDuration
      , MsgFinishAppointmentConfirm, MsgDay, MsgTimeZone, MsgMinutes, MsgSave
      , MsgAppointmentTimeIsInThePast, MsgAppointmentDayIsInThePast, MsgNoHistoryYet
      , MsgLoginBanner, MsgNoAssigneeRequestApprove, MsgTimeZoneOffset, MsgNavigationMenu
      , MsgYouAreNotAnEmployeeOfFacility, MsgOnlyEmployeesMaySeeRequests, MsgUserProfile
      , MsgNonexpert, MsgAssignToMe, MsgAreYouSureYouWantToTakeOnThisTask, MsgApprove
      , MsgUnassigned
      )
    )

import Database.Persist (Entity (Entity, entityKey, entityVal), PersistStoreWrite (insert_))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, innerJoin, leftJoin, on, where_, val
    , (:&)((:&)), (==.), (^.), (?.), (%), (++.), (||.), (&&.), (=.)
    , orderBy, desc, just, selectOne, valList, in_, upper_, like, isNothing_
    , not_, update, set, exists, unValue
    )

import Model
    ( Book (Book, bookDay, bookTz, bookTzo, bookTime, bookAddr)
    , BookId, Offer, ServiceId, Service (Service), Role (Role, roleName)
    , Staff (Staff, staffName), Offer (Offer), Thumbnail (Thumbnail)
    , User (User), Contents (Contents)
    , BookStatus
      ( BookStatusRequest, BookStatusApproved, BookStatusCancelled
      , BookStatusPaid, BookStatusAdjusted
      )
    , Assignees (AssigneesMe, AssigneesNone, AssigneesOthers)
    , Business (Business), Hist (Hist)
    , EntityField
      ( BookOffer, OfferId, OfferService, ServiceId, BookDay, BookTime
      , BookRole, RoleId, RoleStaff, StaffId, StaffUser, ContentsSection
      , BookId, ThumbnailService, BookCustomer, UserId, BookStatus, ServiceName
      , ServiceDescr, ServiceOverview, RoleName, OfferName, OfferPrefix
      , OfferSuffix, OfferDescr, StaffName, StaffPhone, StaffMobile, StaffEmail
      , UserName, UserFullName, UserEmail, BookTz, HistBook, HistLogtime
      , RoleService, HistUser, BookTzo, BookAddr, BusinessCurrency
      )
    )

import Menu (menu)
import Handler.Appointments (resolveBookStatus)
import Handler.Contacts (section)


postRequestAssignR :: BookId -> ServiceId -> Handler Html
postRequestAssignR bid sid = do
    user <- maybeAuth
    book <- runDB $ selectOne $ do
        b <- from $ table @Book
        where_ $ b ^. BookId ==. val bid
        return b
    case (user,book) of
      (Just (Entity uid _), Just (Entity bid' (Book _ _ _ day' time' addr' tzo' tz' status'))) -> do

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
                redirect $ RequestR bid
            Nothing -> do
                addMessageI "warn" MsgNonexpert
                redirect $ RequestR bid

      (Nothing, _) -> do
          addMessageI "warn" MsgLoginToPerformAction
          redirect $ RequestR bid
      (_, Nothing) -> do
          addMessageI "warn" MsgEntityNotFound
          redirect $ RequestR bid


getRequestHistR :: BookId -> Handler Html
getRequestHistR bid = do
    user <- maybeAuth
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ case user of
          Nothing -> val False
          Just (Entity uid _) -> x ^. StaffUser ==. just (val uid)
        return x
    unless (isJust empl) $ redirect RequestsR
    case user of
      Just _ -> do
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
      Nothing -> do
          addMessageI "warn" MsgLoginToPerformAction
          redirect $ RequestR bid


getRequestRescheduleR :: BookId -> Handler Html
getRequestRescheduleR bid = do
    user <- maybeAuth
    unless (isJust user) $ redirect RequestsR
    empls <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ case user of
          Nothing -> val False
          Just (Entity uid _) -> x ^. StaffUser ==. just (val uid)
    unless (isJust empls) $ redirect RequestsR
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


postRequestFinishR :: BookId -> Handler Html
postRequestFinishR bid = do
    ((fr,_),_) <- runFormPost $ formApprove Nothing
    user <- maybeAuth
    case (fr, user) of
      (FormSuccess _bid, Just (Entity uid _)) -> do
          book <- runDB $ selectOne $ do
              b :& r :& e <- from $ table @Book
                  `leftJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. r ?. RoleId)
                  `leftJoin` table @Staff `on` (\(_ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
              where_ $ b ^. BookId ==. val _bid
              return (b,r,e)
          case book of
            Just (Entity _ (Book _ _ _ day time addr tzo tz _),role',empl') -> do
                runDB $ update $ \x -> do
                    set x [BookStatus =. val BookStatusPaid]
                    where_ $ x ^. BookId ==. val bid
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusPaid
                    (roleName . entityVal <$> role') (staffName . entityVal <$> empl')
                redirect $ RequestR bid

            Nothing -> do
                addMessageI "warn" MsgEntityNotFound
                redirect $ RequestR bid

      (FormSuccess _, Nothing) -> do
          addMessageI "warn" MsgLoginToPerformAction
          redirect $ RequestR bid
      (FormFailure _, _) -> do
          addMessageI "warn" MsgInvalidFormData
          redirect $ RequestR bid
      (FormMissing, _) -> do
          addMessageI "warn" MsgMissingForm
          redirect $ RequestR bid


postRequestApproveR :: BookId -> Handler Html
postRequestApproveR bid = do
    ((fr,_),_) <- runFormPost $ formApprove Nothing
    user <- maybeAuth
    case (fr, user) of
      (FormSuccess _bid, Just (Entity uid _)) -> do
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
                Just ((_,Entity _ (Offer sid _ _ _ _ _)),_) -> s ^. ServiceId ==. val sid
                Nothing -> val False
              return r

          case (book,role) of
            (Just ((Entity _ (Book _ _ _ day time addr tzo tz _),_),(role',empl')),Just (Entity rid _)) -> do
                runDB $ update $ \x -> do
                    set x [BookRole =. just (val rid), BookStatus =. val BookStatusApproved]
                    where_ $ x ^. BookId ==. val bid
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusApproved
                    (roleName . entityVal <$> role') (staffName . entityVal <$> empl')
                redirect $ RequestR bid

            (Nothing,_) -> do
                addMessageI "warn" MsgEntityNotFound
                redirect $ RequestR bid
            (_,Nothing) -> do
                addMessageI "warn" MsgNonexpert
                redirect $ RequestR bid

      (FormSuccess _, Nothing) -> do
          addMessageI "warn" MsgLoginToPerformAction
          redirect $ RequestR bid
      (FormFailure _, _) -> do
          addMessageI "warn" MsgInvalidFormData
          redirect $ RequestR bid
      (FormMissing, _) -> do
          addMessageI "warn" MsgMissingForm
          redirect $ RequestR bid


getRequestsSearchR :: Handler Html
getRequestsSearchR = do
    user <- maybeAuth
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ case user of
          Nothing -> val False
          Just (Entity uid _) -> x ^. StaffUser ==. just (val uid)
    unless (isJust empl) $ redirect RequestsR
    setUltDestCurrent
    msgs <- getMessages
    case user of
      Nothing -> defaultLayout $ do
          setTitleI MsgLogin
          $(widgetFile "requests/login")
      Just (Entity uid _) -> do
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
                          , isNothing_ $ e ?. StaffUser
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


postRequestR :: BookId -> Handler Html
postRequestR bid = do
    user <- maybeAuth
    book <- runDB $ selectOne $ do
        b :& o :& r :& e <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `leftJoin` table @Role `on` (\(b :& _ :& r) -> b ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
        where_ $ b ^. BookId ==. val bid
        return ((b,o),(r,e))
    ((fr,fw),et) <- runFormPost $ formReschedule (fst . fst <$> book)
    case (fr,user) of
      (FormSuccess (day,time,_,_,_), Just (Entity uid _)) -> do
          role <- runDB $ selectOne $ do
              r :& _ :& u :& s <- from $ table @Role
                  `innerJoin` table @Staff `on` (\(r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
                  `innerJoin` table @User `on` (\(_ :& e :& u) -> e ^. StaffUser ==. just (u ^. UserId))
                  `innerJoin` table @Service `on` (\(r :& _ :& _ :& s) -> r ^. RoleService ==. s ^. ServiceId)
              where_ $ u ^. UserId ==. val uid
              where_ $ case book of
                Just ((_,Entity _ (Offer sid _ _ _ _ _)),(_,_)) -> s ^. ServiceId ==. val sid
                Nothing -> val False
              return r

          case (book,role) of
            (Just ( ( Entity _ (Book _ _ _ _ _ addr tzo tz _) ,_ ),(role',empl')), Just (Entity rid _) ) -> do
                runDB $ update $ \x -> do
                    set x [ BookRole =. just (val rid)
                          , BookDay =. val day
                          , BookTime =. val time
                          , BookAddr =. val addr
                          , BookTzo =. val tzo
                          , BookTz =. val tz
                          , BookStatus =. val BookStatusAdjusted
                          ]
                    where_ $ x ^. BookId ==. val bid
                now <- liftIO getCurrentTime
                runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusAdjusted
                    (roleName . entityVal <$> role') (staffName . entityVal <$> empl')
                redirect $ RequestR bid
            (Nothing,_) -> do
                addMessageI "warn" MsgEntityNotFound
                redirect $ RequestR bid

            (_,Nothing) -> do
                addMessageI "warn" MsgNonexpert
                redirect $ RequestR bid

      (FormSuccess _, Nothing) -> do
          app <- getYesod
          langs <- languages
          rndr <- getUrlRender
          setUltDestCurrent
          defaultLayout $ do
              setTitleI MsgReschedule
              $(widgetFile "requests/reschedule/banner")
      (FormMissing, _) -> do
          addMessageI "warn" MsgMissingForm
          redirect $ RequestR bid
      (FormFailure _, _) -> defaultLayout $ do
          setTitleI MsgReschedule
          $(widgetFile "requests/reschedule/reschedule")


getRequestR :: BookId -> Handler Html
getRequestR bid = do
    stati <- reqGetParams <$> getRequest
    user <- maybeAuth
    unless (isJust user) $ redirect (RequestsR, stati)

    currency <- (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Business
        return $ x ^. BusinessCurrency )
    
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ case user of
          Nothing -> val False
          Just (Entity uid _) -> x ^. StaffUser ==. just (val uid)
    unless (isJust empl) $ redirect (RequestsR, stati)
    app <- getYesod
    langs <- languages
    location <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    business <- runDB $ selectOne $ from $ table @Business
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
    defaultLayout $ do
        setTitleI MsgRequest
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


getRequestsR :: Handler Html
getRequestsR = do
    mbid <- (toSqlKey <$>) <$> runInputGet (iopt intField "bid")
    stati <- filter ((== "status") . fst) . reqGetParams <$> getRequest
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    case user of
      Nothing -> defaultLayout $ do
          setTitleI MsgLogin
          $(widgetFile "requests/login")
      Just (Entity uid _) -> do
          formSearch <- newIdent
          dlgStatusList <- newIdent
          let states = mapMaybe (readMaybe . unpack . snd) stati
          owners <- filter ((== "assignee") . fst) . reqGetParams <$> getRequest
          let assignees = mapMaybe (readMaybe . unpack . snd) owners
          dlgAssignee <- newIdent
          empl <- runDB $ selectOne $ do
              x <- from $ table @Staff
              where_ $ x ^. StaffUser ==. just (val uid)
              return x
          requests <- case empl of
            Nothing -> return []
            Just _ -> runDB $ select $ do
                x :& _ :& s :& _ :& e <- from $ table @Book
                    `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                    `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
                    `leftJoin` table @Role `on` (\(x :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
                    `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)

                case states of
                  [] -> return ()
                  xs -> where_ $ x ^. BookStatus `in_` valList xs

                let ors = [ ( AssigneesMe `elem` assignees
                            , e ?. StaffUser ==. just (just (val uid))
                            )
                          , ( AssigneesNone `elem` assignees
                            , isNothing_ $ e ?. StaffUser
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
              setTitleI MsgRequests
              $(widgetFile "requests/requests")


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
