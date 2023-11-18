{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}

module Admin.Staff
  ( getAdmStaffR
  , getAdmStaffCreateR
  , getAdmStaffPhotoR
  , getAdmEmplR
  , postAdmEmplR
  , getAdmStaffEditR
  , postAdmStaffR
  , postAdmStaffDeleteR
  , getAdmRolesR
  , postAdmRolesR
  , getAdmRoleR
  , postAdmRoleR
  , getAdmRoleCreateR
  , getAdmRoleEditR
  , postAdmRoleDeleteR
  , getAdmEmplUserR
  , postAdmEmplUserR
  , postAdmEmplUnregR
  , getAdmStaffSearchR
  , getAdmScheduleCreateR
  , getAdmScheduleR
  , postAdmScheduleR
  , getAdmTimeSlotR
  , postAdmTimeSlotR
  , getAdmScheduleEditR
  , postAdmScheduleDeleteR
  , getAdmEmplCalendarR
  , getEmplCalendarSlotR
  , getEmplCalendarSlotEditR
  , postEmplCalendarSlotDeleteR
  , getEmplCalendarSlotCreateR
  , postEmplCalendarSlotsR
  , postEmplCalendarSlotR
  , getEmplCalendarSlotsR
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (Bifunctor(first))
import Data.Fixed (mod')
import qualified Data.Map.Lazy as M (lookup, fromListWith)
import Data.Text (Text, pack, unpack, intercalate)
import qualified Data.Text as T (toLower, words, concat)
import Text.Shakespeare.I18N (renderMessage)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar
    ( Day, DayOfWeek (Monday), toGregorian, weekFirstDay, addDays
    , periodFirstDay, periodLastDay
    )
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.Clock
    ( utctDay, getCurrentTime, secondsToNominalDiffTime, NominalDiffTime
    , DiffTime
    )
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (TimeOfDay, LocalTime (LocalTime), diffLocalTime)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Data.FileEmbed (embedFile)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad (forM)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages
    , TypedContent (TypedContent), ToContent (toContent)
    , typeSvg, addMessageI, redirect, FileInfo (fileContentType)
    , SomeMessage (SomeMessage), fileSourceByteString
    , MonadTrans (lift), whamlet, getRequest
    , YesodRequest (reqGetParams), newIdent
    )
import Yesod.Core.Handler (getCurrentRoute, getYesod, languages)
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth (maybeAuth, Route (LoginR))

import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), Field
    , FieldView (fvInput, fvLabel, fvId, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsName, fsAttrs, fsId)
    , Field (Field, fieldParse, fieldView, fieldEnctype), Enctype (UrlEncoded)
    )
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields
    ( textField, emailField, passwordField, intField, hiddenField
    , fileField, checkBoxField, dayField, timeField, FormMessage (MsgInvalidEntry)
    )
import Yesod.Form.Functions
    ( mreq, mopt, generateFormPost, runFormPost, check, checkM, checkBool
    , parseHelper
    )
import Settings (widgetFile)

import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (replace, insert, insert_, delete)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Yesod.Persist (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, asc, val, between
    , (^.), (==.), (:&)((:&)), (?.), (=.), (%), (++.), (||.)
    , where_, not_, selectQuery, subSelectList, in_
    , just, notIn, isNothing_, innerJoin, on, desc
    , leftJoin, update, set, like, upper_, distinct, valList
    , exists, Value (Value, unValue), justList
    )

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR, StaticR, ProfileR
      )
    , AdminR
      ( AdmStaffDeleteR, AdmStaffEditR, AdmEmplR, AdmStaffR, AdmRoleR
      , AdmStaffCreateR, AdmStaffPhotoR, AdmRolesR, AdmRoleCreateR
      , AdmRoleEditR, AdmRoleDeleteR, AdmEmplUserR, AdmEmplUnregR
      , AdmStaffSearchR, AdmScheduleR, AdmTimeSlotR, AdmScheduleCreateR
      , AdmScheduleEditR, AdmScheduleDeleteR, AdmEmplCalendarR
      , EmplCalendarSlotR, EmplCalendarSlotEditR, EmplCalendarSlotDeleteR
      , EmplCalendarSlotCreateR, AdmEmplCalendarR, EmplCalendarSlotsR
      )
    , AppMessage
      ( MsgStaff, MsgPhoto, MsgCancel, MsgSave, MsgBack, MsgAddWorkingHours
      , MsgNoStaffYet, MsgEmployee, MsgRecordEdited, MsgName, MsgWorkSchedule
      , MsgRole, MsgPhone, MsgMobile, MsgEmail, MsgRecordAdded, MsgNoScheduleYet
      , MsgEmployeeAlreadyInTheList, MsgDeleteAreYouSure, MsgYesDelete, MsgList
      , MsgPleaseConfirm, MsgRecordDeleted, MsgRoles, MsgNoRolesYet, MsgCalendar
      , MsgAddRole, MsgService, MsgTheName, MsgRating, MsgRoleAlreadyInTheList
      , MsgRegisterAsUser, MsgUser, MsgRegistration, MsgUsername, MsgPassword
      , MsgFullName, MsgAlreadyExists, MsgUnregisterAreYouSure, MsgSearch
      , MsgNoStaffMembersFound, MsgStatus, MsgSelect, MsgRatings, MsgEdit
      , MsgUnavailable, MsgAvailable, MsgAccountStatus, MsgRegistered, MsgDel
      , MsgUnregistered, MsgValueNotInRange, MsgAdministrator, MsgUnregister
      , MsgNavigationMenu, MsgUserProfile, MsgLogin, MsgUnregisterAsUser
      , MsgWorkingHours, MsgDay, MsgStartTime, MsgEndTime, MsgDetails, MsgToday
      , MsgInvalidTimeInterval, MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat
      , MsgSun, MsgSymbolHour, MsgSymbolMinute, MsgInvalidFormData, MsgAdd
      , MsgCompletionTime, MsgWorkday, MsgSortAscending, MsgSortDescending
      , MsgPatternHourMinute, MsgAnalyst
      )
    )

import Model
    ( StaffId, Staff (Staff, staffName, staffPhone, staffMobile, staffEmail, staffStatus)
    , ScheduleId, Schedule (Schedule, scheduleWorkDay, scheduleWorkStart, scheduleWorkEnd)
    , EntityField
      ( StaffId, StaffPhotoStaff, StaffPhotoMime, StaffPhotoPhoto, ScheduleStaff
      , StaffName, RoleStaff, RoleId, ServiceId, ServiceGroup
      , RoleService, RoleName, RoleRating, StaffUser, UserId, UserName
      , StaffPhone, StaffMobile, StaffEmail, StaffStatus, ScheduleId
      , ScheduleWorkDay, ScheduleWorkStart, ScheduleWorkEnd
      )
    , StaffPhoto (StaffPhoto, staffPhotoPhoto, staffPhotoMime, staffPhotoStaff)
    , Role (Role, roleService, roleName, roleDuration, roleRating), RoleId
    , ServiceId, Service (Service)
    , UserId,  User (User), UserPhoto (UserPhoto)
    , EmplStatus (EmplStatusUnavailable, EmplStatusAvailable)
    , SortOrder (SortOrderAsc, SortOrderDesc)
    )

import Settings.StaticFiles (img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg)
import Menu (menu)


getEmplCalendarSlotsR :: StaffId -> Day -> Handler Html
getEmplCalendarSlotsR eid day = do
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian
    slots <- runDB $ select $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleStaff ==. val eid
        where_ $ x ^. ScheduleWorkDay ==. val day
        return x
    fabSlotCreate <- newIdent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkday
        $(widgetFile "admin/staff/empl/calendar/slots/slots")


postEmplCalendarSlotDeleteR :: StaffId -> ScheduleId -> Day -> Handler Html
postEmplCalendarSlotDeleteR eid wid day = do
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian
    ((fr,_),_) <- runFormPost formSlotDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete wid
          addMessageI "info" MsgRecordDeleted
          redirect $ AdminR $ AdmEmplCalendarR eid (month day)
      _ -> do
          addMessageI "warn" MsgInvalidFormData
          redirect $ AdminR $ EmplCalendarSlotR eid wid day


postEmplCalendarSlotR :: StaffId -> ScheduleId -> Day -> Handler Html
postEmplCalendarSlotR eid wid day = do
    ((fr,fw),et) <- runFormPost $ formSlot eid day Nothing
    case fr of
      FormSuccess r -> do
          runDB $ replace wid r
          addMessageI "info" MsgRecordEdited
          redirect $ AdminR $ EmplCalendarSlotR eid wid day
      _ -> defaultLayout $ do
          timeDay <- newIdent
          setTitleI MsgWorkingHours
          $(widgetFile "admin/staff/empl/calendar/slots/edit")


getEmplCalendarSlotEditR :: StaffId -> ScheduleId -> Day -> Handler Html
getEmplCalendarSlotEditR eid wid day = do
    slot <- runDB $ selectOne $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleId ==. val wid
        return x
    (fw,et) <- generateFormPost $ formSlot eid day slot
    defaultLayout $ do
        timeDay <- newIdent
        setTitleI MsgWorkingHours
        $(widgetFile "admin/staff/empl/calendar/slots/edit")


postEmplCalendarSlotsR :: StaffId -> Day -> Handler Html
postEmplCalendarSlotsR eid day = do
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian
    ((fr,fw),et) <- runFormPost $ formSlot eid day Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR $ EmplCalendarSlotsR eid day
      _ -> defaultLayout $ do
          timeDay <- newIdent
          setTitleI MsgWorkingHours
          $(widgetFile "admin/staff/empl/calendar/slots/create")


getEmplCalendarSlotCreateR :: StaffId -> Day -> Handler Html
getEmplCalendarSlotCreateR eid day = do
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian
    (fw,et) <- generateFormPost $ formSlot eid day Nothing
    defaultLayout $ do
        timeDay <- newIdent
        setTitleI MsgWorkingHours
        $(widgetFile "admin/staff/empl/calendar/slots/create")


formSlot :: StaffId -> Day -> Maybe (Entity Schedule)
         -> Html -> MForm Handler (FormResult Schedule,Widget)
formSlot eid day slot extra = do
    (startR,startV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgStartTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (scheduleWorkStart . entityVal <$> slot)
    (endR,endV) <- mreq (afterTimeField startR) FieldSettings
        { fsLabel = SomeMessage MsgEndTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (scheduleWorkEnd . entityVal <$> slot)
    let r = Schedule eid day <$> startR <*> endR
    let w = [whamlet|
#{extra}
$forall (v,icon) <- [(startV,"schedule"),(endV,"schedule")]
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
|]
    return (r,w)
  where

      afterTimeField :: FormResult TimeOfDay -> Field Handler TimeOfDay
      afterTimeField startR = check (afterTime startR) timeField

      afterTime :: FormResult TimeOfDay -> TimeOfDay -> Either AppMessage TimeOfDay
      afterTime startR x = case startR of
          FormSuccess s | x > s -> Right x
                        | otherwise -> Left MsgInvalidTimeInterval
          _ -> Right x



getEmplCalendarSlotR :: StaffId -> ScheduleId -> Day -> Handler Html
getEmplCalendarSlotR eid wid day = do
    slot <- runDB $ selectOne $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleId ==. val wid
        return x
    msgs <- getMessages
    dlgSlotDelete <- newIdent
    (fw,et) <- generateFormPost formSlotDelete
    defaultLayout $ do
        setTitleI MsgWorkingHours
        $(widgetFile "admin/staff/empl/calendar/slots/slot")


formSlotDelete :: Html -> MForm Handler (FormResult (),Widget)
formSlotDelete extra = return (FormSuccess (),[whamlet|#{extra}|])


postAdmScheduleDeleteR :: StaffId -> ScheduleId -> Handler Html
postAdmScheduleDeleteR eid wid = do
    stati <- reqGetParams <$> getRequest
    ((fr,_),_) <- runFormPost formSlotDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete wid
          addMessageI "info" MsgRecordDeleted
          redirect (AdminR $ AdmScheduleR eid,stati)
      _ -> do
          addMessageI "info" MsgInvalidFormData
          redirect (AdminR $ AdmTimeSlotR eid wid)



postAdmTimeSlotR :: StaffId -> ScheduleId -> Handler Html
postAdmTimeSlotR eid wid = do
    stati <- reqGetParams <$> getRequest
    ((fr,fw),et) <- runFormPost $ formSchedule eid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ replace wid r
          addMessageI "info" MsgRecordEdited
          redirect (AdminR $ AdmTimeSlotR eid wid)
      _ -> defaultLayout $ do
          setTitleI MsgWorkSchedule
          $(widgetFile "admin/staff/schedule/edit")


getAdmScheduleEditR :: StaffId -> ScheduleId -> Handler Html
getAdmScheduleEditR eid wid = do
    stati <- reqGetParams <$> getRequest
    slot <- runDB $ selectOne $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleId ==. val wid
        return x
    (fw,et) <- generateFormPost $ formSchedule eid slot
    defaultLayout $ do
        setTitleI MsgWorkSchedule
        $(widgetFile "admin/staff/schedule/edit")


getAdmTimeSlotR :: StaffId -> ScheduleId -> Handler Html
getAdmTimeSlotR eid wid = do
    stati <- reqGetParams <$> getRequest
    slot <- runDB $ selectOne $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleId ==. val wid
        return x
    dlgSlotDelete <- newIdent
    (fw,et) <- generateFormPost formSlotDelete
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgWorkingHours
        $(widgetFile "admin/staff/schedule/schedule")


getAdmEmplCalendarR :: StaffId -> Month -> Handler Html
getAdmEmplCalendarR eid month = do
    stati <- reqGetParams <$> getRequest
    empl <- runDB $ selectOne $ do
        x :& u <- from $ table @Staff
            `leftJoin` table @User `on` (\(x :& u) -> x ^. StaffUser ==. u ?. UserId)
        where_ $ x ^. StaffId ==. val eid
        return (x,u)

    slots <- M.fromListWith (+) . (diff <$>) <$> runDB ( select ( do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleStaff ==. val eid
        where_ $ x ^. ScheduleWorkDay `between` (val (periodFirstDay month), val (periodLastDay month))
        return ( x ^. ScheduleWorkDay
               , (x ^. ScheduleWorkStart,x ^. ScheduleWorkEnd)
               ) ) )

    app <- getYesod
    langs <- languages
    curr <- getCurrentRoute
    msgs <- getMessages

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month
    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    formQuery <- newIdent
    toolbarTop <- newIdent
    calendarPage <- newIdent
    defaultLayout $ do
        setTitleI MsgEmployee
        $(widgetFile "admin/staff/empl/calendar/calendar")

  where
      diff (Value d,(Value s,Value e)) = (d,diffLocalTime (LocalTime d e) (LocalTime d s))



getAdmScheduleR :: StaffId -> Handler Html
getAdmScheduleR eid = do
    sort <- fromMaybe SortOrderDesc . (readMaybe . unpack =<<) <$> runInputGet (iopt textField "sort")
    mwid <- runInputGet $ iopt textField "wid"
    scrollY <- runInputGet $ iopt textField "y"
    stati <- reqGetParams <$> getRequest
    empl <- runDB $ selectOne $ do
        x :& u <- from $ table @Staff
            `leftJoin` table @User `on` (\(x :& u) -> x ^. StaffUser ==. u ?. UserId)
        where_ $ x ^. StaffId ==. val eid
        return (x,u)
    schedule <- runDB $ select $ do
        x <- from $ table @Schedule
        where_ $ x ^. ScheduleStaff ==. val eid
        case sort of
          SortOrderAsc -> orderBy [asc (x ^. ScheduleWorkDay), asc (x ^. ScheduleWorkStart)]
          _            -> orderBy [desc (x ^. ScheduleWorkDay), desc (x ^. ScheduleWorkStart)]
        return x
    curr <- getCurrentRoute
    msgs <- getMessages
    touchTargetWrapperAddSchedule <- newIdent
    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    formQuery <- newIdent
    toolbarTop <- newIdent
    buttonSort <- newIdent
    defaultLayout $ do
        setTitleI MsgEmployee
        $(widgetFile "admin/staff/empl/schedule")


postAdmScheduleR :: StaffId -> Handler Html
postAdmScheduleR eid = do
    stati <- reqGetParams <$> getRequest
    ((fr,fw),et) <- runFormPost $ formSchedule eid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI "info" MsgRecordAdded
          redirect (AdminR $ AdmScheduleR eid,stati)
      _ -> defaultLayout $ do
        setTitleI MsgWorkSchedule
        $(widgetFile "admin/staff/schedule/create")


getAdmScheduleCreateR :: StaffId -> Handler Html
getAdmScheduleCreateR eid = do
    stati <- reqGetParams <$> getRequest
    (fw,et) <- generateFormPost $ formSchedule eid Nothing
    defaultLayout $ do
        setTitleI MsgWorkSchedule
        $(widgetFile "admin/staff/schedule/create")


formSchedule :: StaffId -> Maybe (Entity Schedule)
             -> Html -> MForm Handler (FormResult Schedule,Widget)
formSchedule eid schedule extra = do
    (dayR,dayV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgDay
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (scheduleWorkDay . entityVal <$> schedule)
    (startR,startV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgStartTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (scheduleWorkStart . entityVal <$> schedule)
    (endR,endV) <- mreq (afterTimeField startR) FieldSettings
        { fsLabel = SomeMessage MsgEndTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (scheduleWorkEnd . entityVal <$> schedule)
    let r = Schedule eid <$> dayR <*> startR <*> endR
    let w = [whamlet|
#{extra}
$forall (v,icon) <- [(dayV,"event"),(startV,"schedule"),(endV,"schedule")]
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
|]
    return (r,w)
  where

      afterTimeField :: FormResult TimeOfDay -> Field Handler TimeOfDay
      afterTimeField startR = check (afterTime startR) timeField

      afterTime :: FormResult TimeOfDay -> TimeOfDay -> Either AppMessage TimeOfDay
      afterTime startR x = case startR of
          FormSuccess s | x > s -> Right x
                        | otherwise -> Left MsgInvalidTimeInterval
          _ -> Right x


getAdmStaffSearchR :: Handler Html
getAdmStaffSearchR = do
    mq <- runInputGet $ iopt textField "q"
    rnames <- (snd <$>) . filter ((== "role") . fst) . reqGetParams <$> getRequest
    ratings <- (read . unpack . snd <$>) . filter ((== "rating") . fst) . reqGetParams <$> getRequest
    stati <- (read . unpack . snd <$>) . filter ((== "status") . fst) . reqGetParams <$> getRequest
    accstati <- (snd <$>) . filter ((== "accstatus") . fst) . reqGetParams <$> getRequest
    staff <- runDB $ select $ do
          x <- from $ table @Staff
          case mq of
            Just q -> where_ $ ( upper_ (x ^. StaffName) `like` (%) ++. upper_ (val q) ++. (%) )
              ||. ( upper_ (x ^. StaffPhone) `like` (%) ++. upper_ (just (val q)) ++. (%) )
              ||. ( upper_ (x ^. StaffMobile) `like` (%) ++. upper_ (just (val q)) ++. (%) )
              ||. ( upper_ (x ^. StaffEmail) `like` (%) ++. upper_ (just (val q)) ++. (%) )
            Nothing -> return ()
          case stati of
            [] -> return ()
            ys -> where_ $ x ^. StaffStatus `in_` valList ys
          case accstati of
            ["registered"] -> where_ $ not_ $ isNothing_ $ x ^. StaffUser
            ["unregistered"] -> where_ $ isNothing_ $ x ^. StaffUser
            _ -> return ()
          case rnames of
            [] -> return ()
            ys -> where_ $ exists $ do
                y <- from $ table @Role
                where_ $ y ^. RoleStaff ==. x ^. StaffId
                where_ $ y ^. RoleName `in_` valList ys
          case ratings of
            [] -> return ()
            ys -> where_ $ exists $ do
                y <- from $ table @Role
                where_ $ y ^. RoleStaff ==. x ^. StaffId
                where_ $ y ^. RoleRating `in_` justList (valList ys)
          orderBy [asc (x ^. StaffId)]
          return x
    roles <- forM staff ( \e@(Entity eid _) -> (e,) <$> runDB ( selectOne $ do
          x <- from $ table @Role
          where_ $ x ^. RoleStaff ==. val eid
          orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
          return x ) )
    roleList <- (unValue <$>) <$> runDB ( select $ distinct $ do
        x <- from $ table @Role
        orderBy [asc (x ^. RoleName)]
        return $ x ^. RoleName )
    ratingList <- (unValue <$>) <$> runDB ( select $ distinct $ do
        x <- from $ table @Role
        orderBy [asc (x ^. RoleRating)]
        return $ x ^. RoleRating )
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "admin/staff/search")


postAdmEmplUnregR :: StaffId -> UserId -> Handler ()
postAdmEmplUnregR eid uid = do
    runDB $ delete uid
    addMessageI "info" MsgRecordEdited
    redirect $ AdminR $ AdmEmplR eid


postAdmEmplUserR :: StaffId -> Handler Html
postAdmEmplUserR eid = do
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x
    ((fr,fw),et) <- runFormPost $ formUser empl
    case fr of
      FormSuccess r -> do
          uid <- runDB $ insert r
          runDB $ update $ \x -> do
              set x [StaffUser =. just (val uid)]
              where_ $ x ^. StaffId ==. val eid
          photo <- runDB $ selectOne $ do
              y <- from $ table @StaffPhoto
              where_ $ y ^. StaffPhotoStaff ==. val eid
              return y
          case photo of
            Just (Entity _ (StaffPhoto _ bs mime)) -> runDB $ insert_ (UserPhoto uid bs mime)
            Nothing -> return ()
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR $ AdmEmplR eid
      _ -> defaultLayout $ do
        setTitleI MsgRegistration
        $(widgetFile "admin/staff/user/user")


getAdmEmplUserR :: StaffId -> Handler Html
getAdmEmplUserR eid = do
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x
    (fw,et) <- generateFormPost $ formUser empl
    defaultLayout $ do
        setTitleI MsgRegistration
        $(widgetFile "admin/staff/user/user")


formUser :: Maybe (Entity Staff) -> Html -> MForm Handler (FormResult User, Widget)
formUser empl extra = do
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgUsername
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (T.concat . T.words . T.toLower . staffName . entityVal <$> empl)
    (passR,passV) <- mreq passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    (adminR,adminV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAdministrator
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (pure False)
    (analystR,analystV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAnalyst
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (pure False)
    (fnameR,fnameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (pure (staffName . entityVal <$> empl))
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffEmail . entityVal <$> empl)
    let r = User <$> nameR <*> passR <*> adminR <*> analystR <*> fnameR <*> emailR
    let w = [whamlet|
#{extra}
$forall v <- [nameV,passV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid
      :isJust (fvErrors v):.mdc-text-field--with-trailing-icon>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      $maybe _ <- fvErrors v
        <i.mdc-text-field__icon.mdc-text-field__icon--trailing.material-symbols-outlined>error
      <div.mdc-line-ripple>
    $maybe errs <- fvErrors v
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}

$forall (r,v) <- [(adminR,adminV),(analystR,analystV)]
  <div.mdc-form-field.form-field data-mdc-auto-init=MDCFormField style="display:flex;flex-direction:row">
    ^{fvInput v}
    $with selected <- resolveSelected r
      <button.mdc-switch type=button role=switch #switch#{fvId v} data-mdc-auto-init=MDCSwitch
        :selected:.mdc-switch--selected :selected:aria-checked=true
        :not selected:.mdc-switch--unselected :not selected:aria-checked=false
        onclick="document.getElementById('#{fvId v}').checked = !this.MDCSwitch.selected">
        <div.mdc-switch__track>
        <div.mdc-switch__handle-track>
          <div.mdc-switch__handle>
            <div.mdc-switch__shadow>
              <div.mdc-elevation-overlay>
            <div.mdc-switch__ripple>
            <div.mdc-switch__icons>
              <svg.mdc-switch__icon.mdc-switch__icon--on viewBox="0 0 24 24">
                <path d="M19.69,5.23L8.96,15.96l-4.23-4.23L2.96,13.5l6,6L21.46,7L19.69,5.23z">
              <svg.mdc-switch__icon.mdc-switch__icon--off viewBox="0 0 24 24">
                <path d="M20 13H4v-2h16v2z">

      <span.mdc-switch__focus-ring-wrapper>
        <span.mdc-switch__focus-ring>
      <label for=switch#{fvId v}>#{fvLabel v}

$forall v <- [fnameV,emailV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid
      :isJust (fvErrors v):.mdc-text-field--with-trailing-icon>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      $maybe _ <- fvErrors v
        <i.mdc-text-field__icon.mdc-text-field__icon--trailing.material-symbols-outlined>error
      <div.mdc-line-ripple>
    $maybe errs <- fvErrors v
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}
|]
    return (r,w)
  where
      resolveSelected adminR = case adminR of FormSuccess x -> x ; _ -> False

      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mu <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserName ==. val name
              return x
          return $ case mu of
            Nothing -> Right name
            Just _ -> Left MsgAlreadyExists


postAdmRoleDeleteR :: StaffId -> RoleId -> Handler ()
postAdmRoleDeleteR eid rid = do
    runDB $ delete rid
    addMessageI "info" MsgRecordDeleted
    stati <- reqGetParams <$> getRequest
    redirect (AdminR $ AdmRolesR eid,stati)


postAdmRoleR :: StaffId -> RoleId -> Handler Html
postAdmRoleR eid rid = do
    role <- runDB $ selectOne $ do
        x <- from $ table @Role
        where_ $ x ^. RoleId ==. val rid
        return x
    ((fr,fw),et) <- runFormPost $ formRole eid role
    stati <- reqGetParams <$> getRequest
    case fr of
      FormSuccess r -> do
          runDB $ replace rid r
          addMessageI "info" MsgRecordEdited
          redirect (AdminR $ AdmRoleR eid rid,stati)
      _ -> defaultLayout $ do
          setTitleI MsgRole
          $(widgetFile "admin/staff/role/edit")


getAdmRoleEditR :: StaffId -> RoleId -> Handler Html
getAdmRoleEditR eid rid = do
    stati <- reqGetParams <$> getRequest
    role <- runDB $ selectOne $ do
        x <- from $ table @Role
        where_ $ x ^. RoleId ==. val rid
        return x
    (fw,et) <- generateFormPost $ formRole eid role
    defaultLayout $ do
        setTitleI MsgRole
        $(widgetFile "admin/staff/role/edit")



getAdmRoleR :: StaffId -> RoleId -> Handler Html
getAdmRoleR sid rid = do
    state <- reqGetParams <$> getRequest
    role <- runDB $ selectOne $ do
        x :& e :& s <- from $ table @Role
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. RoleStaff ==. e ^. StaffId)
            `innerJoin` table @Service `on` (\(x :& _ :& s) -> x ^. RoleService ==. s ^. ServiceId)
        where_ $ x ^. RoleId ==. val rid
        return (x,e,s)
    app <- getYesod
    langs <- languages
    dlgRoleDelete <- newIdent
    defaultLayout $ do
        setTitleI MsgRole
        $(widgetFile "admin/staff/role/role")


getAdmRolesR :: StaffId -> Handler Html
getAdmRolesR eid = do
    mrid <- runInputGet $ iopt textField "rid"
    scrollY <- runInputGet $ iopt textField "y"
    stati <- reqGetParams <$> getRequest
    empl <- runDB $ selectOne $ do
        x :& u <- from $ table @Staff
            `leftJoin` table @User `on` (\(x :& u) -> x ^. StaffUser ==. u ?. UserId)
        where_ $ x ^. StaffId ==. val eid
        return (x,u)
    roles <- runDB $ select $ do
        x :& s <- from $ table @Role
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. RoleService ==. s ^. ServiceId)
        where_ $ x ^. RoleStaff ==. val eid
        orderBy [asc (x ^. RoleId)]
        return (x,s)
    curr <- getCurrentRoute
    msgs <- getMessages
    touchTargetWrapperAddRole <- newIdent
    defaultLayout $ do
        setTitleI MsgEmployee
        $(widgetFile "admin/staff/empl/roles")


postAdmRolesR :: StaffId -> Handler Html
postAdmRolesR eid = do
    stati <- reqGetParams <$> getRequest
    ((fr,fw),et) <- runFormPost $ formRole eid Nothing
    case fr of
      FormSuccess r -> do
          rid <- runDB $ insert r
          addMessageI "info" MsgRecordAdded
          redirect (AdminR $ AdmRolesR eid,stati ++ [("rid",pack $ show $ fromSqlKey rid)])
      _ -> defaultLayout $ do
          setTitleI MsgRole
          $(widgetFile "admin/staff/role/create")


getAdmRoleCreateR :: StaffId -> Handler Html
getAdmRoleCreateR eid = do
    stati <- reqGetParams <$> getRequest
    (fw,et) <- generateFormPost $ formRole eid Nothing
    defaultLayout $ do
        setTitleI MsgRole
        $(widgetFile "admin/staff/role/create")


formRole :: StaffId -> Maybe (Entity Role) -> Html -> MForm Handler (FormResult Role,Widget)
formRole eid role extra = do
    services <- lift $ runDB queryServices
    (servR,servV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (roleService . entityVal <$> role)
    (nameR,nameV) <- mreq (uniqueNameField servR) FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (roleName . entityVal <$> role)
    (durationR,durationV) <- mreq hmField FieldSettings
        { fsLabel = SomeMessage MsgCompletionTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (roleDuration . entityVal <$> role)
    (ratingR,ratingV) <- mopt ratingField FieldSettings
        { fsLabel = SomeMessage MsgRating
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("min","0"),("max","5")]
        } (roleRating . entityVal <$> role)
    let r = Role eid <$> servR <*> nameR <*> durationR <*> ratingR
    let w = [whamlet|
#{extra}
<div.form-field>
  <div.mdc-select.mdc-select--filled.mdc-select--required data-mdc-auto-init=MDCSelect
    :isJust (fvErrors servV):.mdc-select--invalid>
    ^{fvInput servV}
    <div.mdc-select__anchor role=button aria-haspopup=listbox aria-expanded=false>
      <span.mdc-select__ripple>
      <span.mdc-floating-label>#{fvLabel servV}
      <span.mdc-select__selected-text-container>
        <span.mdc-select__selected-text>
      <span.mdc-select__dropdown-icon>
        <svg.mdc-select__dropdown-icon-graphic viewBox="7 10 10 5" focusable=false>
          <polygon.mdc-select__dropdown-icon-inactive stroke=none fill-rule=evenodd points="7 10 12 15 17 10">
          <polygon.mdc-select__dropdown-icon-active stroke=none fill-rule=evenodd points="7 15 12 10 17 15">
      <span.mdc-line-ripple>

    <div.mdc-select__menu.mdc-menu.mdc-menu-surface.mdc-menu-surface--fullwidth>
      <ul.mdc-deprecated-list role=listbox>
        $forall Entity sid (Service name _ _ _ _ _) <- services
          <li.mdc-deprecated-list-item role=option data-value=#{fromSqlKey sid} aria-selected=false>
            <span.mdc-deprecated-list-item__ripple>
            <span.mdc-deprecated-list-item__text>
              #{name}

  $maybe errs <- fvErrors servV
    <div.mdc-select-helper-text.mdc-select-helper-text--validation-msg>
      #{errs}


<div.form-field>
  <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
    :isJust (fvErrors nameV):.mdc-text-field--invalid
    :isJust (fvErrors nameV):.mdc-text-field--with-trailing-icon>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel nameV}
    ^{fvInput nameV}
    $maybe _ <- fvErrors nameV
      <i.mdc-text-field__icon.mdc-text-field__icon--trailing.material-symbols-outlined>error
    <span.mdc-line-ripple>
  $maybe errs <- fvErrors nameV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}


<div.form-field>
  <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
    :isJust (fvErrors durationV):.mdc-text-field--invalid
    :isJust (fvErrors durationV):.mdc-text-field--with-trailing-icon>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel durationV}
    ^{fvInput durationV}
    $maybe _ <- fvErrors durationV
      <i.mdc-text-field__icon.mdc-text-field__icon--trailing.material-symbols-outlined>error
    <span.mdc-line-ripple>
  <div.mdc-text-field-helper-line>
    $maybe errs <- fvErrors durationV
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
    $nothing
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--persistent aria-hidden=true>
        _{MsgPatternHourMinute}


<div.form-field>
  <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
    :isJust (fvErrors ratingV):.mdc-text-field--invalid
    :isJust (fvErrors ratingV):.mdc-text-field--with-trailing-icon>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel ratingV}
    ^{fvInput ratingV}
    $maybe _ <- fvErrors ratingV
      <i.mdc-text-field__icon.mdc-text-field__icon--trailing.material-symbols-outlined>error
    <span.mdc-line-ripple>
  $maybe errs <- fvErrors ratingV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
|]
    return (r,w)
  where

      hmField :: Field Handler DiffTime
      hmField = Field
          { fieldParse = parseHelper $ \s -> case parseTimeM True defaultTimeLocale "%H:%M" . unpack $ s of
              Just x -> Right x
              Nothing -> Left $ MsgInvalidEntry s
          , fieldView = \theId name attrs v isReq -> [whamlet|
$newline never
<input type=text ##{theId} name=#{name} value=#{showVal v} *{attrs} :isReq:required>
|]
          , fieldEnctype = UrlEncoded
          }

      showVal = either id (pack . formatTime defaultTimeLocale "%H:%M")

      ratingField = checkBool (\x -> x >= 1 && x <= 5) (MsgValueNotInRange 1 5) intField

      uniqueNameField :: FormResult ServiceId -> Field Handler Text
      uniqueNameField servR = checkM (uniqueName servR) textField

      uniqueName :: FormResult ServiceId -> Text -> Handler (Either AppMessage Text)
      uniqueName servR name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Role
              where_ $ x ^. RoleStaff ==. val eid
              case servR of
                FormSuccess sid -> where_ $ x ^. RoleService ==. val sid
                _ -> return ()
              where_ $ x ^. RoleName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity rid _) -> case role of
              Nothing -> Left MsgRoleAlreadyInTheList
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgRoleAlreadyInTheList

      queryServices = select $ do
          x <- from $ table @Service
          where_ $ just (x ^. ServiceId) `notIn` subSelectList
              ( from $ selectQuery $ do
                y <- from $ table @Service
                where_ $ not_ $ isNothing_ (y ^. ServiceGroup)
                return $ y ^. ServiceGroup
              )
          return x


postAdmStaffDeleteR :: StaffId -> Handler ()
postAdmStaffDeleteR sid = do
    runDB $ delete sid
    addMessageI "info" MsgRecordDeleted
    redirect $ AdminR AdmStaffR


postAdmEmplR :: StaffId -> Handler Html
postAdmEmplR sid = do
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val sid
        return x
    ((fr,fw),et) <- runFormPost $ formEmpl empl
    case fr of
      FormSuccess (r,mfi) -> do
          runDB $ replace sid r
          addMessageI "info" MsgRecordEdited
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                _ <- runDB $ upsert
                     (StaffPhoto sid bs (fileContentType fi))
                     [StaffPhotoPhoto P.=. bs, StaffPhotoMime P.=. fileContentType fi]
                return ()
            Nothing -> return ()
          redirect $ AdminR $ AdmEmplR sid
      _ -> defaultLayout $ do
          setTitleI MsgEmployee
          $(widgetFile "admin/staff/empl/edit")


getAdmEmplR :: StaffId -> Handler Html
getAdmEmplR eid = do
    stati <- reqGetParams <$> getRequest
    scrollY <- runInputGet $ iopt textField "y"
    empl <- runDB $ selectOne $ do
        x :& u <- from $ table @Staff
            `leftJoin` table @User `on` (\(x :& u) -> x ^. StaffUser ==. u ?. UserId)
        where_ $ x ^. StaffId ==. val eid
        return (x,u)
    curr <- getCurrentRoute
    msgs <- getMessages
    dlgUnregEmplUser <- newIdent
    dlgEmplDelete <- newIdent
    defaultLayout $ do
        setTitleI MsgEmployee
        $(widgetFile "admin/staff/empl/details")


getAdmStaffEditR :: StaffId -> Handler Html
getAdmStaffEditR sid = do
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val sid
        return x
    (fw,et) <- generateFormPost $ formEmpl empl
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "admin/staff/empl/edit")


getAdmStaffCreateR :: Handler Html
getAdmStaffCreateR = do
    (fw,et) <- generateFormPost $ formEmpl Nothing
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "admin/staff/empl/create")


formEmpl :: Maybe (Entity Staff) -> Html -> MForm Handler (FormResult (Staff,Maybe FileInfo), Widget)
formEmpl staff extra = do
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffName . entityVal <$> staff)
    (statusR,statusV) <- first (read . unpack <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgStatus
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (pack . show <$> ((staffStatus . entityVal <$> staff) <|> pure EmplStatusAvailable))
    (phoneR,phoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffPhone . entityVal <$> staff)
    (mobileR,mobileV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgMobile
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffMobile . entityVal <$> staff)
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffEmail . entityVal <$> staff)
    (photoR,photoV) <- mopt fileField  FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r = (,)
            <$> ( Staff
                  <$> nameR
                  <*> statusR
                  <*> phoneR
                  <*> mobileR
                  <*> emailR
                  <*> FormSuccess Nothing
                )
            <*> photoR
    let w = $(widgetFile "admin/staff/empl/form")
    return (r,w)

  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Staff
              where_ $ x ^. StaffName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity eid _) -> case staff of
              Nothing -> Left MsgEmployeeAlreadyInTheList
              Just (Entity eid' _) | eid == eid' -> Right name
                                   | otherwise -> Left MsgEmployeeAlreadyInTheList



postAdmStaffR :: Handler Html
postAdmStaffR = do
    ((fr,fw),et) <- runFormPost $ formEmpl Nothing
    case fr of
      FormSuccess (r,mfi) -> do
          eid <- runDB $ insert r
          addMessageI "info" MsgRecordAdded
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                runDB $ insert_ StaffPhoto { staffPhotoStaff = eid
                                           , staffPhotoPhoto = bs
                                           , staffPhotoMime = fileContentType fi
                                           }
            Nothing -> return ()
          redirect $ AdminR AdmStaffR
      _ -> defaultLayout $ do
          setTitleI MsgEmployee
          $(widgetFile "admin/staff/empl/create")


getAdmStaffR :: Handler Html
getAdmStaffR = do
    user <- maybeAuth
    meid <- (toSqlKey <$>) <$> runInputGet (iopt intField "eid")
    scrollY <- runInputGet (iopt textField "y")
    staff <- runDB $ select $ do
        x <- from $ table @Staff
        orderBy [asc (x ^. StaffId)]
        return x
    roles <- forM staff ( \e@(Entity eid _) -> (e,) <$> runDB ( selectOne $ do
          x <- from $ table @Role
          where_ $ x ^. RoleStaff ==. val eid
          orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
          return x ) )
    msgs <- getMessages
    setUltDestCurrent
    fabAddStaff <- newIdent
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "admin/staff/staff")


getAdmStaffPhotoR :: StaffId -> Handler TypedContent
getAdmStaffPhotoR sid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @StaffPhoto
        where_ $ x ^. StaffPhotoStaff ==. val sid
        return x
    return $ case photo of
      Just (Entity _ (StaffPhoto _ bs mime)) -> TypedContent (encodeUtf8 mime) (toContent bs)
      Nothing -> TypedContent typeSvg $ toContent $(embedFile "static/img/person_FILL0_wght400_GRAD0_opsz48.svg")


range :: Enum a => a -> a -> [a]
range a b = [a..b]


fullHours :: NominalDiffTime -> NominalDiffTime -> Bool
fullHours x y = mod' x y == 0
