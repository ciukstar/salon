{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Stats
  ( getPopOffersR
  , getWorkloadsR
  , getWorkloadEmplMonthR
  , getWorkloadEmplDayR
  , getCustomerRankingR
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Foldable (find)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (pack, unpack, intercalate)
import Data.Time.Calendar
    ( Day, dayPeriod, periodFirstDay, periodLastDay, weekFirstDay
    , DayOfWeek (Monday), addDays, toGregorian
    )
import Data.Time.Calendar.Month (Month, pattern YearMonth, addMonths)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, getCurrentTime, utctDay)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (timeOfDayToTime)
import qualified Data.Map.Lazy as M (fromListWith, fromList, toList)
import qualified Data.Map.Merge.Lazy as ML (merge, mapMissing, zipWithMatched)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (SomeMessage (SomeMessage), renderMessage)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler
    ( setUltDestCurrent, newIdent, getRequest, reqGetParams, getYesod, languages
    )
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (dayField, textField)
import Yesod.Form.Functions (check, generateFormGet', runFormGet, mreq)
import Yesod.Form.Types
    ( Field, MForm, FormResult (FormSuccess, FormMissing, FormFailure)
    , fvErrors, fvLabel, fvInput, fvId
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsName, fsAttrs, fsId)
    )
import Yesod.Persist.Core (runDB)

import Database.Persist (Entity (Entity))

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (Value), select, from, table, innerJoin, on, subSelectCount
    , (:&)((:&)), (^.), (==.)
    , where_, orderBy, desc, just, groupBy, sum_, coalesceDefault, val, between
    , selectOne, unValue
    )

import Foundation
    ( Handler, Widget
    , Route (AccountPhotoR, PhotoPlaceholderR, ProfileR, AuthR, AdminR, StatsR)
    , AdminR (AdmStaffPhotoR)
    , StatsR (WorkloadsR, WorkloadEmplMonthR, WorkloadEmplDayR, CustomerRankingR)
    , AppMessage
      ( MsgPopularOffers, MsgUserProfile, MsgPhoto, MsgNavigationMenu, MsgLogin
      , MsgNoDataToDisplay, MsgService, MsgBookings, MsgNumberSign, MsgWorkload
      , MsgSelect, MsgCancel, MsgPeriod, MsgBetweenFrom, MsgBetweenTo, MsgBack
      , MsgInvalidFormData, MsgInvalidTimeInterval, MsgNoDataFound, MsgMon
      , MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun, MsgTotalBookedTime
      , MsgTotalScheduledTime, MsgSymbolMinute, MsgSymbolHour, MsgSortAscending
      , MsgSortDescending
      )
    )

import Model
    ( Offer (Offer), Book, Service (Service), Role, Staff (Staff), StaffId
    , Schedule
    , EntityField
      ( OfferId, BookOffer, ServiceId, OfferService, BookRole, RoleId, RoleStaff
      , StaffId, BookDay, RoleDuration, ScheduleStaff, ScheduleWorkDay, ScheduleWorkEnd
      , ScheduleWorkStart, StaffName
      )
    , SortOrder (SortOrderAsc, SortOrderDesc)
    )

import Settings (widgetFile)
import Menu (menu)


getCustomerRankingR :: Handler Html
getCustomerRankingR = do
    user <- maybeAuth
    defaultLayout $ do
        setTitleI MsgWorkload
        $(widgetFile "stats/customer/aov")


getWorkloadEmplDayR :: StaffId -> Day -> Handler Html
getWorkloadEmplDayR eid day = do
    stati <- reqGetParams <$> getRequest
    
    employee <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x

    booked <- maybe 0 unValue <$> runDB ( selectOne $ do
        b :& r :& e <- from $ table @Book
            `innerJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. just (r ^. RoleId))
            `innerJoin` table @Staff `on` (\(_ :& r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
        where_ $ e ^. StaffId ==. val eid
        where_ $ b ^. BookDay ==. val day
        
        let dur = coalesceDefault [sum_ (r ^. RoleDuration)] (val 0) :: SqlExpr (Value DiffTime)

        return dur )

    planned <- sum . (calcdur <$>) <$> runDB ( select ( do
        s :& e <- from $ table @Schedule
            `innerJoin` table @Staff `on` (\(s :& e) -> s ^. ScheduleStaff ==. e ^. StaffId)
        where_ $ e ^. StaffId ==. val eid
        where_ $ s ^. ScheduleWorkDay ==. val day
        return (s ^. ScheduleWorkStart, s ^. ScheduleWorkEnd) ) )

    let ratio = if planned == 0 then 0 else
                  fromIntegral (diffTimeToPicoseconds booked)
                  / fromIntegral (diffTimeToPicoseconds planned) :: Double
            
    app <- getYesod
    langs <- languages
    defaultLayout $ do
        setTitleI MsgWorkload
        $(widgetFile "stats/workloads/details")

  where

      calcdur (Value start,Value end) = timeOfDayToTime end - timeOfDayToTime start
    


getWorkloadEmplMonthR :: StaffId -> Month -> Handler Html
getWorkloadEmplMonthR eid month = do
    stati <- reqGetParams <$> getRequest
    
    employee <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x

    booked <- (unwrap <$>) <$> runDB ( select $ do
        b :& r :& e <- from $ table @Book
            `innerJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. just (r ^. RoleId))
            `innerJoin` table @Staff `on` (\(_ :& r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
        where_ $ e ^. StaffId ==. val eid
        where_ $ (b ^. BookDay) `between` (val (periodFirstDay month), val (periodLastDay month))
        
        let dur = coalesceDefault [sum_ (r ^. RoleDuration)] (val 0) :: SqlExpr (Value DiffTime)

        groupBy (e ^. StaffId, b ^. BookDay)
        return (e ^. StaffId, b ^. BookDay, dur) )

    planned <- (calcdur <$>) <$> runDB ( select ( do
        s :& e <- from $ table @Schedule
            `innerJoin` table @Staff `on` (\(s :& e) -> s ^. ScheduleStaff ==. e ^. StaffId)
        where_ $ e ^. StaffId ==. val eid
        where_ $ (s ^. ScheduleWorkDay) `between` (val (periodFirstDay month), val (periodLastDay month))
        return (e ^. StaffId, s ^. ScheduleWorkDay, s ^. ScheduleWorkStart, s ^. ScheduleWorkEnd) ) )

    let ratios = M.toList $ ML.merge
          (ML.mapMissing $ \ _ _ -> 0)
          (ML.mapMissing $ \ _ _ -> 0)
          (ML.zipWithMatched $ \ _ x y -> (if 0 == y then 0 else x / y :: Double))
          (M.fromList . (second (fromIntegral . diffTimeToPicoseconds) <$>) $ booked)
          (M.fromList . (second (fromIntegral . diffTimeToPicoseconds) <$>) $ planned)

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    defaultLayout $ do
        setTitleI MsgWorkload
        $(widgetFile "stats/workloads/calendar")

  where

      unwrap (Value emplid,Value day,Value dur) = ((emplid,day),dur)

      calcdur (Value emplid,Value day,Value start,Value end) =
          ((emplid,day),timeOfDayToTime end - timeOfDayToTime start)


getWorkloadsR :: Handler Html
getWorkloadsR = do
    stati <- filter ((/= "sort") . fst) . reqGetParams <$> getRequest

    today <- liftIO (utctDay <$> getCurrentTime)
    start <- fromMaybe today <$> runInputGet (iopt dayField "start")
    end <- fromMaybe today <$> runInputGet (iopt dayField "end")
    sort <- fromMaybe SortOrderDesc . (readMaybe . unpack =<<) <$> runInputGet (iopt textField "sort")

    booked <- (unwrap <$>) <$> runDB ( select $ do
        b :& r :& e <- from $ table @Book
            `innerJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. just (r ^. RoleId))
            `innerJoin` table @Staff `on` (\(_ :& r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
        where_ $ (b ^. BookDay) `between` (val start, val end)

        let dur = coalesceDefault [sum_ (r ^. RoleDuration)] (val 0) :: SqlExpr (Value DiffTime)

        groupBy (e ^. StaffId, e ^. StaffName)
        return (e ^. StaffId, e ^. StaffName, dur) )

    planned <- (calcdur <$>) <$> runDB ( select ( do
        s :& e <- from $ table @Schedule
            `innerJoin` table @Staff `on` (\(s :& e) -> s ^. ScheduleStaff ==. e ^. StaffId)
        where_ $ s ^. ScheduleWorkDay `between` (val start, val end)
        return (e ^. StaffId, e ^. StaffName, s ^. ScheduleWorkStart, s ^. ScheduleWorkEnd) ) )

    let ratios = sortBy (choose sort) . M.toList $ ML.merge
          (ML.mapMissing $ \ _ _ -> 0)
          (ML.mapMissing $ \ _ _ -> 0)
          (ML.zipWithMatched $ \ _ x y -> (if 0 == y then 0 else x / y :: Double))
          (M.fromListWith (+) . (second (fromIntegral . diffTimeToPicoseconds) <$>) $ booked)
          (M.fromListWith (+) . (second (fromIntegral . diffTimeToPicoseconds) <$>) $ planned)

    user <- maybeAuth
    setUltDestCurrent
    toolbarTop <- newIdent
    dlgTimeFrame <- newIdent
    formTimeFrame <- newIdent

    ((fr,fw0),et0) <- runFormGet $ formPeriod start end
    case fr of
      FormMissing -> do
          (fw,et) <- generateFormGet' $ formPeriod start end
          defaultLayout $ do
              setTitleI MsgWorkload
              $(widgetFile "stats/workloads/workloads")

      _ -> do
          let (fw,et) = (fw0,et0)
          defaultLayout $ do
              setTitleI MsgWorkload
              $(widgetFile "stats/workloads/workloads")

  where

      choose SortOrderAsc  (_,x) (_,y) = x `compare` y
      choose SortOrderDesc (_,x) (_,y) = y `compare` x
      
      unwrap (Value eid,Value ename,Value dur) = ((eid,ename),dur)

      calcdur (Value eid,Value ename,Value start,Value end) =
          ((eid,ename),timeOfDayToTime end - timeOfDayToTime start)


formPeriod :: Day -> Day -> Html -> MForm Handler (FormResult (Day,Day),Widget)
formPeriod start end extra = do
    (startR,startV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgBetweenFrom
        , fsTooltip = Nothing, fsId = Nothing, fsName = Just "start"
        , fsAttrs = [("class","mdc-text-field__input")]
        } (Just start)
    (endR,endV) <- mreq (afterDayField startR) FieldSettings
        { fsLabel = SomeMessage MsgBetweenTo
        , fsTooltip = Nothing, fsId = Nothing, fsName = Just "end"
        , fsAttrs = [("class","mdc-text-field__input")]
        } (Just end)

    return ((,) <$> startR <*> endR,[whamlet|
#{extra}
$forall (v,icon) <- [(startV,"event"),(endV,"event")]
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
|])
  where

    afterDayField :: FormResult Day -> Field Handler Day
    afterDayField startR = check (afterDay startR) dayField

    afterDay :: FormResult Day -> Day -> Either AppMessage Day
    afterDay startR x = case startR of
      FormSuccess s | x < s -> Left MsgInvalidTimeInterval
                    | otherwise -> Right x

      _ -> Right x


getPopOffersR :: Handler Html
getPopOffersR = do
    user <- maybeAuth
    offers <- zip [1 :: Int ..] <$> runDB ( select $ do
        o :& s <- from $ table @Offer
            `innerJoin` table @Service `on` (\(o :& s) -> o ^. OfferService ==. s ^. ServiceId)
        let n :: SqlExpr (Value Int)
            n = subSelectCount $ from (table @Book) >>= \b -> where_ $ b ^. BookOffer ==. o ^. OfferId
        orderBy [desc n]
        return (s,o,n) )
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgPopularOffers
        $(widgetFile "stats/offers/pop")
