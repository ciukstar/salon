{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Admin.Business
  ( getBusinessR
  , postBusinessR
  , getBusinessCreateR
  , getBusinessEditR
  , postBusinessEditR
  , postBusinessDeleteR
  , getBusinessHoursR
  , postBusinessHoursR
  , getBusinessHoursCreateR
  , getBusinessTimeSlotR
  , postBusinessTimeSlotDeleteR
  , getBusinessHoursEditR
  , postBusinessTimeSlotR
  , getBusinessCalendarR
  , getBusinessCalendarSlotsR
  , getBusinessCalendarSlotCreateR
  , postBusinessCalendarSlotCreateR
  , getBusinessCalendarSlotEditR
  , postBusinessCalendarSlotEditR
  , postBusinessCalendarSlotDeleteR
  , getBusinessCalendarSlotR
  , getBusinessAboutR
  , postBusinessAboutR
  , getBusinessAboutCreateR
  , getBusinessAboutEditR
  , postBusinessAboutEditR
  , postBusinessAboutDeleteR
  , getBusinessContactR
  , postBusinessContactR
  , getBusinessContactCreateR
  , getBusinessContactEditR
  , postBusinessContactEditR
  , postBusinessContactDeleteR
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Fixed (mod')
import qualified Data.Map.Lazy as M (fromListWith, lookup)
import Data.Maybe (isNothing, isJust, fromMaybe)
import Data.Text (Text, pack, unpack, intercalate)
import Data.Time.Clock
    ( NominalDiffTime, getCurrentTime, utctDay, secondsToNominalDiffTime )
import Data.Time.Calendar
    ( Day, DayOfWeek (Monday), toGregorian, weekFirstDay, addDays
    , periodFirstDay, periodLastDay
    )
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.LocalTime
    ( TimeZone(timeZoneMinutes), minutesToTimeZone, TimeOfDay
    , diffLocalTime, LocalTime (LocalTime)
    )
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage)
import Text.Read (readMaybe)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, SomeMessage (SomeMessage)
    , redirect, addMessageI, newIdent
    )
import Yesod.Core.Handler
    ( setUltDestCurrent, getCurrentRoute, getYesod, languages
    )
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields
    ( textField, emailField, textareaField, intField, dayField, timeField
    , hiddenField, htmlField, checkBoxField
    )
import Yesod.Form.Functions
    ( generateFormPost, mreq, mopt, runFormPost, checkM, check )
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldView (fvLabel, fvInput, fvErrors, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , Field
    )

import Yesod.Persist.Core (YesodPersist(runDB))
import qualified Database.Persist as P (PersistStoreWrite (delete))
import Database.Persist
    ( Entity (Entity, entityVal), PersistStoreWrite (insert_, replace) )
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, update, set, val, where_, delete
    , (=.), (^.), (==.)
    , orderBy, desc, asc, between, Value (Value)
    )

import Foundation
    ( Handler, Widget
    , Route (ProfileR, AccountPhotoR, PhotoPlaceholderR, AuthR, AdminR)
    , AdminR
      ( BusinessR, BusinessCreateR, BusinessEditR, BusinessDeleteR
      , BusinessHoursR, BusinessHoursCreateR, BusinessTimeSlotR
      , BusinessTimeSlotDeleteR, BusinessHoursEditR, BusinessCalendarR
      , BusinessCalendarSlotsR, BusinessCalendarSlotCreateR
      , BusinessCalendarSlotCreateR, BusinessCalendarSlotEditR
      , BusinessCalendarSlotDeleteR, BusinessCalendarSlotR
      , BusinessAboutR, BusinessAboutCreateR, BusinessAboutEditR
      , BusinessAboutDeleteR
      , BusinessContactR, BusinessContactCreateR, BusinessContactEditR
      , BusinessContactDeleteR
      )
    , AppMessage
      ( MsgBusiness, MsgPhoto, MsgNoBusinessYet, MsgTheName, MsgAddress, MsgAdd
      , MsgPhone, MsgMobile, MsgEmail, MsgSave, MsgCancel, MsgRecordAdded
      , MsgYesDelete, MsgDeleteAreYouSure, MsgPleaseConfirm, MsgRecordEdited
      , MsgRecordDeleted, MsgBusinessAlreadyExists, MsgTimeZoneOffset, MsgTimeZone
      , MsgMinutes, MsgLogin, MsgUserProfile, MsgNavigationMenu, MsgDel, MsgEdit
      , MsgBack, MsgTheFullName, MsgCurrency, MsgBusinessDays, MsgDetails, MsgDay
      , MsgNoBusinessScheduleYet, MsgBusinessHours, MsgStartTime, MsgEndTime
      , MsgDayType, MsgWeekday, MsgWeekend, MsgHoliday, MsgInvalidTimeInterval
      , MsgList, MsgCalendar, MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgSymbolHour, MsgSymbolMinute, MsgToday, MsgBusinessDay, MsgSortAscending
      , MsgSortDescending, MsgAboutUs, MsgContactUs, MsgNoContentYet, MsgContent
      , MsgAlreadyExists, MsgInvalidFormData, MsgWorkSchedule, MsgShowSchedule
      )
    )

import Model
    ( BusinessId
    , Business
      ( Business, businessName, businessFullName, businessAddr, businessPhone
      , businessMobile, businessEmail, businessTzo, businessTz, businessCurrency
      )
    , BusinessHoursId
    , BusinessHours
      ( BusinessHours, businessHoursDay, businessHoursOpen, businessHoursClose
      , businessHoursDayType
      )
    , AboutUsId, AboutUs (AboutUs, aboutUsHtml)
    , ContactUsId, ContactUs (ContactUs, contactUsHtml, contactUsShowSchedule)
    , EntityField
      ( BusinessName, BusinessFullName, BusinessAddr, BusinessPhone, BusinessMobile
      , BusinessEmail, BusinessId, BusinessTzo, BusinessTz, BusinessCurrency
      , BusinessHoursId, BusinessHoursDay, BusinessHoursOpen, BusinessHoursClose
      , BusinessHoursDayType, AboutUsBusiness, AboutUsId, ContactUsBusiness
      , ContactUsId
      )
    , DayType (Weekday, Weekend, Holiday)
    , SortOrder (SortOrderAsc, SortOrderDesc)
    )

import Settings (widgetFile)
import Menu (menu)


postBusinessContactDeleteR :: BusinessId -> ContactUsId -> Handler Html
postBusinessContactDeleteR bid xid = do
    ((fr,_),_) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete xid
          addMessageI "info" MsgRecordDeleted
          redirect $ AdminR $ BusinessContactR bid
      _ -> do
          addMessageI "warn" MsgInvalidFormData
          redirect $ AdminR $ BusinessContactR bid


postBusinessContactEditR :: BusinessId -> ContactUsId -> Handler Html
postBusinessContactEditR bid xid = do
    info <- runDB $ selectOne $ do
        x <- from $ table @ContactUs
        where_ $ x ^. ContactUsId ==. val xid
        return x
    ((fr,fw),et) <- runFormPost $ formContact bid info
    case fr of
      FormSuccess r -> do
          runDB $ replace xid r
          addMessageI "info" MsgRecordEdited
          redirect $ AdminR $ BusinessContactR bid
      _ -> defaultLayout $ do
          setTitleI MsgContactUs
          $(widgetFile "admin/business/contact/edit")


getBusinessContactEditR :: BusinessId -> ContactUsId -> Handler Html
getBusinessContactEditR bid xid = do
    info <- runDB $ selectOne $ do
        x <- from $ table @ContactUs
        where_ $ x ^. ContactUsId ==. val xid
        return x
    (fw,et) <- generateFormPost $ formContact bid info
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "admin/business/contact/edit")


postBusinessContactR :: BusinessId -> Handler Html
postBusinessContactR bid = do
    ((fr,fw),et) <- runFormPost $ formContact bid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR $ BusinessContactR bid
      _ -> defaultLayout $ do
          setTitleI MsgAboutUs
          $(widgetFile "admin/business/contact/create")


getBusinessContactCreateR :: BusinessId -> Handler Html
getBusinessContactCreateR bid = do
    (fw,et) <- generateFormPost $ formContact bid Nothing
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "admin/business/contact/create")


formContact :: BusinessId -> Maybe (Entity ContactUs)
            -> Html -> MForm Handler (FormResult ContactUs, Widget)
formContact bid e extra = do
    (showScheduleR,showScheduleV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgShowSchedule
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (contactUsShowSchedule . entityVal <$> e)
    (htmlR,htmlV) <- mreq uniqueField FieldSettings
        { fsLabel = SomeMessage MsgContent
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("rows","12")]
        } ( contactUsHtml . entityVal <$> e)
    let r = ContactUs bid <$> showScheduleR <*> htmlR
    let v = [whamlet|
#{extra}

<div.form-field.mdc-form-field data-mdc-auto-init=MDCFormField style="line-height:4;display:flex;flex-direction:row">
  ^{fvInput showScheduleV}
  $with selected <- fromMaybe False ((contactUsShowSchedule . entityVal) <$> e)
    <button.mdc-switch type=button role=switch #switchShowSchedule data-mdc-auto-init=MDCSwitch
      :selected:.mdc-switch--selected :selected:aria-checked=true
      :not selected:.mdc-switch--unselected :not selected:aria-checked=false
      onclick="document.getElementById('#{fvId showScheduleV}').checked = !this.MDCSwitch.selected">
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
    <label for=switchShowSchedule>_{MsgShowSchedule}
    
<div.form-field>
  <label.mdc-text-field.mdc-text-field--filled.mdc-text-field--textarea data-mdc-auto-init=MDCTextField
    :isJust (fvErrors htmlV):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>_{MsgContent}
    <span.mdc-text-field__resizer>
      ^{fvInput htmlV}
    <spam.mdc-line-ripple>
  $maybe errs <- fvErrors htmlV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
|]
    return (r,v)
  where
      
    uniqueField = checkM uniqueContactUs htmlField

    uniqueContactUs :: Html -> Handler (Either AppMessage Html)
    uniqueContactUs html = do
        case e of
          Just _ -> return $ Right html
          Nothing -> do
              x <- runDB $ selectOne $ do
                  y <- from $ table @ContactUs
                  where_ $ y ^. ContactUsBusiness ==. val bid
                  return y
              case x of
                Just _ -> return $ Left MsgAlreadyExists
                Nothing -> return $ Right html


getBusinessContactR :: BusinessId -> Handler Html
getBusinessContactR bid = do
    user <- maybeAuth

    info <- runDB $ selectOne $ do
        x <- from $ table @ContactUs
        where_ $ x ^. ContactUsBusiness ==. val bid
        return x

    curr <- getCurrentRoute
    msgs <- getMessages
    formContactUsDelete <- newIdent
    dlgContactUsDelete <- newIdent
    (fw,et) <- generateFormPost formDelete
    defaultLayout $ do
        setTitleI MsgContactUs
        $(widgetFile "admin/business/contact/contacts")


postBusinessAboutDeleteR :: BusinessId -> AboutUsId -> Handler Html
postBusinessAboutDeleteR bid xid = do
    ((fr,_),_) <- runFormPost formDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete xid
          addMessageI "info" MsgRecordDeleted
          redirect $ AdminR $ BusinessAboutR bid
      _ -> do
          addMessageI "warn" MsgInvalidFormData
          redirect $ AdminR $ BusinessAboutR bid


formDelete :: Html -> MForm Handler (FormResult (),Widget)
formDelete extra = return (FormSuccess (),[whamlet|#{extra}|])


postBusinessAboutEditR :: BusinessId -> AboutUsId -> Handler Html
postBusinessAboutEditR bid xid = do
    info <- runDB $ selectOne $ do
        x <- from $ table @AboutUs
        where_ $ x ^. AboutUsId ==. val xid
        return x
    ((fr,fw),et) <- runFormPost $ formAbout bid info
    case fr of
      FormSuccess r -> do
          runDB $ replace xid r
          addMessageI "info" MsgRecordEdited
          redirect $ AdminR $ BusinessAboutR bid
      _ -> defaultLayout $ do
          setTitleI MsgAboutUs
          $(widgetFile "admin/business/about/edit")


getBusinessAboutEditR :: BusinessId -> AboutUsId -> Handler Html
getBusinessAboutEditR bid xid = do
    info <- runDB $ selectOne $ do
        x <- from $ table @AboutUs
        where_ $ x ^. AboutUsId ==. val xid
        return x
    (fw,et) <- generateFormPost $ formAbout bid info
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "admin/business/about/edit")


postBusinessAboutR :: BusinessId -> Handler Html
postBusinessAboutR bid = do
    ((fr,fw),et) <- runFormPost $ formAbout bid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR $ BusinessAboutR bid
      _ -> defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "admin/business/about/create")


getBusinessAboutCreateR :: BusinessId -> Handler Html
getBusinessAboutCreateR bid = do
    (fw,et) <- generateFormPost $ formAbout bid Nothing
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "admin/business/about/create")


formAbout :: BusinessId -> Maybe (Entity AboutUs)
          -> Html -> MForm Handler (FormResult AboutUs, Widget)
formAbout bid e extra = do
    (htmlR,htmlV) <- mreq uniqueField FieldSettings
        { fsLabel = SomeMessage MsgContent
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("rows","12")]
        } ( aboutUsHtml . entityVal <$> e)
    let r = AboutUs bid <$> htmlR
    let v = [whamlet|
#{extra}
<div.form-field>
  <label.mdc-text-field.mdc-text-field--filled.mdc-text-field--textarea data-mdc-auto-init=MDCTextField
    :isJust (fvErrors htmlV):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>_{MsgContent}
    <span.mdc-text-field__resizer>
      ^{fvInput htmlV}
    <spam.mdc-line-ripple>
  $maybe errs <- fvErrors htmlV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
|]
    return (r,v)
  where
      
    uniqueField = checkM uniqueAboutUs htmlField

    uniqueAboutUs :: Html -> Handler (Either AppMessage Html)
    uniqueAboutUs html = do
        case e of
          Just _ -> return $ Right html
          Nothing -> do
              x <- runDB $ selectOne $ do
                  y <- from $ table @AboutUs
                  where_ $ y ^. AboutUsBusiness ==. val bid
                  return y
              case x of
                Just _ -> return $ Left MsgAlreadyExists
                Nothing -> return $ Right html


getBusinessAboutR :: BusinessId -> Handler Html
getBusinessAboutR bid = do
    user <- maybeAuth

    info <- runDB $ selectOne $ do
        x <- from $ table @AboutUs
        where_ $ x ^. AboutUsBusiness ==. val bid
        return x

    curr <- getCurrentRoute
    msgs <- getMessages
    formAboutUsDelete <- newIdent
    dlgAboutUsDelete <- newIdent
    (fw,et) <- generateFormPost formDelete
    defaultLayout $ do
        setTitleI MsgContactUs
        $(widgetFile "admin/business/about/about")


getBusinessCalendarSlotR :: BusinessId -> Day -> BusinessHoursId -> Handler Html
getBusinessCalendarSlotR bid day sid = do
    slot <- runDB $ selectOne $ do
        x <- from $ table @BusinessHours
        where_ $ x ^. BusinessHoursId ==. val sid
        return x
    dlgSlotDelete <- newIdent
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/schedule/calendar/slots/slot")


postBusinessCalendarSlotDeleteR :: BusinessId -> BusinessHoursId -> Day -> Handler Html
postBusinessCalendarSlotDeleteR bid sid day = do
    runDB $ P.delete sid
    addMessageI "info" MsgRecordDeleted
    redirect (AdminR $ BusinessCalendarR bid ((\(y,m,_) -> YearMonth y m) . toGregorian $ day))


postBusinessCalendarSlotEditR :: BusinessId -> Day -> BusinessHoursId -> Handler Html
postBusinessCalendarSlotEditR bid day sid = do
    ((fr,fw),et) <- runFormPost $ formSlot bid day Nothing
    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
          addMessageI "info" MsgRecordEdited
          redirect (AdminR $ BusinessCalendarSlotsR bid day)
      _ -> do
          timeDay <- newIdent
          defaultLayout $ do
              setTitleI MsgBusinessHours
              $(widgetFile "admin/business/schedule/calendar/slots/edit")


getBusinessCalendarSlotEditR :: BusinessId -> Day -> BusinessHoursId -> Handler Html
getBusinessCalendarSlotEditR bid day sid = do
    slot <- runDB $ selectOne $ do
        x <- from $ table @BusinessHours
        where_ $ x ^. BusinessHoursId ==. val sid
        return x
    (fw,et) <- generateFormPost $ formSlot bid day slot
    timeDay <- newIdent
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/schedule/calendar/slots/edit")


postBusinessCalendarSlotCreateR :: BusinessId -> Day -> Handler Html
postBusinessCalendarSlotCreateR bid day = do
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day
    ((fr,fw),et) <- runFormPost $ formSlot bid day Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI "info" MsgRecordAdded
          redirect (AdminR $ BusinessCalendarR bid month)
      _ -> do
          timeDay <- newIdent
          defaultLayout $ do
              setTitleI MsgBusinessHours
              $(widgetFile "admin/business/schedule/calendar/slots/create")


getBusinessCalendarSlotCreateR :: BusinessId -> Day -> Handler Html
getBusinessCalendarSlotCreateR bid day = do
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day
    (fw,et) <- generateFormPost $ formSlot bid day Nothing
    timeDay <- newIdent
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/schedule/calendar/slots/create")


formSlot :: BusinessId -> Day -> Maybe (Entity BusinessHours)
         -> Html -> MForm Handler (FormResult BusinessHours,Widget)
formSlot bid day slot extra = do
    (startR,startV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgStartTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessHoursOpen . entityVal <$> slot)

    (endR,endV) <- mreq (afterTimeField startR) FieldSettings
        { fsLabel = SomeMessage MsgEndTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessHoursClose . entityVal <$> slot)

    (typeR,typeV) <- first (read . unpack <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgDayType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (pack . show <$> ((businessHoursDayType . entityVal <$> slot) <|> pure Weekday))
    let r = BusinessHours bid day <$> startR <*> endR <*> typeR
    let w = $(widgetFile "admin/business/schedule/calendar/slots/form")
    return (r,w)
  where

      afterTimeField :: FormResult TimeOfDay -> Field Handler TimeOfDay
      afterTimeField startR = check (afterTime startR) timeField

      afterTime :: FormResult TimeOfDay -> TimeOfDay -> Either AppMessage TimeOfDay
      afterTime startR x = case startR of
          FormSuccess s | x > s -> Right x
                        | otherwise -> Left MsgInvalidTimeInterval
          _ -> Right x


getBusinessCalendarSlotsR :: BusinessId -> Day -> Handler Html
getBusinessCalendarSlotsR bid day = do
    slots <- runDB $ select $ do
        x <- from $ table @BusinessHours
        where_ $ x ^. BusinessHoursDay ==. val day
        return x
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian
    fabSlotCreate <- newIdent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusinessDay
        $(widgetFile "admin/business/schedule/calendar/slots/slots")


getBusinessCalendarR :: BusinessId -> Month -> Handler Html
getBusinessCalendarR bid month = do

    slots <- M.fromListWith (+) . (diff <$>) <$> runDB ( select ( do
        x <- from $ table @BusinessHours
        where_ $ x ^. BusinessHoursDay `between` (val (periodFirstDay month), val (periodLastDay month))
        return ( (x ^. BusinessHoursDay,x ^. BusinessHoursDayType)
               , (x ^. BusinessHoursOpen, x ^. BusinessHoursClose)
               ) ) )

    app <- getYesod
    langs <- languages
    user <- maybeAuth
    curr <- getCurrentRoute
    setUltDestCurrent
    msgs <- getMessages

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month
    today <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    defaultLayout $ do
        setTitleI MsgBusinessDays
        $(widgetFile "/admin/business/schedule/calendar/calendar")

  where
      diff ((Value d,Value t),(Value o,Value c)) = ((d,t),diffLocalTime (LocalTime d c) (LocalTime d o))

postBusinessTimeSlotR :: BusinessId -> BusinessHoursId -> Handler Html
postBusinessTimeSlotR bid sid = do
    ((fr,fw),et) <- runFormPost $ formHours bid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
          addMessageI "info" MsgRecordEdited
          redirect (AdminR $ BusinessTimeSlotR bid sid)
      _ -> defaultLayout $ do
          setTitleI MsgBusinessHours
          $(widgetFile "admin/business/schedule/hours/edit")


getBusinessHoursEditR :: BusinessId -> BusinessHoursId -> Handler Html
getBusinessHoursEditR bid sid = do
    slot <- runDB $ selectOne $ do
        x <- from $ table @BusinessHours
        where_ $ x ^. BusinessHoursId ==. val sid
        return x
    (fw,et) <- generateFormPost $ formHours bid slot
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/schedule/hours/edit")


postBusinessTimeSlotDeleteR :: BusinessId -> BusinessHoursId -> Handler Html
postBusinessTimeSlotDeleteR bid sid = do
    runDB $ P.delete sid
    addMessageI "info" MsgRecordDeleted
    redirect (AdminR $ BusinessHoursR bid)


getBusinessTimeSlotR :: BusinessId -> BusinessHoursId -> Handler Html
getBusinessTimeSlotR bid sid = do
    slot <- runDB $ selectOne $ do
        x <- from $ table @BusinessHours
        where_ $ x ^. BusinessHoursId ==. val sid
        return x
    dlgSlotDelete <- newIdent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/schedule/hours/slot")


getBusinessHoursCreateR :: BusinessId -> Handler Html
getBusinessHoursCreateR bid = do
    (fw,et) <- generateFormPost $ formHours bid Nothing
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/schedule/hours/create")


formHours :: BusinessId -> Maybe (Entity BusinessHours)
          -> Html -> MForm Handler (FormResult BusinessHours,Widget)
formHours bid slot extra = do
    (dayR,dayV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgDay
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessHoursDay . entityVal <$> slot)
    (startR,startV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgStartTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessHoursOpen . entityVal <$> slot)

    (endR,endV) <- mreq (afterTimeField startR) FieldSettings
        { fsLabel = SomeMessage MsgEndTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessHoursClose . entityVal <$> slot)

    (typeR,typeV) <- first (read . unpack <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgDayType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (pack . show <$> ((businessHoursDayType . entityVal <$> slot) <|> pure Weekday))
    let r = BusinessHours bid <$> dayR <*> startR <*> endR <*> typeR
    let w = $(widgetFile "admin/business/schedule/hours/form")
    return (r,w)
  where

      afterTimeField :: FormResult TimeOfDay -> Field Handler TimeOfDay
      afterTimeField startR = check (afterTime startR) timeField

      afterTime :: FormResult TimeOfDay -> TimeOfDay -> Either AppMessage TimeOfDay
      afterTime startR x = case startR of
          FormSuccess s | x > s -> Right x
                        | otherwise -> Left MsgInvalidTimeInterval
          _ -> Right x


postBusinessHoursR :: BusinessId -> Handler Html
postBusinessHoursR bid = do
    ((fr,fw),et) <- runFormPost $ formHours bid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR $ BusinessHoursR bid
      _ -> defaultLayout $ do
          setTitleI MsgBusinessHours
          $(widgetFile "admin/business/schedule/hours/create")


getBusinessHoursR :: BusinessId -> Handler Html
getBusinessHoursR bid = do
    sort <- fromMaybe SortOrderDesc . (readMaybe . unpack =<<) <$> runInputGet (iopt textField "sort")
    slots <- runDB $ select $ do
        x <- from $ table @BusinessHours
        case sort of
          SortOrderAsc -> orderBy [asc (x ^. BusinessHoursDay), asc (x ^. BusinessHoursOpen)]
          _            -> orderBy [desc (x ^. BusinessHoursDay), desc (x ^. BusinessHoursOpen)]
        return x
    user <- maybeAuth
    curr <- getCurrentRoute
    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    setUltDestCurrent
    msgs <- getMessages
    toolbarTop <- newIdent
    fabBusinessHoursCreate <- newIdent
    defaultLayout $ do
        setTitleI MsgBusinessDays
        $(widgetFile "/admin/business/schedule/hours/hours")


postBusinessDeleteR :: Handler Html
postBusinessDeleteR = do
    runDB $ delete $ void $ from (table @Business)
    addMessageI "info" MsgRecordDeleted
    redirect $ AdminR BusinessR


postBusinessEditR :: BusinessId -> Handler Html
postBusinessEditR bid = do
    business <- runDB $ selectOne $ do
        x <- from $ table @Business
        where_ $ x ^. BusinessId ==. val bid
        return x
    ((fr,fw),et) <- runFormPost $ formBusiness business
    case fr of
      FormSuccess (Business name fname curr address tzo tz phone mobile email) -> do
          runDB $ update $ \x -> do
              set x [ BusinessName =. val name
                    , BusinessFullName =. val fname
                    , BusinessCurrency =. val curr
                    , BusinessAddr =. val address
                    , BusinessTzo =. val tzo
                    , BusinessTz =. val tz
                    , BusinessPhone =. val phone
                    , BusinessMobile =. val mobile
                    , BusinessEmail =. val email
                    ]
              where_ $ x ^. BusinessId ==. val bid
          addMessageI "info" MsgRecordEdited
          redirect $ AdminR BusinessR
      _ -> defaultLayout $ do
          setTitleI MsgBusiness
          $(widgetFile "admin/business/edit")


getBusinessEditR :: BusinessId -> Handler Html
getBusinessEditR bid = do
    business <- runDB $ selectOne $ from $ table @Business
    (fw,et) <- generateFormPost $ formBusiness business
    defaultLayout $ do
        setTitleI MsgBusiness
        $(widgetFile "admin/business/edit")


getBusinessCreateR :: Handler Html
getBusinessCreateR = do
    (fw,et) <- generateFormPost $ formBusiness Nothing
    defaultLayout $ do
        setTitleI MsgBusiness
        $(widgetFile "admin/business/create")


postBusinessR :: Handler Html
postBusinessR = do
    ((fr,fw),et) <- runFormPost $ formBusiness Nothing
    business <- runDB $ selectOne $ from $ table @Business
    case (fr,business) of
      (FormSuccess r,Nothing) -> do
          runDB $ insert_ r
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR BusinessR
      (_,Just _) -> do
          addMessageI "warn" MsgBusinessAlreadyExists
          redirect $ AdminR BusinessR
      _ -> defaultLayout $ do
          setTitleI MsgBusiness
          $(widgetFile "admin/business/create")


getBusinessR :: Handler Html
getBusinessR = do
    user <- maybeAuth
    month <- (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$> liftIO getCurrentTime
    business <- runDB $ selectOne $ from $ table @Business
    curr <- getCurrentRoute
    setUltDestCurrent
    msgs <- getMessages
    dlgBusinessDelete <- newIdent
    formBusinessDelete <- newIdent
    defaultLayout $ do
        setTitleI MsgBusiness
        $(widgetFile "admin/business/business")


formBusiness :: Maybe (Entity Business) -> Html -> MForm Handler (FormResult Business, Widget)
formBusiness business extra = do
    datalistTz <- newIdent
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessName . entityVal <$> business)
    (fnameR,fnameV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgTheFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessFullName . entityVal <$> business)
    (currR,currV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgCurrency
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessCurrency . entityVal <$> business)
    (addrR,addrV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessAddr . entityVal <$> business)
    (tzR,tzV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTimeZone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("list",datalistTz)]
        } (businessTz . entityVal <$> business)
    (tzoR,tzoV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgTimeZoneOffset
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (timeZoneMinutes . businessTzo . entityVal <$> business)
    (phoneR,phoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessPhone . entityVal <$> business)
    (mobileR,mobileV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgMobile
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessMobile . entityVal <$> business)
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessEmail . entityVal <$> business)

    let r = Business <$> nameR <*> fnameR <*> currR <*> addrR
            <*> (minutesToTimeZone <$> tzoR) <*> tzR <*> phoneR <*> mobileR <*> emailR
    return (r,$(widgetFile "admin/business/form"))
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Business
              where_ $ x ^. BusinessName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity eid _) -> case business of
              Nothing -> Left MsgBusinessAlreadyExists
              Just (Entity eid' _) | eid == eid' -> Right name
                                   | otherwise -> Left MsgBusinessAlreadyExists


fullHours :: NominalDiffTime -> NominalDiffTime -> Bool
fullHours x y = mod' x y == 0
