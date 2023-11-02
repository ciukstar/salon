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
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields
    ( textField, emailField, textareaField, intField, dayField, timeField
    , hiddenField
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
      , MsgSortDescending
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
    , EntityField
      ( BusinessName, BusinessFullName, BusinessAddr, BusinessPhone, BusinessMobile
      , BusinessEmail, BusinessId, BusinessTzo, BusinessTz, BusinessCurrency
      , BusinessHoursId, BusinessHoursDay, BusinessHoursOpen, BusinessHoursClose
      , BusinessHoursDayType
      )
    , DayType (Weekday, Weekend, Holiday)
    , SortOrder (SortOrderAsc, SortOrderDesc)
    )

import Settings (widgetFile)
import Menu (menu)


getBusinessCalendarSlotR :: BusinessId -> Day -> BusinessHoursId -> Handler Html
getBusinessCalendarSlotR bid day sid = do
    slot <- runDB $ selectOne $ do
        x <- from $ table @BusinessHours
        where_ $ x ^. BusinessHoursId ==. val sid
        return x
    dlgSlotDelete <- newIdent
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/calendar/slots/slot")


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
              $(widgetFile "admin/business/calendar/slots/edit")


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
        $(widgetFile "admin/business/calendar/slots/edit")


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
              $(widgetFile "admin/business/calendar/slots/create")


getBusinessCalendarSlotCreateR :: BusinessId -> Day -> Handler Html
getBusinessCalendarSlotCreateR bid day = do
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian $ day
    (fw,et) <- generateFormPost $ formSlot bid day Nothing
    timeDay <- newIdent
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/calendar/slots/create")


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
    let w = $(widgetFile "admin/business/calendar/slots/form")
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
        $(widgetFile "admin/business/calendar/slots/slots")


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
        $(widgetFile "/admin/business/calendar/calendar")
        
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
          $(widgetFile "admin/business/hours/edit")


getBusinessHoursEditR :: BusinessId -> BusinessHoursId -> Handler Html
getBusinessHoursEditR bid sid = do
    slot <- runDB $ selectOne $ do
        x <- from $ table @BusinessHours
        where_ $ x ^. BusinessHoursId ==. val sid
        return x
    (fw,et) <- generateFormPost $ formHours bid slot
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/hours/edit")


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
        $(widgetFile "admin/business/hours/slot")


getBusinessHoursCreateR :: BusinessId -> Handler Html
getBusinessHoursCreateR bid = do
    (fw,et) <- generateFormPost $ formHours bid Nothing
    defaultLayout $ do
        setTitleI MsgBusinessHours
        $(widgetFile "admin/business/hours/create")


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
    let w = $(widgetFile "admin/business/hours/form")
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
          $(widgetFile "admin/business/hours/create")


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
        $(widgetFile "/admin/business/hours/hours")


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

