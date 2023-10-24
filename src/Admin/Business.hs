{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Maybe (isNothing, isJust)
import Data.Text (Text, pack, unpack)
import Data.Time.LocalTime (TimeZone(timeZoneMinutes), minutesToTimeZone)
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, SomeMessage (SomeMessage)
    , redirect, addMessageI, newIdent
    )
import Yesod.Core.Handler (setUltDestCurrent, getCurrentRoute)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Form.Fields
    ( textField, emailField, textareaField, intField, dayField, timeField
    , hiddenField
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost, checkM)
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
    , orderBy, desc, asc
    )

import Foundation
    ( Handler, Widget
    , Route (ProfileR, AccountPhotoR, PhotoPlaceholderR, AuthR, AdminR)
    , AdminR
      ( BusinessR, BusinessCreateR, BusinessEditR, BusinessDeleteR
      , BusinessHoursR, BusinessHoursCreateR, BusinessTimeSlotR
      , BusinessTimeSlotDeleteR, BusinessHoursEditR
      )
    , AppMessage
      ( MsgBusiness, MsgPhoto, MsgNoBusinessYet, MsgTheName, MsgAddress
      , MsgPhone, MsgMobile, MsgEmail, MsgSave, MsgCancel, MsgRecordAdded
      , MsgYesDelete, MsgDeleteAreYouSure, MsgPleaseConfirm, MsgRecordEdited
      , MsgRecordDeleted, MsgBusinessAlreadyExists, MsgTimeZoneOffset, MsgTimeZone
      , MsgMinutes, MsgLogin, MsgUserProfile, MsgNavigationMenu, MsgDel, MsgEdit
      , MsgBack, MsgTheFullName, MsgCurrency, MsgBusinessDays, MsgDetails, MsgDay
      , MsgNoBusinessScheduleYet, MsgBusinessHours, MsgStartTime, MsgEndTime
      , MsgDayType, MsgWeekday, MsgWeekend, MsgHoliday
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
      , BusinessHoursDay, BusinessHoursOpen, BusinessHoursId
      )
    , DayType (Weekday, Weekend, Holiday)
    )

import Settings (widgetFile)
import Menu (menu)


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
    (endR,endV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgEndTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessHoursClose . entityVal <$> slot)
    (typeR,typeV) <- first (read . unpack <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgDayType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (pack . show <$> ((businessHoursDayType . entityVal <$> slot) <|> pure Weekday))
    let r = BusinessHours bid <$> dayR <*> startR <*> endR <*> typeR
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

<div.form-field>
  <div.mdc-select.mdc-select--filled.mdc-select--required.mt-1 data-mdc-auto-init=MDCSelect
    :isJust (fvErrors typeV):.mdc-select--invalid>
    ^{fvInput typeV}
    <div.mdc-select__anchor role=button aria-haspopup=listbox aria-expanded=false>
      <span.mdc-select__ripple>
      <span.mdc-floating-label>#{fvLabel typeV}
      <span.mdc-select__selected-text-container>
        <span.mdc-select__selected-text>
      <span.mdc-select__dropdown-icon>
        <svg.mdc-select__dropdown-icon-graphic viewBox="7 10 10 5" focusable=false>
          <polygon.mdc-select__dropdown-icon-inactive stroke=none fill-rule=evenodd points="7 10 12 15 17 10">
          <polygon.mdc-select__dropdown-icon-active stroke=none fill-rule=evenodd points="7 15 12 10 17 15">
      <span.mdc-line-ripple>

    <div.mdc-select__menu.mdc-menu.mdc-menu-surface.mdc-menu-surface--fullwidth>
      $with options <- [(Weekday,MsgWeekday),(Weekend,MsgWeekend),(Holiday,MsgHoliday)]
        <ul.mdc-deprecated-list role=listbox>
          $forall (v,l) <- ((<$>) (first (pack . show)) options)
            <li.mdc-deprecated-list-item role=option data-value=#{v} aria-selected=false>
              <span.mdc-deprecated-list-item__ripple>
              <span.mdc-deprecated-list-item__text>
                _{l}
  $maybe errs <- fvErrors typeV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
|]
    return (r,w)


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
    slots <- runDB $ select $ do
        x <- from $ table @BusinessHours
        orderBy [desc (x ^. BusinessHoursDay), asc (x ^. BusinessHoursOpen)]
        return x
    user <- maybeAuth
    curr <- getCurrentRoute
    msgs <- getMessages
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
