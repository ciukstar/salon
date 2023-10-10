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
  ) where

import Control.Monad (void)
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), getMessages, SomeMessage (SomeMessage)
    , redirect, addMessageI, newIdent
    )
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Fields (textField, emailField, textareaField, intField)
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost, checkM)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvLabel, fvInput, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , Field
    )

import Settings (widgetFile)

import Foundation
    ( Handler, Widget
    , Route (ProfileR, AccountPhotoR, PhotoPlaceholderR, AuthR, AdminR)
    , AdminR (BusinessR, BusinessCreateR, BusinessEditR, BusinessDeleteR)
    , AppMessage
      ( MsgBusiness, MsgPhoto, MsgNoBusinessYet, MsgTheName, MsgAddress
      , MsgPhone, MsgMobile, MsgEmail, MsgSave, MsgCancel, MsgRecordAdded
      , MsgYesDelete, MsgDeleteAreYouSure, MsgPleaseConfirm, MsgRecordEdited
      , MsgRecordDeleted, MsgBusinessAlreadyExists, MsgTimeZoneOffset, MsgTimeZone, MsgMinutes
      )
    )

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity (Entity, entityVal), PersistStoreWrite (insert_))
import Database.Esqueleto.Experimental
    ( selectOne, from, table, update, set, val, where_, delete
    , (=.), (^.), (==.)
    )

import Model
    ( Business
      ( Business, businessName, businessAddr, businessPhone, businessMobile
      , businessEmail, businessTzo, businessTz
      )
    , BusinessId
    , EntityField
      ( BusinessName, BusinessAddr, BusinessPhone, BusinessMobile, BusinessEmail
      , BusinessId, BusinessTzo, BusinessTz
      )
    )

import Menu (menu)
import Data.Time.LocalTime (TimeZone(timeZoneMinutes), minutesToTimeZone)


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
      FormSuccess (Business name address tzo tz phone mobile email) -> do
          runDB $ update $ \x -> do
              set x [ BusinessName =. val name
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
    setUltDestCurrent
    msgs <- getMessages
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

    let r = Business <$> nameR <*> addrR <*> (minutesToTimeZone <$> tzoR) <*> tzR <*> phoneR <*> mobileR <*> emailR
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
