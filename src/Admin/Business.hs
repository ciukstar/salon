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
    ( Yesod(defaultLayout), getMessages, whamlet, SomeMessage (SomeMessage)
    , redirect, addMessageI
    )
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Fields (textField, emailField, textareaField)
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
      , MsgRecordDeleted, MsgBusinessAlreadyExists
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
      ( Business, businessName, businessAddress, businessPhone, businessMobile
      , businessEmail
      )
    , BusinessId
    , EntityField
      ( BusinessName, BusinessAddress, BusinessPhone, BusinessMobile, BusinessEmail
      , BusinessId
      )
    )

import Menu (menu)


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
      FormSuccess (Business name address phone mobile email) -> do
          runDB $ update $ \x -> do
              set x [ BusinessName =. val name
                    , BusinessAddress =. val address
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
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessName . entityVal <$> business)
    (addrR,addrV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessAddress . entityVal <$> business)
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
    
    let r = Business <$> nameR <*> addrR <*> phoneR <*> mobileR <*> emailR
    let w = [whamlet|
#{extra}
<div.form-field>
  <div.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
    :isJust (fvErrors nameV):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel nameV}
    ^{fvInput nameV}
    <span.mdc-line-ripple>
  $maybe errs <- fvErrors nameV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
        
<div.form-field>
  <div.mdc-text-field.mdc-text-field--textarea.mdc-text-field--filled data-mdc-auto-init=MDCTextField
    :isJust (fvErrors addrV):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel addrV}
    <span.mdc-text-field__resizer>
      ^{fvInput addrV}
    <span.mdc-line-ripple>
  $maybe errs <- fvErrors addrV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
          
$forall v <- [phoneV,mobileV,emailV]
  <div.form-field>
    <div.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      <span.mdc-line-ripple>
    $maybe errs <- fvErrors v
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}
|]
    return (r,w)
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
