{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Admin.Contacts
  ( getAdmContactsR
  , postAdmContactsR
  , getAdmContactsCreateR
  , getAdmContactsEditR
  , postAdmContactsEditR
  , postAdmContactsDeleteR
  ) where

import Text.Hamlet (Html)
import Data.Text (Text)
import Data.Maybe (isJust)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvErrors, fvInput)
    )
import Yesod.Form.Functions (checkM, mreq, generateFormPost, runFormPost)
import Yesod.Core.Handler (setUltDestCurrent, getMessages, addMessageI, redirect)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, preEscapedToMarkup, whamlet
    , SomeMessage (SomeMessage)
    )
import Yesod.Form.Fields (unTextarea, Textarea, textareaField)
import Settings (widgetFile)

import Foundation
    ( Handler, Widget
    , Route (AdminR, AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AdminR (AdmContactsR, AdmContactsCreateR, AdmContactsEditR, AdmContactsDeleteR)
    , AppMessage
      ( MsgContact, MsgNoContentYet, MsgLogout, MsgPhoto
      , MsgYesDelete, MsgCancel, MsgDeleteAreYouSure, MsgPleaseConfirm
      , MsgAlreadyExists, MsgContent, MsgCancel, MsgSave
      , MsgRecordDeleted, MsgRecordEdited, MsgRecordAdded
      )
    )

import Database.Persist (Entity (Entity, entityVal))
import Database.Persist.Sql (PersistStoreWrite(insert_, replace, delete))
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
    
import Model
    ( ContentsId, Contents (Contents, contentsContent)
    , EntityField (ContentsSection)
    )


postAdmContactsDeleteR :: ContentsId -> Handler Html
postAdmContactsDeleteR cid = do
    runDB $ delete cid
    addMessageI "info" MsgRecordDeleted
    redirect $ AdminR AdmContactsR


postAdmContactsEditR :: ContentsId -> Handler Html
postAdmContactsEditR cid = do
    content <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    ((fr,fw),et) <- runFormPost $ formContacts content
    case fr of
      FormSuccess r -> do
          runDB $ replace cid r
          addMessageI "info" MsgRecordEdited
          redirect $ AdminR AdmContactsR
      _ -> defaultLayout $ do
          setTitleI MsgContact
          $(widgetFile "admin/contacts/edit")


getAdmContactsEditR :: ContentsId -> Handler Html
getAdmContactsEditR cid = do
    content <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    (fw,et) <- generateFormPost $ formContacts content
    defaultLayout $ do
        setTitleI MsgContact
        $(widgetFile "admin/contacts/edit")


getAdmContactsCreateR :: Handler Html
getAdmContactsCreateR = do
    (fw,et) <- generateFormPost $ formContacts Nothing
    defaultLayout $ do
        setTitleI MsgContact
        $(widgetFile "admin/contacts/create")


postAdmContactsR :: Handler Html
postAdmContactsR = do
    ((fr,fw),et) <- runFormPost $ formContacts Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR AdmContactsR
      _ -> defaultLayout $ do
          setTitleI MsgContact
          $(widgetFile "admin/contacts/create")


getAdmContactsR :: Handler Html
getAdmContactsR = do
    muid <- maybeAuth
    contents <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgContact
        $(widgetFile "admin/contacts/contacts")


formContacts :: Maybe (Entity Contents) -> Html -> MForm Handler (FormResult Contents, Widget)
formContacts c extra = do
    (cR,cV) <- mreq uniqueField FieldSettings
        { fsLabel = SomeMessage MsgContent
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("rows","12")]
        } ( contentsContent . entityVal <$> c)
    let r = Contents section <$> cR
    let v = [whamlet|
#{extra}
<div.form-field>
  <label.mdc-text-field.mdc-text-field--filled.mdc-text-field--textarea data-mdc-auto-init=MDCTextField
    :isJust (fvErrors cV):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>_{MsgContent}
    <span.mdc-text-field__resizer>
      ^{fvInput cV}
    <spam.mdc-line-ripple>
  $maybe errs <- fvErrors cV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
|]
    return (r,v)
  where
    uniqueField = checkM uniqueSection textareaField

    uniqueSection :: Textarea -> Handler (Either AppMessage Textarea)
    uniqueSection text = do
        case c of
          Just _ -> return $ Right text
          Nothing -> do
              x <- runDB $ selectOne $ do
                  y <- from $ table @Contents
                  where_ $ y ^. ContentsSection ==. val section
                  return y 
              case x of
                Just _ -> return $ Left MsgAlreadyExists
                Nothing -> return $ Right text

section :: Text
section = "CONTACTS"
