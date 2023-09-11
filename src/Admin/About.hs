{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Admin.About
  ( getAdmAboutR
  , getAdmAboutCreateR
  , postAdmAboutR
  , getAdmAboutEditR
  , postAdmAboutEditR
  , postAdmAboutDeleteR
  ) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), preEscapedToMarkup, whamlet
    , SomeMessage (SomeMessage), getMessages, setUltDestCurrent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Fields (Textarea(unTextarea), textareaField)
import Yesod.Auth (Route (LoginR), maybeAuth)
import Settings (widgetFile)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Core.Handler (addMessageI, redirect)
import Yesod.Form.Functions (runFormPost, generateFormPost, mreq, checkM)

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity (Entity, entityVal))
import Database.Persist.Sql (PersistStoreWrite(insert_, replace, delete))
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
    
import Foundation
    ( Handler, Widget
    , Route (AdminR, AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR)
    , AdminR (AdmAboutCreateR, AdmAboutR, AdmAboutEditR, AdmAboutDeleteR)
    , AppMessage
      ( MsgAboutUs, MsgPhoto, MsgNoContentYet
      , MsgContent, MsgCancel, MsgSave, MsgRecordAdded, MsgAlreadyExists
      , MsgRecordEdited, MsgYesDelete, MsgPleaseConfirm, MsgDeleteAreYouSure
      , MsgRecordDeleted
      )
    )

import Model
    ( ContentsId, Contents(Contents, contentsContent)
    , EntityField (ContentsSection)
    )

import Menu (menu)


postAdmAboutDeleteR :: ContentsId -> Handler ()
postAdmAboutDeleteR cid = do
    runDB $ delete cid
    addMessageI "info" MsgRecordDeleted
    redirect $ AdminR AdmAboutR


postAdmAboutEditR :: ContentsId -> Handler Html
postAdmAboutEditR cid = do
    content <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    ((fr,fw),et) <- runFormPost $ formAbout content
    case fr of
      FormSuccess r -> do
          runDB $ replace cid r
          addMessageI "info" MsgRecordEdited
          redirect $ AdminR AdmAboutR
      _ -> defaultLayout $ do
          setTitleI MsgAboutUs
          $(widgetFile "admin/about/edit")
    

getAdmAboutEditR :: ContentsId -> Handler Html
getAdmAboutEditR cid = do
    content <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    (fw,et) <- generateFormPost $ formAbout content
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "admin/about/edit")


postAdmAboutR :: Handler Html
postAdmAboutR = do
    ((fr,fw),et) <- runFormPost $ formAbout Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR AdmAboutR
      _ -> defaultLayout $ do
          setTitleI MsgAboutUs
          $(widgetFile "admin/about/create")


getAdmAboutCreateR :: Handler Html
getAdmAboutCreateR = do
    (fw,et) <- generateFormPost $ formAbout Nothing
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "admin/about/create")


formAbout :: Maybe (Entity Contents) -> Html -> MForm Handler (FormResult Contents, Widget)
formAbout c extra = do
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


getAdmAboutR :: Handler Html
getAdmAboutR = do
    user <- maybeAuth
    contents <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "admin/about/about")

    
section :: Text
section = "ABOUT_US"
