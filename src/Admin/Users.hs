{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Admin.Users
  ( getUsersR
  , getUserCreateFormR
  , postUsersR
  , getUserR
  , postUserR
  , getUserEditFormR
  , postUserDeleteR
  ) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Auth.HashDB (setPassword)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages, whamlet, SomeMessage (SomeMessage)
    , FileInfo (fileContentType), addMessageI, redirect, fileSourceByteString
    )
import Yesod.Form.Types
    ( FormResult (FormSuccess), MForm, FieldView (fvInput, fvLabel, fvId, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost, checkM)
import Yesod.Form.Fields (textField, passwordField, emailField, fileField)
import Settings (widgetFile)

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (insert, insert_, delete)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.)) 
import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc, where_
    , (^.), (==.), (=.)
    , selectOne, val, update, set
    )

import Foundation
    ( Handler, Widget
    , Route (AuthR, AdminR, PhotoPlaceholderR, AccountPhotoR, AdminR, StaticR)
    , AdminR (UserCreateFormR, UsersR, UserR, UserEditFormR, UserDeleteR)
    , AppMessage
      ( MsgUsers, MsgNoUsersYet, MsgLogout, MsgPhoto, MsgSave
      , MsgUser, MsgCancel, MsgUsername, MsgPassword, MsgFullName, MsgEmail
      , MsgRecordAdded, MsgAlreadyExists, MsgYesDelete, MsgDeleteAreYouSure
      , MsgPleaseConfirm, MsgRecordDeleted, MsgRecordEdited, MsgResetPassword
      )
    )
import Model
    ( UserId, User(User, userName, userPassword, userFullName, userEmail)
    , EntityField (UserId, UserName, UserFullName, UserEmail, UserPhotoPhoto, UserPhotoMime)
    , UserPhoto (UserPhoto)
    )

import Settings.StaticFiles (img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg)


postUserDeleteR :: UserId -> Handler ()
postUserDeleteR uid = do
    runDB $ delete uid
    addMessageI "info" MsgRecordDeleted
    redirect $ AdminR UsersR


getUserEditFormR :: UserId -> Handler Html
getUserEditFormR uid = do
    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    (fw,et) <- generateFormPost $ formUserEdit user
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "admin/users/edit")


postUserR :: UserId -> Handler Html
postUserR uid = do
    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    ((fr,fw),et) <- runFormPost $ formUserEdit user
    case fr of
      FormSuccess (User name _ fname email,mfi) -> do
          runDB $ update $ \x -> do
              set x [UserName =. val name, UserFullName =. val fname, UserEmail =. val email]
              where_ $ x ^. UserId ==. val uid
          addMessageI "info" MsgRecordEdited
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                _ <- runDB $ upsert (UserPhoto uid bs (fileContentType fi))
                    [UserPhotoPhoto P.=. bs, UserPhotoMime P.=. fileContentType fi]
                return ()
            _ -> return ()
          redirect $ AdminR $ UserR uid
      _ -> defaultLayout $ do
          setTitleI MsgUser
          $(widgetFile "admin/users/edit")


getUserR :: UserId -> Handler Html
getUserR uid = do
    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "admin/users/user")


postUsersR :: Handler Html
postUsersR = do
    ((fr,fw),et) <- runFormPost formUserCreate
    case fr of
      FormSuccess (r,mfi) -> do
          uid <- setPassword (userPassword r) r >>= \u -> runDB $ insert u
          addMessageI "info" MsgRecordAdded
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                runDB $ insert_ $ UserPhoto uid bs (fileContentType fi)
            _ -> return ()
          redirect $ AdminR UsersR
      _ -> defaultLayout $ do
          setTitleI MsgUser
          $(widgetFile "admin/users/create")


getUserCreateFormR :: Handler Html
getUserCreateFormR = do
    (fw,et) <- generateFormPost formUserCreate
    defaultLayout $ do
        setTitleI MsgUsers
        $(widgetFile "admin/users/create")


getUsersR :: Handler Html
getUsersR = do
    muid <- maybeAuth
    users <- runDB $ select $ do
        x <- from $ table @User
        orderBy [desc (x ^. UserId)]
        return x
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgUsers
        $(widgetFile "admin/users/users")


formUserCreate :: Html -> MForm Handler (FormResult (User,Maybe FileInfo), Widget)
formUserCreate extra = do
    (nameR,nameV) <- mreq uniqueUsernameField FieldSettings
        { fsLabel = SomeMessage MsgUsername
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    (passR,passV) <- mreq passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    (fnameR,fnameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Just "inputPhotoUser", fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing
        
    let r = (,) <$> (User <$> nameR <*> passR <*> fnameR <*> emailR) <*> photoR
    let w = [whamlet|
#{extra}
<div.form-field>
  <label for=#{fvId photoV} #labelPhotoUser>
    <figure #figurePhotoUser>
      <img src=@{StaticR img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg} #imgPhotoUser>
      <figcaption>
        _{MsgPhoto}
  ^{fvInput photoV}
    
$forall v <- [nameV,passV,fnameV,emailV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      <div.mdc-line-ripple>
    $maybe errs <- fvErrors v
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}
|]
    return (r,w)
  where
      uniqueUsernameField = checkM uniqueUsername textField

      uniqueUsername :: Text -> Handler (Either AppMessage Text)
      uniqueUsername username = do
          mu <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserName ==. val username
              return x
          return $ case mu of
            Nothing -> Right username
            Just _ -> Left MsgAlreadyExists


formUserEdit :: Maybe (Entity User) -> Html -> MForm Handler (FormResult (User,Maybe FileInfo), Widget)
formUserEdit user extra = do
    (nameR,nameV) <- mreq uniqueUsernameField FieldSettings
        { fsLabel = SomeMessage MsgUsername
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userName . entityVal <$> user)
    (fnameR,fnameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userFullName . entityVal <$> user)
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userEmail . entityVal <$> user)        
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Just "inputPhotoUser", fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing
        
    let r = (,) <$> (User <$> nameR <*> FormSuccess "Nothing" <*> fnameR <*> emailR) <*> photoR
    let w = [whamlet|
#{extra}
<div.form-field>
  <label for=#{fvId photoV} #labelPhotoUser>
    <figure #figurePhotoUser>
      $maybe Entity uid _ <- user
        <img width=56 height=56 src=@{AccountPhotoR uid} alt=_{MsgPhoto} #imgPhotoUser>
      $nothing
        <img src=@{StaticR img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg} #imgPhotoUser>
      <figcaption>
        _{MsgPhoto}
  ^{fvInput photoV}
  
<div.form-field>
  <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
    :isJust (fvErrors nameV):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel nameV}
    ^{fvInput nameV}
    <div.mdc-line-ripple>
  $maybe errs <- fvErrors nameV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
  
$forall v <- [nameV,fnameV,emailV]  
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      <div.mdc-line-ripple>
    $maybe errs <- fvErrors v
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}
|]
    return (r,w)
  where
      uniqueUsernameField = checkM uniqueUsername textField

      uniqueUsername :: Text -> Handler (Either AppMessage Text)
      uniqueUsername username = do
          mu <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserName ==. val username
              return x
          return $ case user of
            Nothing -> case mu of
              Nothing -> Right username
              Just _ -> Left MsgAlreadyExists
            Just (Entity uid _) -> case mu of
              Nothing -> Right username
              Just (Entity uid' _) | uid == uid' -> Right username
                                   | otherwise -> Left MsgAlreadyExists
                
            
          
