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
  , getUserPwdResetR
  , postUserPwdResetR
  , getUsersSearchR
  ) where

import Control.Monad (unless)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.List.Safe as LS (head)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Hamlet (Html)
import Yesod.Auth.Util.PasswordStore (makePassword)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Auth.HashDB (setPassword)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages, whamlet, SomeMessage (SomeMessage)
    , FileInfo (fileContentType), addMessageI, redirect
    , fileSourceByteString, MonadIO (liftIO)
    , RenderMessage (renderMessage), languages, getYesod
    , newIdent, YesodRequest (reqGetParams), getRequest
    )
import Yesod.Form.Types
    ( FormResult (FormSuccess, FormFailure), MForm
    , FieldView (fvInput, fvLabel, fvId, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost, checkM)
import Yesod.Form.Fields
    (textField, passwordField, emailField, fileField, searchField, checkBoxField)
import Yesod.Form.Input (iopt, runInputGet)
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
    , (^.), (==.), (=.), (%), (++.), (||.)
    , selectOne, val, update, set, upper_, like, just, not_, exists
    )

import Foundation
    ( Handler, Widget
    , Route
      ( ProfileR, AuthR, AdminR, PhotoPlaceholderR, AccountPhotoR, AdminR
      , StaticR
      )
    , AdminR
      ( UsersSearchR, UserCreateFormR, UsersR, UserR, UserEditFormR, UserDeleteR
      , UserPwdResetR
      )
    , AppMessage
      ( MsgUsers, MsgNoUsersYet, MsgPhoto, MsgSave, MsgBack, MsgDel, MsgEdit
      , MsgUser, MsgCancel, MsgUsername, MsgPassword, MsgFullName, MsgEmail
      , MsgRecordAdded, MsgAlreadyExists, MsgYesDelete, MsgDeleteAreYouSure
      , MsgPleaseConfirm, MsgRecordDeleted, MsgRecordEdited, MsgResetPassword
      , MsgConfirmPassword, MsgNewPassword, MsgPasswordsDoNotMatch
      , MsgPasswordChanged, MsgSearch, MsgNoUsersFound, MsgEmployee
      , MsgCustomer, MsgAdministrator, MsgYes, MsgNo, MsgStatus
      , MsgSelect, MsgNavigationMenu, MsgUserProfile, MsgLogin
      , MsgAnalyst, MsgBlocked, MsgRemoved, MsgRole, MsgRoles
      )
    )
import Model
    ( UserId
    , User
      ( User, userName, userFullName, userEmail, userAdmin
      , userAnalyst, userBlocked, userRemoved
      )
    , UserPhoto (UserPhoto)
    , EntityField
      ( UserId, UserName, UserFullName, UserEmail, UserPhotoPhoto
      , UserPhotoMime, UserPassword, UserAdmin, StaffUser, UserAnalyst
      , UserBlocked, UserRemoved, UserAuthType
      )
    , Staff, AuthenticationType (UserAuthTypePassword)
    )

import Settings.StaticFiles (img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg)

import Menu (menu)


getUsersSearchR :: Handler Html
getUsersSearchR = do
    mq <- runInputGet $ iopt (searchField True) "q"
    roles <-  (snd <$>) . filter ((== role) . fst) . reqGetParams <$> getRequest
    statuses <-  (snd <$>) . filter ((== status) . fst) . reqGetParams <$> getRequest
    
    users <- runDB $ select $ do
        x <- from $ table @User
        case mq of
          Just q -> where_ $ ( upper_ (x ^. UserName) `like` ((%) ++. upper_ (val q) ++. (%)) )
            ||. ( upper_ (x ^. UserFullName) `like` ((%) ++. upper_ (just (val q)) ++. (%)) )
            ||. ( upper_ (x ^. UserEmail) `like` ((%) ++. upper_ (just (val q)) ++. (%)) )
          Nothing -> return ()

        let ors1 = snd <$> filter ((`elem` roles) . fst)
                [ (customer,not_ (x ^. UserAdmin))
                , (employee, exists $ do
                        y <- from $ table @Staff
                        where_ $ y ^. StaffUser ==. just (x ^. UserId) )
                , (admin, x ^. UserAdmin)
                , (analyst, x ^. UserAnalyst)
                ]

        unless (null ors1) $ where_ $ foldr (||.) (val False) ors1

        let ors2 = snd <$> filter ((`elem` statuses) . fst)
                [ (blocked, x ^. UserBlocked)
                , (removed, x ^. UserRemoved)
                ]

        unless (null ors2) $ where_ $ foldr (||.) (val False) ors2
        
        orderBy [desc (x ^. UserId)]
        return x
    let userRoles = [ (customer, MsgCustomer)
                    , (employee, MsgEmployee)
                    , (admin, MsgAdministrator)
                    , (analyst, MsgAnalyst)
                    ]
                    
    formSearch <- newIdent
    toolbarTop <- newIdent
    dlgRoleList <- newIdent
    dlgStatusList <- newIdent
    
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "admin/users/search")

  where
      role :: Text
      role = "role"
      
      customer, employee, admin, analyst :: Text
      customer = "customer"
      employee = "employee"
      admin    = "admin"
      analyst  = "analyst"
                 
      status :: Text
      status = "status"

      blocked, removed :: Text
      blocked = "blocked"
      removed = "removed"


postUserPwdResetR :: UserId -> Handler Html
postUserPwdResetR uid = do
    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    ((fr,fw),et) <- runFormPost $ formPwdReset user
    case fr of
      FormSuccess (r,_) -> do
          pwd <- liftIO $ decodeUtf8 <$> makePassword (encodeUtf8 r) 17
          runDB $ update $ \x -> do
              set x [UserPassword =. val (pure pwd)]
              where_ $ x ^. UserId ==. val uid
          addMessageI "info" MsgPasswordChanged
          redirect $ AdminR $ UserR uid
      _ -> defaultLayout $ do
          setTitleI MsgResetPassword
          $(widgetFile "admin/users/pwdreset")


getUserPwdResetR :: UserId -> Handler Html
getUserPwdResetR uid = do
    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    (fw,et) <- generateFormPost $ formPwdReset user
    defaultLayout $ do
        setTitleI MsgResetPassword
        $(widgetFile "admin/users/pwdreset")


formPwdReset :: Maybe (Entity User) -> Html -> MForm Handler (FormResult (Text,Text),Widget)
formPwdReset user extra = do
    app <- getYesod
    langs <- languages 
    (fstR,fstV) <- mreq passwordField FieldSettings
        { fsLabel = SomeMessage MsgNewPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    (sndR,sndV) <- mreq passwordField FieldSettings
        { fsLabel = SomeMessage MsgConfirmPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    let r = case (fstR,sndR) of
          (FormSuccess a, FormSuccess b) | a /= b -> FormFailure [renderMessage app langs MsgPasswordsDoNotMatch]
          _ -> (,) <$> fstR <*> sndR
    let w = [whamlet|
#{extra}
$case r
  $of FormFailure errs
    $maybe err <- LS.head errs
      <div.mdc-banner role=banner data-mdc-auto-init=MDCBanner>
        <div.mdc-banner__content role=alertdialog aria-live=assertive>
          <div.mdc-banner__graphic-text-wrapper>
            <div.mdc-banner__graphic role=img style="background-color:var(--mdc-theme-error)">
              <i.mdc-banner__icon.material-symbols-outlined>warning
            <div.mdc-banner__text>
              #{err}
          <div.mdc-banner__actions>
            <button.mdc-banner__primary-action.mdc-icon-button type=button>
              <span.mdc-icon-button__ripple>
              <i.material-symbols-outlined>close            
  $of _
  
$maybe Entity uid (User name _ _ _ _ _ _ _ _) <- user
  <figure>
    <img src=@{AccountPhotoR uid} width=56 heigt=56 alt=_{MsgPhoto}>
    <figcaption>
      #{name}
      
$forall v <- [fstV,sndV]
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
      FormSuccess (User name auth _ admin analyst blocked removed fname email,mfi) -> do
          runDB $ update $ \x -> do
              set x [ UserName =. val name
                    , UserAuthType =. val auth
                    , UserAdmin =. val admin
                    , UserAnalyst =. val analyst
                    , UserBlocked =. val blocked
                    , UserRemoved =. val removed
                    , UserFullName =. val fname
                    , UserEmail =. val email
                    ]
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
    dlgUserDelete <- newIdent
    defaultLayout $ do
        setTitleI MsgUser
        $(widgetFile "admin/users/user")


postUsersR :: Handler Html
postUsersR = do
    ((fr,fw),et) <- runFormPost formUserCreate
    case fr of
      FormSuccess (r@(User _ _ (Just pass) _ _ _ _ _ _),mfi) -> do
          uid <- setPassword pass r >>= \u -> runDB $ insert u
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
    user <- maybeAuth
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
    (adminR,adminV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAdministrator
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (Just False)
    (analystR,analystV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAnalyst
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (pure False)
    (blockedR,blockedV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgBlocked
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (pure False)
    (removedR,removedV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgRemoved
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (pure False)
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

    let r = (,)
            <$> ( User <$> nameR <*> pure UserAuthTypePassword <*> (pure <$> passR)
                  <*> adminR <*> analystR <*> blockedR <*> removedR
                  <*> fnameR <*> emailR
                ) <*> photoR
    let w = [whamlet|
#{extra}
<div.form-field>
  <label for=#{fvId photoV} #labelPhotoUser>
    <figure #figurePhotoUser>
      <img src=@{StaticR img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg} #imgPhotoUser>
      <figcaption>
        _{MsgPhoto}
  ^{fvInput photoV}

$forall v <- [nameV,passV]
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

$forall v <- [fnameV,emailV]
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
          
$forall (r,v) <- [(adminR,adminV),(analystR,analystV),(blockedR,blockedV),(removedR,removedV)]
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
|]
    return (r,w)
  where

      resolveSelected adminR = case adminR of FormSuccess x -> x ; _ -> False
      
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
    (adminR,adminV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAdministrator
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (userAdmin . entityVal <$> user)
    (analystR,analystV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAnalyst
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (userAnalyst . entityVal <$> user)
    (blockedR,blockedV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgBlocked
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (userBlocked . entityVal <$> user)
    (removedR,removedV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgRemoved
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (userRemoved . entityVal <$> user)
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

    let r = (,)
            <$> ( User <$> nameR <*> pure UserAuthTypePassword <*> pure (pure "NOTHING")
                  <*> adminR <*> analystR <*> blockedR <*> removedR
                  <*> fnameR <*> emailR
                )
            <*> photoR
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

$forall v <- [fnameV,emailV]
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

$forall (r,v) <- [(userAdmin,adminV),(userAnalyst,analystV),(userBlocked,blockedV),(userRemoved,removedV)]
  <div.mdc-form-field.form-field data-mdc-auto-init=MDCFormField style="display:flex;flex-direction:row">
    ^{fvInput v}
    $with selected <- fromMaybe False ((r . entityVal) <$> user)
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
