{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Account
  ( getAccountR
  , postAccountR
  , getAccountPhotoR
  , getProfileR
  , getProfileEditR
  , postProfileR
  , postProfileRemoveR
  ) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T (null)
import Data.Maybe (fromMaybe, isJust)
import Text.Hamlet (Html)
import Data.FileEmbed (embedFile)
import Settings (widgetFile)

import Yesod.Core
    ( Yesod(defaultLayout)
    , setTitleI, getUrlRender, lookupSession, whamlet
    , SomeMessage (SomeMessage), FileInfo (fileContentType), ToWidget (toWidget)
    , julius, fileSourceByteString, redirect
    , TypedContent (TypedContent)
    , ToContent (toContent), typeSvg, newIdent, addMessageI, getMessages
    )
import Yesod.Core.Handler (setUltDest)    
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput, fvLabel, fvId, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , Field (Field, fieldParse, fieldEnctype, fieldView)
    , Enctype (UrlEncoded), FormMessage (MsgValueRequired)
    )
import Yesod.Form
    ( generateFormPost, mreq, textField, mopt
    , fileField, emailField, runFormPost
    )
import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)

import Foundation
    ( Handler, Widget
    , Route
      ( StaticR, AccountPhotoR, HomeR, AccountR, PhotoPlaceholderR
      , AuthR, AdminR, ProfileR, ProfileEditR, ProfileRemoveR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgAccount, MsgCancel, MsgUsername, MsgPassword, MsgPhoto, MsgFullName
      , MsgEmail, MsgSignUp, MsgBack, MsgConfirmPassword, MsgEmployee
      , MsgYouMustEnterTwoValues, MsgPasswordsDoNotMatch, MsgRegistration
      , MsgUserProfile, MsgLogout, MsgLogin, MsgLoginToSeeYourProfile, MsgRoles
      , MsgAdministrator, MsgAnalyst, MsgPleaseConfirm, MsgEdit, MsgSave
      , MsgRecordEdited, MsgRecordRemoved, MsgRemove, MsgRemoveProfileAreYouSure
      , MsgRemoveProfileHint
      )
    )

import Yesod.Auth.HashDB (setPassword)
import Yesod (YesodPersist(runDB), Entity (entityVal))
import Database.Persist (Entity (Entity), upsert, insert, insert_)
import qualified Database.Persist as P ((=.))

import Model
    ( ultDestKey
    , UserId, User (userName, User, userPassword, userFullName, userEmail)
    , UserPhoto (UserPhoto, userPhotoUser, userPhotoPhoto, userPhotoMime)
    , EntityField
      ( UserPhotoUser, StaffUser, RoleStaff, RoleName, UserId, UserFullName
      , UserEmail, UserPhotoPhoto, UserPhotoMime, UserRemoved
      )
    , Staff (Staff), Role
    )

import Database.Esqueleto.Experimental
    (Value (Value), selectOne, from, table, where_
    , (^.), (==.), (=.)
    , val, just, select, distinct, update, set
    )

import Settings.StaticFiles
    ( img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg )


postProfileRemoveR :: UserId -> Handler Html
postProfileRemoveR uid = do
    ((fr,fw),et) <- runFormPost formRemove
    case fr of
      FormSuccess _ -> do
          runDB $ update $ \x -> do
              set x [UserRemoved =. val True]
              where_ $ x ^. UserId ==. val uid
          addMessageI info MsgRecordRemoved
          redirect $ AuthR LogoutR
      _ -> defaultLayout $ do
          setTitleI MsgUserProfile
          $(widgetFile "profile/edit")


postProfileR :: UserId -> Handler Html
postProfileR uid = do
    ((fr,fw),et) <- runFormPost $ formProfile Nothing
    case fr of
      FormSuccess ((fname,email),mfi) -> do
          runDB $ update $ \x -> do
              set x [UserFullName =. val fname, UserEmail =. val email]
              where_ $ x ^. UserId ==. val uid
          addMessageI info MsgRecordEdited
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                _ <- runDB $ upsert (UserPhoto uid bs (fileContentType fi))
                    [UserPhotoPhoto P.=. bs, UserPhotoMime P.=. fileContentType fi]
                return ()
            _ -> return ()
          redirect $ ProfileR uid
      _ -> defaultLayout $ do
          setTitleI MsgUserProfile
          $(widgetFile "profile/edit")


getProfileEditR :: UserId -> Handler Html
getProfileEditR uid = do
    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    (fw,et) <- generateFormPost $ formProfile user
    defaultLayout $ do
        setTitleI MsgUserProfile
        $(widgetFile "profile/edit")


formProfile :: Maybe (Entity User)
            -> Html -> MForm Handler (FormResult ((Maybe Text,Maybe Text),Maybe FileInfo),Widget)
formProfile user extra = do
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
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    labelPhoto <- newIdent
    figurePhoto <- newIdent
    imgPhoto <- newIdent
    
    return ( (,) <$> ( (,) <$> fnameR <*> emailR ) <*> photoR
           , $(widgetFile "profile/form")
           )


getProfileR :: UserId -> Handler Html
getProfileR uid = do
    user <- maybeAuth
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession ultDestKey

    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffUser ==. just (val uid)
        return x

    jobs <- case empl of
      Just (Entity eid _) -> runDB $ select $ distinct $ do
          x <- from $ table @Role
          where_ $ x ^. RoleStaff ==. val eid
          return $ x ^. RoleName
      Nothing -> return []

    (fw,et) <- generateFormPost formRemove
    dlgProfileRemove <- newIdent
    msgs <- getMessages
    setUltDest HomeR
    defaultLayout $ do
        setTitleI MsgUserProfile
        $(widgetFile "profile/profile")


formRemove :: Html -> MForm Handler (FormResult (), Widget)
formRemove extra = return (FormSuccess (),[whamlet|#{extra}|])


getAccountPhotoR :: UserId -> Handler TypedContent
getAccountPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    return $ case photo of
      Just (Entity _ (UserPhoto _ bs mime)) -> TypedContent (encodeUtf8 mime) $ toContent bs 
      Nothing -> TypedContent typeSvg $ toContent $(embedFile "static/img/person_FILL0_wght400_GRAD0_opsz48.svg") 


postAccountR :: Handler Html
postAccountR = do
    ((fr,widget),enctype) <- runFormPost $ formAccount Nothing
    case fr of
      FormSuccess (user,mfi) -> do
          uid <- setPassword (userPassword user) user >>= \u -> runDB $ insert u
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                runDB $ insert_ UserPhoto
                    { userPhotoUser = uid
                    , userPhotoPhoto = bs 
                    , userPhotoMime = fileContentType fi
                    }
            Nothing -> return ()
          redirect $ AuthR LoginR
      _ -> defaultLayout $ do
          ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession ultDestKey
          setTitleI MsgAccount
          $(widgetFile "account")


getAccountR :: Handler Html
getAccountR = do
    (widget,enctype) <- generateFormPost $ formAccount Nothing
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession ultDestKey
    defaultLayout $ do
        setTitleI MsgAccount
        $(widgetFile "account")


formAccount :: Maybe User -> Html -> MForm Handler (FormResult (User, Maybe FileInfo), Widget)
formAccount user extra = do
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgUsername
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userName <$> user)
    (passR,passV) <- mreq passwordConfirmField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userPassword <$> user)
    (fnameR,fnameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userFullName <$> user)
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userEmail <$> user)
    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r = (,)
            <$> ( User <$> nameR <*> passR
                  <*> FormSuccess False <*> FormSuccess False <*> FormSuccess False <*> FormSuccess False
                  <*> fnameR <*> emailR
                )
            <*> photoR

    let w = do
            toWidget [julius|
document.getElementById(#{fvId photoV}).addEventListener('change',function (e) {
  if (this.files && this.files[0]) {
    let fr = new FileReader();
    fr.onload = function (e) {
      document.getElementById('imgPhoto').setAttribute('src',e.target.result);
    }
    fr.readAsDataURL(this.files[0]);
  }
});
|]
            [whamlet|
#{extra}

<figure>
  <label for=#{fvId photoV}>
    <img src=@{StaticR img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg} #imgPhoto alt=_{MsgPhoto}
      height=64 style="clip-path:circle(50%)">
  <figcaption>_{MsgPhoto}
^{fvInput photoV}

<div.form-field>
  <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
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
  ^{fvInput passV}
  $maybe err <- fvErrors passV
    <label.mdc-text-field--invalid>
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{err}
        
$forall v <- [fnameV,emailV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
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
    return (r, w)


passwordConfirmField :: Field Handler Text
passwordConfirmField = Field
    { fieldParse = \rawVals _ -> return $ case rawVals of
        [a,b] | T.null a || T.null b -> Left (SomeMessage MsgValueRequired)
              | a == b -> Right $ Just a
              | otherwise -> Left (SomeMessage MsgPasswordsDoNotMatch)
        [] -> Right Nothing
        _ -> Left (SomeMessage MsgYouMustEnterTwoValues)
    , fieldView = \theId name attrs _ isReq -> [whamlet|
<label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField>
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label>_{MsgPassword}
  <input ##{theId} name=#{name} *{attrs} type=password :isReq:required>
  <span.mdc-line-ripple>
  
<label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField style="margin-top:1rem">
  <span.mdc-text-field__ripple>
  <span.mdc-floating-label>_{MsgConfirmPassword}
  <input ##{theId}Confirm name=#{name} *{attrs} type=password :isReq:required>
  <span.mdc-line-ripple>
|]
    , fieldEnctype = UrlEncoded
    }


info :: Text
info = "info"

warn :: Text
warn = "warn"
