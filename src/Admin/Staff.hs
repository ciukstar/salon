{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Admin.Staff
  ( getAdmStaffR
  , getAdmStaffCreateR
  , getAdmStaffPhotoR
  , getAdmEmplR
  , postAdmEmplR
  , getAdmStaffEditR
  , postAdmStaffR
  , postAdmStaffDeleteR
  ) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages
    , TypedContent (TypedContent), ToContent (toContent)
    , typeSvg, addMessageI, redirect, FileInfo (fileContentType)
    , SomeMessage (SomeMessage), fileSourceByteString
    )
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))

import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput, fvLabel, fvId, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsName, fsAttrs, fsId)
    , Field
    )
import Yesod.Form (mreq, mopt, runFormPost, fileField, checkM)
import Yesod.Form.Fields (textField, emailField)
import Yesod.Form.Functions (generateFormPost)
import Settings (widgetFile)

import Foundation
    ( Handler, Widget
    , AdminR (AdmStaffDeleteR, AdmStaffEditR, AdmEmplR, AdmStaffR, AdmStaffCreateR, AdmStaffPhotoR)
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR, StaticR)
    , AppMessage
      ( MsgStaff, MsgLogout, MsgPhoto, MsgCancel, MsgSave
      , MsgNoStaffYet, MsgEmployee, MsgRecordEdited, MsgName
      , MsgRole, MsgPhone, MsgMobile, MsgEmail, MsgRecordAdded
      , MsgEmployeeAlreadyInTheList, MsgDeleteAreYouSure, MsgYesDelete
      , MsgPleaseConfirm, MsgRecordDeleted
      )
    )
import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (replace, insert, insert_, delete)
    , PersistUniqueWrite (upsert)
    , (=.)
    )
import Yesod.Persist (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, asc, val
    , (^.), (==.)
    , where_
    )

import Model
    ( StaffId, Staff(Staff, staffName, staffRole, staffPhone, staffMobile, staffEmail)
    , EntityField (StaffId, StaffPhotoStaff, StaffPhotoMime, StaffPhotoPhoto, StaffName)
    , StaffPhoto (StaffPhoto, staffPhotoPhoto, staffPhotoMime, staffPhotoStaff)
    )
import Data.FileEmbed (embedFile)
import Settings.StaticFiles (img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg)
import Data.Maybe (isJust)


postAdmStaffDeleteR :: StaffId -> Handler ()
postAdmStaffDeleteR sid = do
    runDB $ delete sid
    addMessageI "info" MsgRecordDeleted
    redirect $ AdminR AdmStaffR


postAdmEmplR :: StaffId -> Handler Html
postAdmEmplR sid = do
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val sid
        return x
    ((fr,fw),et) <- runFormPost $ formEmpl empl
    case fr of
      FormSuccess (r,mfi) -> do
          runDB $ replace sid r
          addMessageI "info" MsgRecordEdited
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                _ <- runDB $ upsert
                     (StaffPhoto sid bs (fileContentType fi))
                     [StaffPhotoPhoto =. bs, StaffPhotoMime =. fileContentType fi]
                return ()
            Nothing -> return ()
          redirect $ AdminR $ AdmEmplR sid
      _ -> defaultLayout $ do
          setTitleI MsgEmployee
          $(widgetFile "admin/staff/edit")


getAdmEmplR :: StaffId -> Handler Html
getAdmEmplR sid = do
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val sid
        return x
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEmployee
        $(widgetFile "admin/staff/employee")


getAdmStaffEditR :: StaffId -> Handler Html
getAdmStaffEditR sid = do
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val sid
        return x
    (fw,et) <- generateFormPost $ formEmpl empl
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "admin/staff/edit")


getAdmStaffCreateR :: Handler Html
getAdmStaffCreateR = do
    (fw,et) <- generateFormPost $ formEmpl Nothing
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "admin/staff/create")


formEmpl :: Maybe (Entity Staff) -> Html -> MForm Handler (FormResult (Staff,Maybe FileInfo), Widget)
formEmpl staff extra = do
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffName . entityVal <$> staff)
    (roleR,roleV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgRole
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffRole . entityVal <$> staff)
    (phoneR,phoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffPhone . entityVal <$> staff)
    (mobileR,mobileV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgMobile
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffMobile . entityVal <$> staff)
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffEmail . entityVal <$> staff)
    (photoR,photoV) <- mopt fileField  FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r = (,) <$> (Staff <$> nameR <*> roleR <*> phoneR <*> mobileR <*> emailR) <*> photoR
    let w = $(widgetFile "admin/staff/form")
    return (r,w)

  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Staff
              where_ $ x ^. StaffName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity eid _) -> case staff of
              Nothing -> Left MsgEmployeeAlreadyInTheList
              Just (Entity eid' _) | eid == eid' -> Right name
                                   | otherwise -> Left MsgEmployeeAlreadyInTheList
                                   


postAdmStaffR :: Handler Html
postAdmStaffR = do
    ((fr,fw),et) <- runFormPost $ formEmpl Nothing
    case fr of
      FormSuccess (r,mfi) -> do
          eid <- runDB $ insert r
          addMessageI "info" MsgRecordAdded
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                runDB $ insert_ StaffPhoto { staffPhotoStaff = eid
                                           , staffPhotoPhoto = bs
                                           , staffPhotoMime = fileContentType fi
                                           }
            Nothing -> return ()
          redirect $ AdminR AdmStaffR
      _ -> defaultLayout $ do
          setTitleI MsgEmployee
          $(widgetFile "admin/staff/create")


getAdmStaffR :: Handler Html
getAdmStaffR = do
    muid <- maybeAuth
    staff <- runDB $ select $ do
        x <- from $ table @Staff
        orderBy [asc (x ^. StaffId)]
        return x
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "admin/staff/staff")


getAdmStaffPhotoR :: StaffId -> Handler TypedContent
getAdmStaffPhotoR sid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @StaffPhoto
        where_ $ x ^. StaffPhotoStaff ==. val sid
        return x
    return $ case photo of
      Just (Entity _ (StaffPhoto _ bs mime)) -> TypedContent (encodeUtf8 mime) (toContent bs)
      Nothing -> TypedContent typeSvg $ toContent $(embedFile "static/img/person_FILL0_wght400_GRAD0_opsz48.svg")
