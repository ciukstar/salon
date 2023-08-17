{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Admin.Staff
  ( getAdmStaffR
  , getAdmStaffCreateR
  , getAdmStaffPhotoR
  , getAdmEmplR
  , postAdmEmplR
  , getAdmStaffEditR
  , postAdmStaffR
  , postAdmStaffDeleteR
  , postAdmRolesR
  , getAdmRoleR
  , postAdmRoleR
  , getAdmRoleCreateR
  , getAdmRoleEditR
  , postAdmRoleDeleteR
  , getAdmEmplUserR
  , postAdmEmplUserR
  ) where

import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Text.Hamlet (Html)
import Data.FileEmbed (embedFile)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad (forM)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages
    , TypedContent (TypedContent), ToContent (toContent)
    , typeSvg, addMessageI, redirect, FileInfo (fileContentType)
    , SomeMessage (SomeMessage), fileSourceByteString
    , MonadTrans (lift), whamlet, getRequest
    , YesodRequest (reqGetParams)
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))

import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput, fvLabel, fvId, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsName, fsAttrs, fsId)
    , Field
    )
import Yesod.Form (mreq, mopt, runFormPost, checkM, passwordField)
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (textField, emailField, doubleField, hiddenField, fileField)
import Yesod.Form.Functions (generateFormPost)
import Settings (widgetFile)

import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (replace, insert, insert_, delete)
    , PersistUniqueWrite (upsert)
    , (=.)
    )
import Database.Persist.Sql (fromSqlKey)
import Yesod.Persist (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, asc, val
    , (^.), (==.), (:&)((:&)), (?.)
    , where_, not_, selectQuery, subSelectList
    , just, notIn, isNothing_, innerJoin, on, desc, limit
    , leftJoin
    )

import Foundation
    ( Handler, Widget
    , AdminR
      ( AdmStaffDeleteR, AdmStaffEditR, AdmEmplR, AdmStaffR, AdmRoleR
      , AdmStaffCreateR, AdmStaffPhotoR, AdmRolesR, AdmRoleCreateR
      , AdmRoleEditR, AdmRoleDeleteR, AdmEmplUserR
      )
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR, StaticR)
    , AppMessage
      ( MsgStaff, MsgLogout, MsgPhoto, MsgCancel, MsgSave
      , MsgNoStaffYet, MsgEmployee, MsgRecordEdited, MsgName
      , MsgRole, MsgPhone, MsgMobile, MsgEmail, MsgRecordAdded
      , MsgEmployeeAlreadyInTheList, MsgDeleteAreYouSure, MsgYesDelete
      , MsgPleaseConfirm, MsgRecordDeleted, MsgRoles, MsgNoRolesYes
      , MsgAddRole, MsgService, MsgTheName, MsgRating, MsgRoleAlreadyInTheList
      , MsgRegisterAsUser, MsgUser, MsgRegistration, MsgUsername, MsgPassword, MsgFullName
      )
    )

import Model
    ( StaffId, Staff(Staff, staffName, staffPhone, staffMobile, staffEmail)
    , EntityField
      ( StaffId, StaffPhotoStaff, StaffPhotoMime, StaffPhotoPhoto
      , StaffName, RoleStaff, RoleId, ServiceId, ServiceGroup
      , RoleService, RoleName, RoleRating, StaffUser, UserId
      )
    , StaffPhoto (StaffPhoto, staffPhotoPhoto, staffPhotoMime, staffPhotoStaff)
    , Role (Role, roleService, roleName, roleRating), RoleId
    , ServiceId, Service (Service), User (User)
    )

import Settings.StaticFiles (img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg)


postAdmEmplUserR :: StaffId -> Handler Html
postAdmEmplUserR eid = undefined


getAdmEmplUserR :: StaffId -> Handler Html
getAdmEmplUserR eid = do
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x
    (fw,et) <- generateFormPost $ formUser empl 
    defaultLayout $ do
        setTitleI MsgRegistration
        $(widgetFile "admin/staff/user/user")


formUser :: Maybe (Entity Staff) -> Html -> MForm Handler (FormResult User, Widget)
formUser empl extra = do
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgUsername
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffName . entityVal <$> empl)
    (passR,passV) <- mreq passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    (fnameR,fnameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgFullName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (pure (staffName . entityVal <$> empl))
    (emailR,emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffEmail . entityVal <$> empl)
    let r = User <$> nameR <*> passR <*> fnameR <*> emailR
    let w = [whamlet|
#{extra}
$forall v <- [nameV,passV,fnameV,emailV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      <div.mdc-line-ripple>
|]
    return (r,w)


postAdmRoleDeleteR :: StaffId -> RoleId -> Handler ()
postAdmRoleDeleteR eid rid = do
    runDB $ delete rid
    addMessageI "info" MsgRecordDeleted
    state <- reqGetParams <$> getRequest
    redirect (AdminR $ AdmEmplR eid,state)


postAdmRoleR :: StaffId -> RoleId -> Handler Html
postAdmRoleR eid rid = do
    role <- runDB $ selectOne $ do
        x <- from $ table @Role
        where_ $ x ^. RoleId ==. val rid
        return x
    ((fr,fw),et) <- runFormPost $ formRole eid role
    state <- reqGetParams <$> getRequest
    case fr of
      FormSuccess r -> do
          runDB $ replace rid r
          addMessageI "info" MsgRecordEdited
          redirect (AdminR $ AdmEmplR eid,state)
      _ -> defaultLayout $ do
          setTitleI MsgRole
          $(widgetFile "admin/staff/role/edit")


getAdmRoleEditR :: StaffId -> RoleId -> Handler Html
getAdmRoleEditR eid rid = do
    state <- reqGetParams <$> getRequest
    role <- runDB $ selectOne $ do
        x <- from $ table @Role
        where_ $ x ^. RoleId ==. val rid
        return x
    (fw,et) <- generateFormPost $ formRole eid role
    defaultLayout $ do
        setTitleI MsgRole
        $(widgetFile "admin/staff/role/edit")



getAdmRoleR :: StaffId -> RoleId -> Handler Html
getAdmRoleR sid rid = do
    state <- reqGetParams <$> getRequest
    role <- runDB $ selectOne $ do
        x :& e :& s <- from $ table @Role
            `innerJoin` table @Staff `on` (\(x :& e) -> x ^. RoleStaff ==. e ^. StaffId)
            `innerJoin` table @Service `on` (\(x :& _ :& s) -> x ^. RoleService ==. s ^. ServiceId)
        where_ $ x ^. RoleId ==. val rid
        return (x,e,s)
    defaultLayout $ do
        setTitleI MsgRole
        $(widgetFile "admin/staff/role/role")


postAdmRolesR :: StaffId -> Handler Html
postAdmRolesR sid = do
    state <- reqGetParams <$> getRequest
    ((fr,fw),et) <- runFormPost $ formRole sid Nothing
    case fr of
      FormSuccess r -> do
          rid <- runDB $ insert r
          addMessageI "info" MsgRecordAdded
          redirect (AdminR $ AdmEmplR sid,state ++ [("rid",pack $ show $ fromSqlKey rid)])
      _ -> defaultLayout $ do
          setTitleI MsgRole
          $(widgetFile "admin/staff/role/create")


getAdmRoleCreateR :: StaffId -> Handler Html
getAdmRoleCreateR sid = do
    state <- reqGetParams <$> getRequest
    (fw,et) <- generateFormPost $ formRole sid Nothing
    defaultLayout $ do
        setTitleI MsgRole
        $(widgetFile "admin/staff/role/create")


formRole :: StaffId -> Maybe (Entity Role) -> Html -> MForm Handler (FormResult Role,Widget)
formRole eid role extra = do
    services <- lift $ runDB queryServices
    (servR,servV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgService
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (roleService . entityVal <$> role)
    (nameR,nameV) <- mreq (uniqueNameField servR) FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (roleName . entityVal <$> role)
    (ratingR,ratingV) <- mopt doubleField FieldSettings
        { fsLabel = SomeMessage MsgRating
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (roleRating . entityVal <$> role)
    let r = Role eid <$> servR <*> nameR <*> ratingR
    let w = [whamlet|
#{extra}
<div.form-field>
  <div.mdc-select.mdc-select--filled.mdc-select--required data-mdc-auto-init=MDCSelect
    :isJust (fvErrors servV):.mdc-select--invalid>
    ^{fvInput servV}
    <div.mdc-select__anchor role=button aria-aspopup=listbox aria-expanded=false aria-required=true>
      <span.mdc-select__ripple>
      <span.mdc-floating-label>#{fvLabel servV}
      <span.mdc-select__selected-text-container>
        <span.mdc-select__selected-text>
      <span.mdc-select__dropdown-icon>
        <svg.mdc-select__dropdown-icon-graphic viewBox="7 10 10 5" focusable=false>
          <polygon.mdc-select__dropdown-icon-inactive stroke=none fill-rule=evenodd points="7 10 12 15 17 10">
          <polygon.mdc-select__dropdown-icon-active stroke=none fill-rule=evenodd points="7 15 12 10 17 15">
      <span.mdc-line-ripple>

    <div.mdc-select__menu.mdc-menu.mdc-menu-surface.mdc-menu-surface--fullwidth>
      <ul.mdc-deprecated-list role=listbox>
        $forall Entity sid (Service name _ _ _ _) <- services
          <li.mdc-deprecated-list-item role=option data-value=#{fromSqlKey sid} aria-selected=false>
            <span.mdc-deprecated-list-item__ripple>
            <span.mdc-deprecated-list-item__text>
              #{name}

  $maybe errs <- fvErrors servV
    <div.mdc-select-helper-text.mdc-select-helper-text--validation-msg>
      #{errs}

$forall v <- [nameV,ratingV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid
      :isJust (fvErrors v):.mdc-text-field--with-trailing-icon>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      $maybe _ <- fvErrors v
        <i.mdc-text-field__icon.mdc-text-field__icon--trailing.material-symbols-outlined>error
      <span.mdc-line-ripple>
    $maybe errs <- fvErrors v
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}
|]
    return (r,w)
  where
      uniqueNameField :: FormResult ServiceId -> Field Handler Text
      uniqueNameField servR = checkM (uniqueName servR) textField

      uniqueName :: FormResult ServiceId -> Text -> Handler (Either AppMessage Text)
      uniqueName servR name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Role
              where_ $ x ^. RoleStaff ==. val eid
              case servR of
                FormSuccess sid -> where_ $ x ^. RoleService ==. val sid
                _ -> return ()
              where_ $ x ^. RoleName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity rid _) -> case role of
              Nothing -> Left MsgRoleAlreadyInTheList
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgRoleAlreadyInTheList

      queryServices = select $ do
          x <- from $ table @Service
          where_ $ just (x ^. ServiceId) `notIn` subSelectList
              ( from $ selectQuery $ do
                y <- from $ table @Service
                where_ $ not_ $ isNothing_ (y ^. ServiceGroup)
                return $ y ^. ServiceGroup
              )
          return x


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
    mrid <- runInputGet $ iopt textField "rid"
    open <- runInputGet $ iopt textField "open"
    scrollY <- runInputGet $ iopt textField "scrollY"
    empl <- runDB $ selectOne $ do
        x :& u <- from $ table @Staff
            `leftJoin` table @User `on` (\(x :& u) -> x ^. StaffUser ==. u ?. UserId)
        where_ $ x ^. StaffId ==. val sid
        return (x,u)
    roles <- case empl of
      Just (Entity eid _, _) -> runDB $ select $ do
          x :& s <- from $ table @Role
             `innerJoin` table @Service `on` (\(x :& s) -> x ^. RoleService ==. s ^. ServiceId)
          where_ $ x ^. RoleStaff ==. val eid
          orderBy [asc (x ^. RoleId)]
          return (x,s)
      _ -> return []
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

    let r = (,) <$> (Staff <$> nameR <*> phoneR <*> mobileR <*> emailR <*> FormSuccess Nothing) <*> photoR
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
    roles <- forM staff ( \e@(Entity eid _) -> (e,) <$> runDB ( select $ do
          x <- from $ table @Role
          where_ $ x ^. RoleStaff ==. val eid
          orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
          limit 2
          return x ) )
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
