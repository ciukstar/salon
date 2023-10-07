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
  , postAdmEmplUnregR
  , getAdmStaffSearchR
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor(first))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T (toLower, words, concat)
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
    , YesodRequest (reqGetParams), newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth (maybeAuth, Route (LoginR))

import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput, fvLabel, fvId, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsName, fsAttrs, fsId)
    , Field
    )
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields
    ( textField, emailField, passwordField, intField, hiddenField
    , fileField, checkBoxField
    )
import Yesod.Form.Functions
    ( mreq, mopt, generateFormPost, runFormPost, checkM, checkBool )
import Settings (widgetFile)

import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (replace, insert, insert_, delete)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Yesod.Persist (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, asc, val
    , (^.), (==.), (:&)((:&)), (?.), (=.), (%), (++.), (||.)
    , where_, not_, selectQuery, subSelectList, in_
    , just, notIn, isNothing_, innerJoin, on, desc
    , leftJoin, update, set, like, upper_, distinct, valList
    , exists, Value (unValue), justList
    )

import Foundation
    ( Handler, Widget
    , AdminR
      ( AdmStaffDeleteR, AdmStaffEditR, AdmEmplR, AdmStaffR, AdmRoleR
      , AdmStaffCreateR, AdmStaffPhotoR, AdmRolesR, AdmRoleCreateR
      , AdmRoleEditR, AdmRoleDeleteR, AdmEmplUserR, AdmEmplUnregR
      , AdmStaffSearchR
      )
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR, StaticR, ProfileR)
    , AppMessage
      ( MsgStaff, MsgPhoto, MsgCancel, MsgSave
      , MsgNoStaffYet, MsgEmployee, MsgRecordEdited, MsgName
      , MsgRole, MsgPhone, MsgMobile, MsgEmail, MsgRecordAdded
      , MsgEmployeeAlreadyInTheList, MsgDeleteAreYouSure, MsgYesDelete
      , MsgPleaseConfirm, MsgRecordDeleted, MsgRoles, MsgNoRolesYes
      , MsgAddRole, MsgService, MsgTheName, MsgRating, MsgRoleAlreadyInTheList
      , MsgRegisterAsUser, MsgUser, MsgRegistration, MsgUsername, MsgPassword
      , MsgFullName, MsgAlreadyExists, MsgUnregisterAreYouSure, MsgSearch
      , MsgNoStaffMembersFound, MsgStatus, MsgSelect, MsgRatings
      , MsgDismissed, MsgEmployed, MsgAccountStatus, MsgRegistered
      , MsgUnregistered, MsgValueNotInRange, MsgAdministrator, MsgUnregister
      )
    )

import Model
    ( StaffId, Staff(Staff, staffName, staffPhone, staffMobile, staffEmail, staffStatus)
    , EntityField
      ( StaffId, StaffPhotoStaff, StaffPhotoMime, StaffPhotoPhoto
      , StaffName, RoleStaff, RoleId, ServiceId, ServiceGroup
      , RoleService, RoleName, RoleRating, StaffUser, UserId, UserName
      , StaffPhone, StaffMobile, StaffEmail, StaffStatus
      )
    , StaffPhoto (StaffPhoto, staffPhotoPhoto, staffPhotoMime, staffPhotoStaff)
    , Role (Role, roleService, roleName, roleRating), RoleId
    , ServiceId, Service (Service)
    , UserId,  User (User), UserPhoto (UserPhoto)
    , EmplStatus (EmplStatusDismissed, EmplStatusEmployed)
    )

import Settings.StaticFiles (img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg)

import Menu (menu)


getAdmStaffSearchR :: Handler Html
getAdmStaffSearchR = do
    mq <- runInputGet $ iopt textField "q"
    rnames <- (snd <$>) . filter ((== "role") . fst) . reqGetParams <$> getRequest
    ratings <- (read . unpack . snd <$>) . filter ((== "rating") . fst) . reqGetParams <$> getRequest
    stati <- (read . unpack . snd <$>) . filter ((== "status") . fst) . reqGetParams <$> getRequest
    accstati <- (snd <$>) . filter ((== "accstatus") . fst) . reqGetParams <$> getRequest
    staff <- runDB $ select $ do
          x <- from $ table @Staff
          case mq of
            Just q -> where_ $ ( upper_ (x ^. StaffName) `like` (%) ++. upper_ (val q) ++. (%) )
              ||. ( upper_ (x ^. StaffPhone) `like` (%) ++. upper_ (just (val q)) ++. (%) )
              ||. ( upper_ (x ^. StaffMobile) `like` (%) ++. upper_ (just (val q)) ++. (%) )
              ||. ( upper_ (x ^. StaffEmail) `like` (%) ++. upper_ (just (val q)) ++. (%) )
            Nothing -> return ()
          case stati of
            [] -> return ()
            ys -> where_ $ x ^. StaffStatus `in_` valList ys
          case accstati of
            ["registered"] -> where_ $ not_ $ isNothing_ $ x ^. StaffUser
            ["unregistered"] -> where_ $ isNothing_ $ x ^. StaffUser
            _ -> return ()
          case rnames of
            [] -> return ()
            ys -> where_ $ exists $ do
                y <- from $ table @Role
                where_ $ y ^. RoleStaff ==. x ^. StaffId
                where_ $ y ^. RoleName `in_` valList ys
          case ratings of
            [] -> return ()
            ys -> where_ $ exists $ do
                y <- from $ table @Role
                where_ $ y ^. RoleStaff ==. x ^. StaffId
                where_ $ y ^. RoleRating `in_` justList (valList ys)
          orderBy [asc (x ^. StaffId)]
          return x
    roles <- forM staff ( \e@(Entity eid _) -> (e,) <$> runDB ( selectOne $ do
          x <- from $ table @Role
          where_ $ x ^. RoleStaff ==. val eid
          orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
          return x ) )
    roleList <- (unValue <$>) <$> runDB ( select $ distinct $ do
        x <- from $ table @Role
        orderBy [asc (x ^. RoleName)]
        return $ x ^. RoleName )
    ratingList <- (unValue <$>) <$> runDB ( select $ distinct $ do
        x <- from $ table @Role
        orderBy [asc (x ^. RoleRating)]
        return $ x ^. RoleRating )
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "admin/staff/search")


postAdmEmplUnregR :: StaffId -> UserId -> Handler ()
postAdmEmplUnregR eid uid = do
    runDB $ delete uid
    addMessageI "info" MsgRecordEdited
    redirect $ AdminR $ AdmEmplR eid


postAdmEmplUserR :: StaffId -> Handler Html
postAdmEmplUserR eid = do
    empl <- runDB $ selectOne $ do
        x <- from $ table @Staff
        where_ $ x ^. StaffId ==. val eid
        return x
    ((fr,fw),et) <- runFormPost $ formUser empl
    case fr of
      FormSuccess r -> do
          uid <- runDB $ insert r
          runDB $ update $ \x -> do
              set x [StaffUser =. just (val uid)]
              where_ $ x ^. StaffId ==. val eid
          photo <- runDB $ selectOne $ do
              y <- from $ table @StaffPhoto
              where_ $ y ^. StaffPhotoStaff ==. val eid
              return y
          case photo of
            Just (Entity _ (StaffPhoto _ bs mime)) -> runDB $ insert_ (UserPhoto uid bs mime)
            Nothing -> return ()
          addMessageI "info" MsgRecordAdded
          redirect $ AdminR $ AdmEmplR eid
      _ -> defaultLayout $ do
        setTitleI MsgRegistration
        $(widgetFile "admin/staff/user/user")


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
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgUsername
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (T.concat . T.words . T.toLower . staffName . entityVal <$> empl)
    (passR,passV) <- mreq passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
    (adminR,adminV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAdministrator
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (pure False)
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
    let r = User <$> nameR <*> passR <*> adminR <*> fnameR <*> emailR
    let w = [whamlet|
#{extra}
$forall v <- [nameV,passV]
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

<div.mdc-form-field.form-field data-mdc-auto-init=MDCFormField style="display:flex;flex-direction:row">
  ^{fvInput adminV}
  $with selected <- resolveSelected adminR
    <button.mdc-switch type=button role=switch #switchAdmin data-mdc-auto-init=MDCSwitch
      :selected:.mdc-switch--selected :selected:aria-checked=true
      :not selected:.mdc-switch--unselected :not selected:aria-checked=false
      onclick="document.getElementById('#{fvId adminV}').checked = !this.MDCSwitch.selected">
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
    <label for=switchAdmin>_{MsgAdministrator}

$forall v <- [fnameV,emailV]
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
  where
      resolveSelected adminR = case adminR of FormSuccess x -> x ; _ -> False

      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mu <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserName ==. val name
              return x
          return $ case mu of
            Nothing -> Right name
            Just _ -> Left MsgAlreadyExists


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
    (ratingR,ratingV) <- mopt ratingField FieldSettings
        { fsLabel = SomeMessage MsgRating
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("min","0"),("max","5")]
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
        $forall Entity sid (Service name _ _ _ _ _) <- services
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

      ratingField = checkBool (\x -> x >= 1 && x <= 5) (MsgValueNotInRange 1 5) intField

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
                     [StaffPhotoPhoto P.=. bs, StaffPhotoMime P.=. fileContentType fi]
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
    state <- reqGetParams <$> getRequest
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
    (statusR,statusV) <- first (read . unpack <$>) <$> mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgStatus
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (pack . show <$> ((staffStatus . entityVal <$> staff) <|> pure EmplStatusEmployed))
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

    let r = (,)
            <$> ( Staff
                  <$> nameR
                  <*> statusR
                  <*> phoneR
                  <*> mobileR
                  <*> emailR
                  <*> FormSuccess Nothing
                )
            <*> photoR
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
    user <- maybeAuth
    msid <- (toSqlKey <$>) <$> runInputGet (iopt intField "sid")
    scrollY <- runInputGet (iopt textField "scrollY")
    staff <- runDB $ select $ do
        x <- from $ table @Staff
        orderBy [asc (x ^. StaffId)]
        return x
    roles <- forM staff ( \e@(Entity eid _) -> (e,) <$> runDB ( selectOne $ do
          x <- from $ table @Role
          where_ $ x ^. RoleStaff ==. val eid
          orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
          return x ) )
    msgs <- getMessages
    setUltDestCurrent
    fabAddStaff <- newIdent
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


range :: Enum a => a -> a -> [a]
range a b = [a..b]
