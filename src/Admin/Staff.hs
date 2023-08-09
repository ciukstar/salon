{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Admin.Staff
  ( getAdmStaffR
  , getAdmStaffCreateR
  ) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setUltDestCurrent, getMessages)
import Yesod.Core.Widget (setTitleI, whamlet)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
    
import Yesod.Form.Types (MForm, FormResult, FieldView (fvInput))
import Yesod.Form (mreq, mopt)
import Yesod.Form.Fields (textField, emailField)
import Yesod.Form.Functions (generateFormPost)
import Settings (widgetFile)

import Foundation
    ( Handler, Widget
    , AppMessage (MsgStaff, MsgLogout, MsgPhoto, MsgCancel, MsgSave)
    , AdminR (AdmStaffR, AdmStaffCreateR)
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR)
    )
import Database.Persist (Entity (Entity, entityVal))
import Yesod.Persist (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    (select, from, table, orderBy, asc
    , (^.)
    )

import Model
    ( Staff(Staff, staffName, staffRole, staffPhone, staffMobile, staffEmail)
    , EntityField (StaffId)
    )


getAdmStaffCreateR :: Handler Html
getAdmStaffCreateR = do
    (fw,et) <- generateFormPost $ formStaff Nothing
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "admin/staff/create")


formStaff :: Maybe (Entity Staff) -> Html -> MForm Handler (FormResult Staff, Widget)
formStaff staff extra = do
    (nameR,nameV) <- mreq textField "" (staffName . entityVal <$> staff)
    (roleR,roleV) <- mreq textField "" (staffRole . entityVal <$> staff)
    (phoneR,phoneV) <- mopt textField "" (staffPhone . entityVal <$> staff)
    (mobileR,mobileV) <- mopt textField "" (staffMobile . entityVal <$> staff)
    (emailR,emailV) <- mopt emailField "" (staffEmail . entityVal <$> staff)
    
    let r = Staff <$> nameR <*> roleR <*> phoneR <*> mobileR <*> emailR
    let w = [whamlet|
#{extra}
$forall v <- [nameV,roleV,phoneV,mobileV,emailV]
  <div.form-field>
    ^{fvInput v}
|]
    return (r,w)


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
