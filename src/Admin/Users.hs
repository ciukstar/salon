{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Admin.Users
  ( getUsersR
  , getUserCreateFormR
  , postUsersR
  ) where

import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages, whamlet
    )
import Settings (widgetFile)

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity (Entity, entityVal))
import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, desc
    , (^.)
    )

import Foundation
    ( Handler, Widget
    , Route (AuthR, AdminR, PhotoPlaceholderR, AccountPhotoR, AdminR)
    , AdminR (UserCreateFormR, UsersR)
    , AppMessage
      ( MsgUsers, MsgNoUsersYet, MsgLogout, MsgPhoto, MsgSave
      , MsgUser, MsgCancel
      )
    )
import Model (User(User, userName, userPassword, userFullName, userEmail), EntityField (UserId))
import Yesod.Form.Types (FormResult, MForm, FieldView (fvInput))
import Yesod.Form.Functions (mreq, mopt, generateFormPost)
import Yesod.Form.Fields (textField, passwordField, emailField)


postUsersR :: Handler Html
postUsersR = undefined


getUserCreateFormR :: Handler Html
getUserCreateFormR = do
    (fw,et) <- generateFormPost $ formUser Nothing
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


formUser :: Maybe (Entity User) -> Html -> MForm Handler (FormResult User, Widget)
formUser user extra = do
    (nameR,nameV) <- mreq textField "" (userName . entityVal <$> user)
    (passR,passV) <- mreq passwordField "" (userPassword . entityVal <$> user)
    (fnameR,fnameV) <- mopt textField "" (userFullName . entityVal <$> user)
    (emailR,emailV) <- mopt emailField "" (userEmail . entityVal <$> user)
    let r = User <$> nameR <*> passR <*> fnameR <*> emailR
    let w = [whamlet|
#{extra}
$forall v <- [nameV,passV,fnameV,emailV]
  <div.form-field>
    ^{fvInput v}
|]
    return (r,w)
