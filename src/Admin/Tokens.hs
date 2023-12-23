{-# LANGUAGE TemplateHaskell #-}

module Admin.Tokens (getTokensR) where

import Foundation
    ( Handler
    , AppMessage
      ( MsgTokens, MsgNavigationMenu, MsgUserProfile, MsgPhoto, MsgLogin
      , MsgInitialize, MsgClearSettings, MsgStoreInDatabase
      , MsgStoreWithSecretManager
      )
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR
      )
    )
import Text.Hamlet (Html)
import Database.Persist (Entity(Entity))

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core (Yesod(defaultLayout))
import Yesod.Core.Widget (setTitleI)

import Settings (widgetFile)
import Menu (menu)


getTokensR :: Handler Html
getTokensR = do
    user <- maybeAuth
    defaultLayout $ do
        setTitleI MsgTokens
        $(widgetFile "admin/tokens/tokens")
