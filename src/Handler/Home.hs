{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Text.Hamlet (Html)
import Settings (widgetFile)
import Yesod.Core (Yesod(defaultLayout), setTitleI, setUltDestCurrent)
import Yesod.Auth ( Route(LoginR, LogoutR), maybeAuth )

import Settings.StaticFiles (img_salon_svg)

import Database.Persist (Entity (Entity))
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler
    , Route (StaticR, ServicesR, AboutR, AuthR)
    , AppMessage
      ( MsgSalon, MsgWelcome, MsgServices, MsgAboutUs
      , MsgBook, MsgLogout
      )
    )

import Model (User (User))

getHomeR :: Handler Html
getHomeR = do
    muid <- maybeAuth
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgWelcome
        $(widgetFile "homepage")

