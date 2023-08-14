{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Text.Hamlet (Html)
import Settings (widgetFile)
import Yesod.Core (Yesod(defaultLayout), setTitleI, setUltDestCurrent, getMessages)
import Yesod.Auth ( Route(LoginR, LogoutR), maybeAuth )
import Settings.StaticFiles (img_salon_svg)

import Database.Persist (Entity (Entity))

import Foundation
    ( Handler
    , Route
      ( StaticR, ServicesR, AboutUsR, AuthR, AccountPhotoR
      , PhotoPlaceholderR, BookR
      )
    , AppMessage
      ( MsgSalon, MsgWelcome, MsgServices, MsgAboutUs
      , MsgBook, MsgLogout, MsgPhoto
      )
    )

getHomeR :: Handler Html
getHomeR = do
    muid <- maybeAuth
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgWelcome
        $(widgetFile "homepage")

