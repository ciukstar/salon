{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.AboutUs (getAboutUsR) where

import Text.Hamlet (Html)
import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages
    )

import Settings (widgetFile)
import Foundation
    ( Handler
    , Route (ProfileR, AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage
      ( MsgAboutUs, MsgPhoto, MsgUserProfile, MsgLogin, MsgNavigationMenu
      , MsgNoContentYet
      )
    )

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity(Entity))
import Database.Esqueleto.Experimental (selectOne, from, table)
import Model (AboutUs(AboutUs))

import Menu (menu)

getAboutUsR :: Handler Html
getAboutUsR = do
    user <- maybeAuth
    info <- runDB $ selectOne $ from $ table @AboutUs
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "about/about")
