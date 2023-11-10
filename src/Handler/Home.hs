{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Home (getHomeR) where

import Text.Hamlet (Html)
import Settings (widgetFile)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages
    )
import Yesod.Auth (Route(LoginR), maybeAuth)

import Settings.StaticFiles (img_salon_svg)

import Database.Persist (Entity (Entity))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental (selectOne, from, table)

import Foundation
    ( Handler
    , Route
      ( StaticR, ServicesR, AboutUsR, AuthR, AccountPhotoR
      , PhotoPlaceholderR, BookOffersR, AdminR, ProfileR
      )
    , AdminR (BrandMarkR)
    , AppMessage
      ( MsgSalon, MsgWelcome, MsgServices, MsgAboutUs, MsgNavigationMenu
      , MsgBook, MsgPhoto, MsgBrandMark, MsgLogin, MsgUserProfile
      )
    )
import Model (Brand(Brand))

import Menu (menu)

getHomeR :: Handler Html
getHomeR = do
    user <- maybeAuth
    brand <- runDB $ selectOne $ from $ table @Brand
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgWelcome
        $(widgetFile "homepage")

