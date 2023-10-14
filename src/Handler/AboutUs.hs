{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.AboutUs (getAboutUsR) where

import Data.Text (Text)
import Text.Hamlet (Html)
import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , preEscapedToMarkup, getMessages
    )
import Yesod.Form.Fields (unTextarea)

import Settings (widgetFile)
import Foundation
    ( Handler
    , Route (ProfileR, AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage (MsgAboutUs, MsgPhoto, MsgUserProfile, MsgLogin, MsgNavigationMenu)
    )

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity(Entity))
import Database.Esqueleto.Experimental
    (selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Model (Contents(Contents), EntityField (ContentsSection))

import Menu (menu)

getAboutUsR :: Handler Html
getAboutUsR = do
    user <- maybeAuth
    content <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "about/about")


section :: Text
section = "ABOUT_US"
