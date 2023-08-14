{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.AboutUs (getAboutUsR) where

import Data.Text (Text)
import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setTitleI, setUltDestCurrent, preEscapedToMarkup)
import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Yesod.Form.Fields (unTextarea)

import Settings (widgetFile)
import Foundation
    ( Handler
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage (MsgAboutUs, MsgPhoto, MsgLogout)
    )

import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Persist (Entity(Entity))
import Database.Esqueleto.Experimental
    (selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Model (Contents(Contents), EntityField (ContentsSection))


getAboutUsR :: Handler Html
getAboutUsR = do
    content <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    muid <- maybeAuth
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgAboutUs
        $(widgetFile "about/about")


section :: Text
section = "ABOUT_US"
