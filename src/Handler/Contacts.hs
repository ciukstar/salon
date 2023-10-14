{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Contacts (getContactR, section) where

import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core (Yesod(defaultLayout), preEscapedToMarkup, setUltDestCurrent, getMessages)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Fields (unTextarea)
import Settings (widgetFile)

import Database.Persist (Entity (Entity))
import Data.Text (Text)
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )

import Foundation
    ( Handler
    , Route (ProfileR, AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage
      ( MsgContactUs, MsgContact, MsgPhoto, MsgNavigationMenu, MsgUserProfile
      , MsgLogin
      )
    )
    
import Model (Contents(Contents), EntityField (ContentsSection))

import Menu (menu)

getContactR :: Handler Html
getContactR = do
    contacts <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgContact
        $(widgetFile "contacts/contact")

section :: Text
section = "CONTACTS"
