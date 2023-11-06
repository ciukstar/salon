{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Contacts (getContactR) where

import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages)
import Yesod.Core.Widget (setTitleI)
import Settings (widgetFile)

import Database.Persist (Entity (Entity))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( selectOne, from, table )

import Foundation
    ( Handler
    , Route (ProfileR, AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage
      ( MsgContactUs, MsgContact, MsgPhoto, MsgNavigationMenu, MsgUserProfile
      , MsgLogin, MsgNoContentYet
      )
    )
    
import Model (ContactUs (ContactUs))

import Menu (menu)

getContactR :: Handler Html
getContactR = do
    contacts <- runDB $ selectOne $ from $ table @ContactUs
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgContact
        $(widgetFile "contacts/contact")

