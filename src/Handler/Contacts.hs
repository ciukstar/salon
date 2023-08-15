{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Contacts (getContactR) where

import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Core (Yesod(defaultLayout), preEscapedToMarkup, setUltDestCurrent)
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
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage (MsgContact, MsgPhoto, MsgLogout)
    )
    
import Model (Contents(Contents), EntityField (ContentsSection))


getContactR :: Handler Html
getContactR = do
    contacts <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    muid <- maybeAuth
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgContact
        $(widgetFile "contacts/contact")

section :: Text
section = "CONTACTS"
