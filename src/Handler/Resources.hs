{-# LANGUAGE TemplateHaskell #-}

module Handler.Resources (getDocsR) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Settings (widgetFile)
import Settings.StaticFiles (img_Salon_ERD_svg)
import Database.Persist (Entity (Entity))
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Foundation
    ( Handler
    , Route (StaticR, AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage
      ( MsgDocumentation, MsgErdDiagram, MsgPhoto, MsgLogout
      , MsgAppName, MsgDoc001
      )
    )

getDocsR :: Handler Html
getDocsR = do
    muid <- maybeAuth
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgDocumentation
        $(widgetFile "resources/docs")
