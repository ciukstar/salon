{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Text.Hamlet (Html)
import Settings (widgetFile)
import Yesod.Core (Yesod(defaultLayout), setTitleI)

import Foundation
    ( Handler
    , Route (StaticR, ServicesR, AboutR)
    , AppMessage
      ( MsgSalon, MsgWelcome, MsgServices, MsgAboutUs
      , MsgBook
      )
    )

import Settings.StaticFiles (img_salon_svg)

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitleI MsgWelcome
        $(widgetFile "homepage")

