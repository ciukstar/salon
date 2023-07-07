{-# LANGUAGE TemplateHaskell #-}

module Handler.Users (getUsersR, getUserCreateFormR) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setTitleI)
import Settings (widgetFile)

import Foundation (Handler, AppMessage (MsgSignUp))


getUserCreateFormR :: Handler Html
getUserCreateFormR = do
    defaultLayout $ do
        setTitleI MsgSignUp
        $(widgetFile "users/create")


getUsersR :: Handler Html
getUsersR = undefined
