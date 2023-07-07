{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Account (getAccountR) where

import Data.Maybe (fromMaybe)
import Text.Hamlet (Html)
import Settings (widgetFile)

import Yesod.Core
    ( Yesod(defaultLayout)
    , setTitleI, getUrlRender, lookupSession, whamlet, SomeMessage (SomeMessage)
    )

import Foundation
    ( Handler, Widget
    , Route (HomeR, AccountR)
    , AppMessage
      ( MsgAccount, MsgSignUp, MsgCancel, MsgUsername, MsgPassword
      , MsgPhoto
      )
    )
import Yesod.Form.Types
    ( MForm, FormResult, FieldView (fvInput, fvLabel)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form (generateFormPost, mreq, textField, passwordField)

import Model (User (userName, User, userPassword), sessKeyULT)


getAccountR :: Handler Html
getAccountR = do
    (widget,enctype) <- generateFormPost $ formAccount Nothing
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$> lookupSession sessKeyULT
    defaultLayout $ do
        setTitleI MsgAccount
        $(widgetFile "account")


formAccount :: Maybe User -> Html -> MForm Handler (FormResult User, Widget)
formAccount user extra = do
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgUsername
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userName <$> user)
    (passR,passV) <- mreq passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userPassword <$> user)
    let r = User <$> nameR <*> passR
    let w = [whamlet|
#{extra}
$forall v <- [nameV,passV]
  <label.mdc-text-field.mdc-text-field--filled>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel v}
    ^{fvInput v}
    <span.mdc-line-ripple>
|]
    return (r, w)
