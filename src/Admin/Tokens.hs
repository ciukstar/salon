{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Admin.Tokens
  ( getTokensR
  , postTokensR
  ) where

import Data.Text (Text)
import Database.Persist (Entity(Entity))
import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR, AdminR
      )
    , AdminR (TokensR)
    , AppMessage
      ( MsgTokens, MsgNavigationMenu, MsgUserProfile, MsgPhoto, MsgLogin
      , MsgInitialize, MsgClearSettings, MsgDatabase, MsgUserSession
      , MsgGoogleSecretManager, MsgStoreType, MsgInitialization
      , MsgInvalidFormData
      )
    )
import Text.Hamlet (Html)
import Text.Cassius (cassius)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), getMessages, whamlet
    , newIdent, toWidget, addMessageI
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Fields (OptionList, optionsPairs, withRadioField)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), Field, FieldView (fvInput, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
    
import Model
    ( StoreType (StoreTypeSession, StoreTypeDatabase, StoreTypeGoogleSecretManager)
    )

import Menu (menu)
import Settings (widgetFile)


postTokensR :: Handler Html
postTokensR = do
    user <- maybeAuth
    ((fr,fw),et) <- runFormPost formGmailApi
    
    formStoreType <- newIdent
    case fr of
      FormSuccess StoreTypeDatabase -> undefined
      FormSuccess StoreTypeGoogleSecretManager -> undefined
      _otherwize -> do
          addMessageI warn MsgInvalidFormData
          msgs <- getMessages          
          defaultLayout $ do
              setTitleI MsgTokens
              $(widgetFile "admin/tokens/tokens")


getTokensR :: Handler Html
getTokensR = do    
    user <- maybeAuth

    (fw,et) <- generateFormPost formGmailApi
    
    formStoreType <- newIdent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTokens
        $(widgetFile "admin/tokens/tokens")


formGmailApi :: Html -> MForm Handler (FormResult StoreType,Widget)
formGmailApi extra = do
    let storeTypes = [ (MsgGoogleSecretManager,StoreTypeGoogleSecretManager)
                     , (MsgDatabase,StoreTypeDatabase)
                     , (MsgUserSession,StoreTypeSession)
                     ]
    (r,v) <- mreq (mdcStoreTypeField (optionsPairs storeTypes)) FieldSettings
        { fsLabel = SomeMessage MsgStoreType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-radio__native-control")]
        } (Just StoreTypeSession)
    return (r, do
                 toWidget [cassius|
##{fvId v}
  display: flex
  flex-direction: column
|]
                 [whamlet|
#{extra}
^{fvInput v}
|])
  where
      mdcStoreTypeField :: Handler (OptionList StoreType) -> Field Handler StoreType
      mdcStoreTypeField = withRadioField
          (\_ _ -> [whamlet||])
          (\theId value _isSel text optionW -> [whamlet|
<div.mdc-form-field.mdc-touch-target-wrapper>
  <div.mdc-radio.mdc-radio--touch>
    ^{optionW}
    <div.mdc-radio__background>
      <div.mdc-radio__outer-circle>
      <div.mdc-radio__inner-circle>
    <div.mdc-radio__ripple>
    <div.mdc-radio__focus-ring>
  <label for=#{theId}-#{value}>#{text}
|])

    
warn :: Text
warn = "warn"
