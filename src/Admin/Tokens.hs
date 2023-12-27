{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Admin.Tokens
  ( getTokensR
  , postTokensGMailR
  , getGMailApiHookR
  , postTokensGMailClearR
  ) where

import Control.Exception.Safe (tryAny)
import Control.Monad (forM_)
import qualified Control.Lens as L ((^.), (^?), (?~))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key, AsValue (_String), AsNumber (_Integer))
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Function ((&))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Database.Persist
    ( Entity(Entity, entityVal)
    , PersistStoreWrite (delete)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.))
import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR, AdminR
      )
    , AdminR (TokensR, TokensGMailR, GMailApiHookR, TokensGMailClearR)
    , AppMessage
      ( MsgTokens, MsgNavigationMenu, MsgUserProfile, MsgPhoto, MsgLogin
      , MsgInitialize, MsgClearSettings, MsgDatabase, MsgUserSession
      , MsgGoogleSecretManager, MsgStoreType, MsgInitialization
      , MsgInvalidFormData, MsgRecordEdited, MsgInvalidStoreType
      , MsgRecordDeleted, MsgCleared
      ), App (appSettings)
    )

import Network.Wreq
    ( FormParam ((:=)), post, responseStatus, statusCode, responseBody, defaults
    , auth, oauth2Bearer, postWith
    )

import Text.Blaze.Html (preEscapedToHtml)
import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), getMessages, whamlet
    , newIdent, toWidget, addMessageI, redirect, setSession, getUrlRender
    , getYesod, deleteSession
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (OptionList, optionsPairs, withRadioField, textField)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), Field, FieldView (fvInput, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
    
import Model
    ( StoreType (StoreTypeSession, StoreTypeDatabase, StoreTypeGoogleSecretManager)
    , Token (Token, tokenStore), EntityField (TokenApi, TokenStore, StoreVal)
    , gmailAccessToken, gmailRefreshToken, Store (Store), gmail
    )

import Menu (menu)
import Settings (widgetFile, AppSettings (appGoogleClientId, appGoogleClientSecret))


getGMailApiHookR :: Handler Html
getGMailApiHookR = do
    rndr <- getUrlRender
    app <- appSettings <$> getYesod
    let googleClientId = appGoogleClientId app
    let googleClientSecret = appGoogleClientSecret app
    
    code <- runInputGet $ ireq textField "code"
    store <- readMaybe .  unpack <$> runInputGet (ireq textField "state")

    r <- liftIO $ post "https://oauth2.googleapis.com/token"
         [ "code" := code
         , "redirect_uri" := rndr (AdminR GMailApiHookR)
         , "client_id" := googleClientId
         , "client_secret" := googleClientSecret
         , "grant_type" := ("authorization_code" :: Text)
         ]

    let _status = r L.^. responseStatus . statusCode
    let _tokenType = r L.^. responseBody . key "token_type" . _String
    let _scope = r L.^. responseBody . key "scope" . _String
    let _expiresIn = r L.^? responseBody . key "expires_in" . _Integer

    let accessToken = r L.^. responseBody . key "access_token" . _String
    let refreshToken = r L.^. responseBody . key "refresh_token" . _String

    case store of
      Just x@StoreTypeSession -> do
          setSession gmailAccessToken accessToken
          setSession gmailRefreshToken refreshToken
          _ <- runDB $ upsert (Token gmail x) [TokenStore P.=. x]
          addMessageI info MsgRecordEdited
          redirect $ AdminR TokensR
      Just x@StoreTypeDatabase -> do
          Entity tid _ <- runDB $ upsert (Token gmail x) [TokenStore P.=. x]
          _ <- runDB $ upsert (Store tid gmailAccessToken accessToken) [StoreVal P.=. accessToken]
          _ <- runDB $ upsert (Store tid gmailRefreshToken refreshToken) [StoreVal P.=. refreshToken]
          addMessageI info MsgRecordEdited
          redirect $ AdminR TokensR
      Just x@StoreTypeGoogleSecretManager -> do

          let apis = [ ( "https://secretmanager.googleapis.com/v1/projects/salon-395815/secrets/gmail_access_token:addVersion"
                       , accessToken
                       )
                     , ( "https://secretmanager.googleapis.com/v1/projects/salon-395815/secrets/gmail_refresh_token:addVersion"
                       , refreshToken
                       )
                     ]
          
          let opts = defaults & auth L.?~ oauth2Bearer (encodeUtf8 accessToken)

          forM_ apis $ \(api,secret) -> do
              liftIO $ tryAny $ postWith opts api
                  (object [ "payload" .= object [ "data" .= decodeUtf8 (B64.encode (encodeUtf8 secret)) ]])
          
          _ <- runDB $ upsert (Token gmail x) [TokenStore P.=. x]
          addMessageI info MsgRecordEdited
          redirect $ AdminR TokensR
      Nothing -> do
          addMessageI warn MsgInvalidStoreType
          redirect $ AdminR TokensR


postTokensGMailClearR :: Handler Html
postTokensGMailClearR = do

    token <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val gmail
        return x
        
    ((fr2,fw2),et2) <- runFormPost formGmailApiClear
    case (fr2,token) of
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeSession))) -> do
          deleteSession gmailAccessToken
          deleteSession gmailRefreshToken
          runDB $ delete tid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR TokensR
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeDatabase))) -> do
          runDB $ delete tid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR TokensR
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeGoogleSecretManager))) -> do
          undefined
          runDB $ delete tid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR TokensR
      (FormSuccess (),Nothing) -> do
          addMessageI info MsgCleared
          redirect $ AdminR TokensR
      _otherwise -> do
          user <- maybeAuth
          formStoreType <- newIdent
          formStoreTypeClear <- newIdent
          (fw,et) <- generateFormPost $ formGmailApi token
          addMessageI warn MsgInvalidFormData
          msgs <- getMessages     
          defaultLayout $ do
              setTitleI MsgTokens
              $(widgetFile "admin/tokens/tokens")


postTokensGMailR :: Handler Html
postTokensGMailR = do
    user <- maybeAuth

    token <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val gmail
        return x
        
    ((fr,fw),et) <- runFormPost $ formGmailApi token
    case fr of
      FormSuccess x -> do
          app <- appSettings <$> getYesod
          rndr <- getUrlRender

          let scope :: Text
              scope = "https://www.googleapis.com/auth/cloud-platform https://www.googleapis.com/auth/gmail.send"
          
          r <- liftIO $ post "https://accounts.google.com/o/oauth2/v2/auth"
              [ "redirect_uri" := rndr (AdminR GMailApiHookR)
              , "response_type" := ("code" :: Text)
              , "prompt" := ("consent" :: Text)
              , "client_id" := appGoogleClientId app
              , "scope" := scope
              , "access_type" := ("offline" :: Text)
              , "state" := pack (show x)
              ]
             
          return $ preEscapedToHtml $ decodeUtf8 $ toStrict (r L.^. responseBody)
      _otherwize -> do
          formStoreType <- newIdent
          formStoreTypeClear <- newIdent
          (fw2,et2) <- generateFormPost formGmailApiClear
          addMessageI warn MsgInvalidFormData
          msgs <- getMessages     
          defaultLayout $ do
              setTitleI MsgTokens
              $(widgetFile "admin/tokens/tokens")


getTokensR :: Handler Html
getTokensR = do    
    user <- maybeAuth

    token <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val gmail
        return x

    (fw,et) <- generateFormPost $ formGmailApi token
    (fw2,et2) <- generateFormPost formGmailApiClear
    formStoreType <- newIdent
    formStoreTypeClear <- newIdent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTokens
        $(widgetFile "admin/tokens/tokens")


formGmailApiClear :: Html -> MForm Handler (FormResult (),Widget)
formGmailApiClear extra = return (FormSuccess (),[whamlet|#{extra}|])


formGmailApi :: Maybe (Entity Token) -> Html -> MForm Handler (FormResult StoreType,Widget)
formGmailApi token extra = do
    let storeTypes = [ (MsgGoogleSecretManager,StoreTypeGoogleSecretManager)
                     , (MsgDatabase,StoreTypeDatabase)
                     , (MsgUserSession,StoreTypeSession)
                     ]
    (r,v) <- mreq (mdcStoreTypeField (optionsPairs storeTypes)) FieldSettings
        { fsLabel = SomeMessage MsgStoreType
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-radio__native-control")]
        } (tokenStore . entityVal <$> token)
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

    
info :: Text
info = "info"

    
warn :: Text
warn = "warn"
