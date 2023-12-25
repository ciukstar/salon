{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Admin.Tokens
  ( getTokensR
  , postTokensGMailR
  , getGMailApiHookR
  ) where

import Control.Applicative ((<|>))
import qualified Control.Lens as L ((^.), (^?))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens (key, AsValue (_String), AsNumber (_Integer))
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)

import Database.Persist (Entity(Entity, entityVal), PersistUniqueWrite (upsert))
import qualified Database.Persist as P ((=.))
import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR, AdminR
      )
    , AdminR (TokensR, TokensGMailR, GMailApiHookR)
    , AppMessage
      ( MsgTokens, MsgNavigationMenu, MsgUserProfile, MsgPhoto, MsgLogin
      , MsgInitialize, MsgClearSettings, MsgDatabase, MsgUserSession
      , MsgGoogleSecretManager, MsgStoreType, MsgInitialization
      , MsgInvalidFormData, MsgRecordEdited, MsgInvalidStoreType
      ), App (appSettings)
    )

import Network.Wreq
    ( FormParam ((:=)), post, responseStatus, statusCode, responseBody
    )

import Text.Blaze.Html (preEscapedToHtml)
import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), SomeMessage (SomeMessage), getMessages, whamlet
    , newIdent, toWidget, addMessageI, redirect, setSession, getUrlRender, getYesod
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
    , (^.) ,(==.)
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

    let accessToken = r L.^. responseBody . key "access_token" . _String
    let refreshToken = r L.^. responseBody . key "refresh_token" . _String
    let _tokenType = r L.^. responseBody . key "token_type" . _String
    let _scope = r L.^. responseBody . key "scope" . _String
    let _expiresIn = r L.^? responseBody . key "expires_in" . _Integer

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
          _ <- runDB $ upsert (Token gmail x) [TokenStore P.=. x]

          
          
          addMessageI info MsgRecordEdited
          redirect $ AdminR TokensR
      Nothing -> do
          addMessageI warn MsgInvalidStoreType
          redirect $ AdminR TokensR


postTokensGMailR :: Handler Html
postTokensGMailR = do
    user <- maybeAuth

    token <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val gmail
        return x
        
    ((fr,fw),et) <- runFormPost $ formGmailApi token
    
    formStoreType <- newIdent
    case fr of
      FormSuccess x -> do
          app <- appSettings <$> getYesod
          rndr <- getUrlRender
          
          r <- liftIO $ post "https://accounts.google.com/o/oauth2/v2/auth"
              [ "redirect_uri" := rndr (AdminR GMailApiHookR)
              , "response_type" := ("code" :: Text)
              , "prompt" := ("consent" :: Text)
              , "client_id" := appGoogleClientId app
              , "scope" := ("https://www.googleapis.com/auth/gmail.send" :: Text)
              , "access_type" := ("offline" :: Text)
              , "state" := pack (show x)
              ]
             
          return $ preEscapedToHtml $ decodeUtf8 $ toStrict (r L.^. responseBody)
      _otherwize -> do
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
    
    formStoreType <- newIdent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgTokens
        $(widgetFile "admin/tokens/tokens")


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
        } ((tokenStore . entityVal <$> token) <|> Just StoreTypeSession)
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
