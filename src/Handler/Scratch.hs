{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Scratch
  ( getScratchInitR
  , postScratchInitR
  , getScratchOneR
  , getScratchTwoR
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Foundation
    ( Handler, App (appSettings)
    , Route (ScratchR)
    , ScratchR (ScratchInitR, ScratchOneR)
    )
import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), getYesod, whamlet, getUrlRender)
import Yesod.Core.Handler (redirect)
import Settings (AppSettings (appGoogleClientId, appGoogleClientSecret))

import Network.Wreq
    ( post, FormParam ((:=)), responseBody, responseStatus, statusCode
    , postWith, defaults, auth, oauth2Bearer
    )
import Yesod.Form.Input (ireq, runInputGet, runInputPost)
import Yesod.Form.Fields (textField)
import Data.Aeson.Lens (AsValue(_String), key, AsNumber (_Integer))
import Control.Lens ((^.),(^?),(?~))
import Network.Mail.Mime (simpleMail', Address (Address), renderMail')
import Data.Text.Lazy (fromStrict)
import Data.Function ((&))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Base64.Lazy (encode)
import Data.Aeson (object, (.=))
import Data.ByteString (toStrict)


postScratchInitR :: Handler Html
postScratchInitR = do

    accessToken <- runInputPost $ ireq textField "access_token"
    to <- runInputPost $ ireq textField "to"
    from <- runInputPost $ ireq textField "from"
    subject <- runInputPost $ ireq textField "subject"
    body <- runInputPost $ ireq textField "body"
    
    let api = "https://gmail.googleapis.com/gmail/v1/users/me/messages/send"

    mail <- liftIO $ encode <$> renderMail' ( simpleMail'
        (Address (Just "Sergiu Starciuc") to)
        (Address (Just "Sergiu Starciuc") from)
        subject (fromStrict body) )

    let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken)

    _ <- liftIO $ postWith opts api (object ["raw" .= decodeUtf8 (toStrict mail)])

    redirect $ ScratchR ScratchInitR
    

getScratchTwoR :: Handler Html
getScratchTwoR = do
    defaultLayout [whamlet|Two...|]


getScratchOneR :: Handler Html
getScratchOneR = do
    rndr <- getUrlRender
    app <- appSettings <$> getYesod
    let googleClientId = appGoogleClientId app
    let googleClientSecret = appGoogleClientSecret app
    
    code <- runInputGet $ ireq textField "code"
    
    r <- liftIO $ post "https://oauth2.googleapis.com/token"
         [ "code" := code
         , "redirect_uri" := rndr (ScratchR ScratchOneR)
         , "client_id" := googleClientId
         , "client_secret" := googleClientSecret
         , "grant_type" := ("authorization_code" :: Text)
         ]

    let status = r ^. responseStatus . statusCode

    let accessToken = r ^. responseBody . key "access_token" . _String
    let refreshToken = r ^. responseBody . key "refresh_token" . _String
    let tokenType = r ^. responseBody . key "token_type" . _String
    let scope = r ^. responseBody . key "scope" . _String
    let expiresIn = r ^? responseBody . key "expires_in" . _Integer

    defaultLayout [whamlet|
<form method=post action=@{ScratchR ScratchInitR}
  style="margin:3rem;display:flex;flex-direction:column;gap:1rem">
  <label>
    To:
    <input type=email name=to value="ciukstar@gmail.com">
  <label>
    From:
    <input type=email name=from value="ciukstar@gmail.com">
  <label>
    Subject:
    <input type=text name=subject value="Simple subject">
  <label>
    Body:
    <input type=textarea name=body value="Simple message">

  <label>
    Access token:
    <input type=text name=access_token value=#{accessToken}>

  <button type=submit>Submit



  <details>
    <summary>Respose status
    <h1>Response
    <h4>status:
    #{status}
    <h4>accessToken:
    #{accessToken}
    <h4>refreshToken:
    #{refreshToken}
    <h4>tokenType:
    #{tokenType}
    <h4>scope:
    #{scope}
    <h4>expiresIn:
    $maybe expiresIn <- expiresIn
      #{expiresIn}
|]
    

getScratchInitR :: Handler Html
getScratchInitR = do
    app <- appSettings <$> getYesod
    let googleClientId = appGoogleClientId app
    
    defaultLayout [whamlet|
<form method=post action="https://accounts.google.com/o/oauth2/v2/auth"
  style="margin:3rem;display:flex;flex-direction:column;gap:1rem">
  <input type=url name=redirect_uri value=@{ScratchR ScratchOneR}>
  <input type=text name=response_type value=code>
  <input type=text name=prompt value=consent>
  <input type=text name=client_id value=#{googleClientId}>
  <input type=text name=scope value="https://www.googleapis.com/auth/gmail.send">
  <input type=text name=access_type value="offline">
  
  <button type=submit>Get authorization code
|]
