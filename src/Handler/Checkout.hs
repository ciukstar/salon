{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Checkout (getCheckoutR, postCreatePaymentIntentR) where

import Foundation
    ( Handler, Route (CheckoutR)
    , App (appSettings)
    , AppMessage (MsgCheckout)
    )
import Text.Hamlet (Html)
import Model (UserId)
import Yesod.Core.Widget (addScriptRemote, setTitleI)
import Yesod.Core
    ( Yesod(defaultLayout), getYesod, returnJson
    )
import Settings (widgetFile, AppSettings (appStripeSk, appStripePk))

import Network.Wreq
    ( postWith, defaults, auth, basicAuth, FormParam ((:=)), responseBody
    )
import Data.Text (Text)
import Data.Function ((&))
import Control.Lens ((?~),(^?))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens (key)
import Data.Aeson (Value, object, (.=))
import Data.Text.Encoding (encodeUtf8)


stripeApi :: String
stripeApi = "https://api.stripe.com/v1/payment_intents"

postCreatePaymentIntentR :: Handler Value
postCreatePaymentIntentR = do
    sk <- encodeUtf8 . appStripeSk . appSettings <$> getYesod
    let opts = defaults & auth ?~ basicAuth sk "" 
    r <- liftIO $ postWith opts stripeApi [ "amount" := (1300 :: Int)
                                          , "currency" := ("usd" :: Text)
                                          , "payment_method_types[]" := ("card" :: Text)
                                          ]
    returnJson $ object [ "clientSecret" .= (r ^? responseBody . key "client_secret")]


getCheckoutR :: UserId -> Handler Html
getCheckoutR uid = do
    pk <- appStripePk . appSettings <$> getYesod
    defaultLayout $ do
        addScriptRemote "https://js.stripe.com/v3/"
        setTitleI MsgCheckout
        $(widgetFile "book/checkout/checkout")
