{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Services
  ( getServicesR
  , getServiceCreateFormR
  ) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setTitleI, setUltDestCurrent, getMessages, whamlet)
import Yesod.Auth (Route (LoginR), maybeAuth)
import Settings (widgetFile)

import Foundation
    ( Handler
    , Route
      ( AuthR, AccountPhotoR, PhotoPlaceholderR, ServicesR
      , ServiceCreateFormR
      )
    , AppMessage
      ( MsgServices, MsgAdd, MsgPhoto, MsgService
      ), Widget
    )

import Yesod.Persist.Core (runDB)
import Database.Persist (Entity (Entity, entityVal))

import Database.Esqueleto.Experimental
    (selectOne, from, table, orderBy, asc
    , (^.)
    )
    
import Model (Service(Service, serviceName, serviceDescr, servicePrice), EntityField (ServiceId))
import Yesod.Form.Types (MForm, FormResult)
import Yesod.Form (mreq, textField, mopt, doubleField, textareaField)


getServiceCreateFormR :: Handler Html
getServiceCreateFormR = do
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "services/create")


getServicesR :: Handler Html
getServicesR = do
    services <- runDB $ selectOne $ do
        x <- from $ table @Service
        orderBy [asc (x ^. ServiceId)]
        return x
    muid <- maybeAuth
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgServices
        $(widgetFile "services/services")


{-
formCreate :: Maybe (Entity Service) -> Html -> MForm Handler (FormResult Service, Widget)
formCreate s extra = do
    (nameR,nameV) <- mreq textField "Name" (serviceName . entityVal <$> s)
    (descrR,descrV) <- mopt textareaField "Descr" (serviceDescr . entityVal <$> s)
    (priceR,priceV) <- mreq doubleField "Price" (fromIntegral servicePrice . entityVal <$> s)

    let r = Service <$> nameR <*> descrR <*> (round <$> priceR)
    let w = [whamlet||]
    return (r,w)
-}
