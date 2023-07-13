{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Services
  ( getServicesR
  , getServiceThumbnailR
  , getServiceR
  ) where

import Text.Hamlet (Html)
import Data.Text.Encoding (encodeUtf8)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages
    , TypedContent (TypedContent), notFound, ToContent (toContent)
    )
import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Settings (widgetFile)

import Foundation
    ( Handler
    , Route
      ( AuthR, AccountPhotoR, PhotoPlaceholderR, ServicesR
      , ServiceR, StaticR
      )
    , AppMessage
      ( MsgServices, MsgPhoto, MsgService
      , MsgThumbnail
      , MsgNoServicesYet
      , MsgLogout
      )
    )

import Yesod.Persist.Core (runDB)
import Database.Persist
    ( Entity (Entity)
    )
import Database.Persist.Sql (fromSqlKey)
import Database.Esqueleto.Experimental
    (selectOne, from, table, orderBy, asc
    , (^.), (==.)
    , where_, val, select, isNothing, not_
    )
    
import Model
    ( Service(Service), ServiceId
    , EntityField (ServiceId, ThumbnailService, ServiceGroup)
    , Thumbnail (Thumbnail)
    )

import Settings.StaticFiles
    ( img_spark_svg
    , img_photo_FILL0_wght400_GRAD0_opsz48_svg
    )


getServiceR :: ServiceId -> Handler Html
getServiceR sid = do
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "services/service")


getServicesR :: Handler Html
getServicesR = do
    categories <- runDB $ select $ do
        x <- from $ table @Service
        where_ $ isNothing $ x ^. ServiceGroup
        orderBy [asc (x ^. ServiceId)]
        return x
    services <- runDB $ select $ do
        x <- from $ table @Service
        where_ $ not_ $ isNothing $ x ^. ServiceGroup
        orderBy [asc (x ^. ServiceId)]
        return x
    muid <- maybeAuth
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgServices
        $(widgetFile "services/services")


getServiceThumbnailR :: ServiceId -> Handler TypedContent
getServiceThumbnailR sid = do
    img <- runDB $ selectOne $ do
       x <- from $ table @Thumbnail
       where_ $ x ^. ThumbnailService ==. val sid
       return x
    case img of
      Just (Entity _ (Thumbnail _ photo mime)) -> return $ TypedContent (encodeUtf8 mime) $ toContent photo
      Nothing -> notFound

