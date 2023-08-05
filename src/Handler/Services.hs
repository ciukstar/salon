{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

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
    , TypedContent (TypedContent), ToContent (toContent), typeSvg
    )
import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Settings (widgetFile)

import Foundation
    ( Handler
    , Route
      ( AuthR, AccountPhotoR, PhotoPlaceholderR, ServicesR
      , ServiceR, StaticR, ServiceThumbnailR
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
    , where_, val, select, isNothing, just
    )
    
import Model
    ( Service(Service), ServiceId
    , EntityField (ServiceId, ThumbnailService, ServiceGroup)
    , Thumbnail (Thumbnail)
    )

import Settings.StaticFiles
    ( img_spark_svg
    )
import Control.Monad (forM)
import Data.FileEmbed (embedFile)


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
        
    services <- forM categories $ \e@(Entity sid _) -> (e,) <$> runDB ( select $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceGroup ==. just (val sid)
        orderBy [asc (x ^. ServiceId)]
        return x )
        
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
    return $ case img of
      Just (Entity _ (Thumbnail _ photo mime)) -> TypedContent (encodeUtf8 mime) $ toContent photo
      Nothing -> TypedContent typeSvg $ toContent $(embedFile "static/img/photo_FILL0_wght400_GRAD0_opsz48.svg")

