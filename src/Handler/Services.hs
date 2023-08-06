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

import Control.Monad (forM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.FileEmbed (embedFile)
import Data.Text (pack)
import Text.Hamlet (Html)
import Data.Text.Encoding (encodeUtf8)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages, typeSvg
    , TypedContent (TypedContent), ToContent (toContent)
    )
import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Yesod.Form.Input (iopt, runInputGet)
import Yesod.Form.Fields (textField, intField)
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
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Esqueleto.Experimental
    (selectOne, from, table, orderBy, asc
    , (^.), (==.)
    , where_, val, select, isNothing, just
    )
    
import Model
    ( Service(Service), ServiceId
    , EntityField (ServiceId, ThumbnailService, ServiceGroup, PricelistService, PricelistId)
    , Thumbnail (Thumbnail), Pricelist (Pricelist), Services (Services)
    )

import Settings.StaticFiles
    ( img_spark_svg
    )


getServiceR :: ServiceId -> Services -> Handler Html
getServiceR sid sids = do
    open <- (("open",) <$>) <$> runInputGet (iopt textField "open")
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "services/service")


getServicesR :: Services -> Handler Html
getServicesR (Services sids) = do
    open <- runInputGet (iopt textField "open")
    scrollY <- runInputGet (iopt textField "scrollY")
    msid <- (toSqlKey <$>) <$> runInputGet (iopt intField "sid")
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

    pricelist <- forM services $ \(g, ss) -> (g,) <$> forM ss ( \s@(Entity sid _) -> (s,) <$> runDB ( select $ do
        x <- from $ table @Pricelist
        where_ $ x ^. PricelistService ==. val sid
        orderBy [asc (x ^. PricelistId)]
        return x ) )
        
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

