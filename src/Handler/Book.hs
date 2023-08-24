{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Book (getBookR) where

import Text.Hamlet (Html)
import Yesod.Core (Yesod(defaultLayout), setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Settings (widgetFile)

import Yesod.Persist.Core (runDB)
import Database.Persist ( Entity(Entity) )
import Database.Persist.Sql ( fromSqlKey )

import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , orderBy, asc
    )

import Foundation
    ( Handler
    , Route (AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage
      ( MsgBook, MsgPhoto, MsgLogout, MsgChooseServicesToBook
      )
    )
    
import Model
    ( Service(Service)
    , Pricelist (Pricelist)
    , EntityField (ServiceId, PricelistService, ServicePublished, ServiceName)
    )


getBookR :: Handler Html
getBookR = do
    muid <- maybeAuth
    offers <- runDB $ select $ do
        x :& p <- from $ table @Service `innerJoin` table @Pricelist
            `on` (\(x :& p) -> x ^. ServiceId ==. p ^. PricelistService)
        where_ $ x ^. ServicePublished
        orderBy [asc (x ^. ServiceName)]
        return (x,p)
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgBook
        $(widgetFile "book/book")
