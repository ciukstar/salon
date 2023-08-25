{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Book (getBookR) where

import qualified Data.List.Safe as LS (head)
import Data.Text (unpack)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getRequest
    , YesodRequest (reqGetParams)
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Settings (widgetFile)

import Yesod.Persist.Core (runDB)
import Database.Persist ( Entity(Entity) )
import Database.Persist.Sql ( fromSqlKey, toSqlKey )

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
      , MsgServices
      )
    )
    
import Model
    ( Service(Service)
    , Offer (Offer), OfferId
    , EntityField (ServiceId, OfferService, ServicePublished, ServiceName)
    )
import Data.Fixed (Centi)


getBookR :: Handler Html
getBookR = do
    muid <- maybeAuth
    oids <- (toSqlKey . read . unpack . snd <$>) . filter ((== "oid") . fst) . reqGetParams <$> getRequest
    offers <- runDB $ select $ do
        x :& p <- from $ table @Service `innerJoin` table @Offer
            `on` (\(x :& p) -> x ^. ServiceId ==. p ^. OfferService)
        where_ $ x ^. ServicePublished
        orderBy [asc (x ^. ServiceName)]
        return (x,p)
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgBook
        $(widgetFile "book/book")


amount :: [OfferId] -> [(Entity Service, Entity Offer)] -> Centi
amount oids xs = sum (
    (\(_,Entity _ (Offer _ _ price _ _ _)) -> price)
      <$> filter (\(_,Entity oid _) -> oid `elem` oids) xs
    )
