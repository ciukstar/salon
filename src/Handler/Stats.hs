{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Stats
  ( getPopOffersR
  , getWorkloadsR
  ) where

import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler (setUltDestCurrent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (runDB) 

import Database.Persist (Entity (Entity))

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (Value), select, from, table, innerJoin, on, subSelectCount
    , (:&)((:&)), (^.), (==.)
    , where_, orderBy, desc
    )

import Foundation
    ( Handler
    , Route (AccountPhotoR, PhotoPlaceholderR, ProfileR, AuthR)
    , AppMessage
      ( MsgPopularOffers, MsgUserProfile, MsgPhoto, MsgNavigationMenu, MsgLogin
      , MsgNoDataToDisplay, MsgService, MsgBookings, MsgNumberSign, MsgWorkload
      )
    )

import Model
    ( Offer (Offer), Book, Service (Service)
    , EntityField(OfferId, BookOffer, ServiceId, OfferService)
    )

import Settings (widgetFile)
import Menu (menu)


getWorkloadsR :: Handler Html
getWorkloadsR = do
    user <- maybeAuth
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgWorkload
        $(widgetFile "stats/workloads/workloads")


getPopOffersR :: Handler Html
getPopOffersR = do
    user <- maybeAuth
    offers <- zip [1 :: Int ..] <$> runDB ( select $ do
        o :& s <- from $ table @Offer
            `innerJoin` table @Service `on` (\(o :& s) -> o ^. OfferService ==. s ^. ServiceId)
        let n :: SqlExpr (Value Int)
            n = subSelectCount $ from (table @Book) >>= \b -> where_ $ b ^. BookOffer ==. o ^. OfferId
        orderBy [desc n]
        return (s,o,n) )
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgPopularOffers
        $(widgetFile "stats/pop-offers/offers")
