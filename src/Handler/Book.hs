{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Handler.Book
  ( getBookR
  , getBookStaffR
  ) where

import Control.Monad (forM)
import Data.Fixed (Centi)
import qualified Data.List.Safe as LS (head)
import Data.Text (unpack, intercalate)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getRequest
    , YesodRequest (reqGetParams), getYesod, languages
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
    , orderBy, asc, selectOne, val, desc
    )

import Foundation
    ( Handler
    , Route (BookR, BookStaffR, AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR)
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgBook, MsgPhoto, MsgLogout, MsgChooseServicesToBook
      , MsgServices, MsgSymbolHour, MsgSymbolMinute, MsgStaff
      , MsgNoStaffAvailable, MsgNoPreference, MsgMaximumAvailability
      , MsgSelectStaff
      )
    )

import Model
    ( Service(Service)
    , Offer (Offer), OfferId
    , Staff (Staff)
    , Role (Role)
    , EntityField
      ( StaffId, RoleId, ServiceId, OfferService, ServicePublished
      , ServiceName, RoleStaff, RoleRating
      )
    )


getBookStaffR :: Handler Html
getBookStaffR = do
    staff <- runDB $ select $ do
        x <- from $ table @Staff
        orderBy [asc (x ^. StaffId)]
        return x
    roles <- forM staff ( \e@(Entity eid _) -> (e,) <$> runDB ( selectOne $ do
          x <- from $ table @Role
          where_ $ x ^. RoleStaff ==. val eid
          orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
          return x ) )
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "book/staff")


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
    app <- getYesod
    langs <- languages
    defaultLayout $ do
        setTitleI MsgBook
        $(widgetFile "book/book")


amount :: [OfferId] -> [(Entity Service, Entity Offer)] -> Centi
amount oids xs = sum (
    (\(_,Entity _ (Offer _ _ price _ _ _)) -> price)
      <$> filter (\(_,Entity oid _) -> oid `elem` oids) xs
    )


range :: Enum a => a -> a -> [a]
range a b = [a..b]
