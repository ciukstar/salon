{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Stats
  ( getPopOffersR
  , getWorkloadsR
  ) where


import Data.Bifunctor (second)
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Data.Time.LocalTime (timeOfDayToTime)
import qualified Data.Map.Lazy as M (fromListWith, fromList, toList)
import qualified Data.Map.Merge.Lazy as ML (merge, mapMissing, zipWithMatched)
import Text.Hamlet (Html)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler (setUltDestCurrent, newIdent)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (runDB)

import Database.Persist (Entity (Entity))

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (Value), select, from, table, innerJoin, on, subSelectCount
    , (:&)((:&)), (^.), (==.)
    , where_, orderBy, desc, just, groupBy, sum_, coalesceDefault, val, between
    )

import Foundation
    ( Handler
    , Route (AccountPhotoR, PhotoPlaceholderR, ProfileR, AuthR, AdminR)
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgPopularOffers, MsgUserProfile, MsgPhoto, MsgNavigationMenu, MsgLogin
      , MsgNoDataToDisplay, MsgService, MsgBookings, MsgNumberSign, MsgWorkload
      , MsgChoosePeriod, MsgSelect, MsgCancel
      )
    )

import Model
    ( Offer (Offer), Book, Service (Service), Role, Staff, Schedule
    , EntityField
      ( OfferId, BookOffer, ServiceId, OfferService, BookRole, RoleId, RoleStaff
      , StaffId, BookDay, RoleDuration, ScheduleStaff, ScheduleWorkDay, ScheduleWorkEnd
      , ScheduleWorkStart, StaffName
      )
    )

import Settings (widgetFile)
import Menu (menu)


getWorkloadsR :: Day -> Day -> Handler Html
getWorkloadsR start end = do

    booked <- (unwrap <$>) <$> runDB ( select $ do
        b :& r :& e <- from $ table @Book
            `innerJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. just (r ^. RoleId))
            `innerJoin` table @Staff `on` (\(_ :& r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
        where_ $ (b ^. BookDay) `between` (val start, val end)
        let dur = coalesceDefault [sum_ (r ^. RoleDuration)] (val 0) :: SqlExpr (Value DiffTime)

        groupBy (e ^. StaffId, e ^. StaffName)
        return (e ^. StaffId, e ^. StaffName, dur) )

    planned <- (calcdur <$>) <$> runDB ( select ( do
        s :& e <- from $ table @Schedule
            `innerJoin` table @Staff `on` (\(s :& e) -> s ^. ScheduleStaff ==. e ^. StaffId)
        return (e ^. StaffId, e ^. StaffName, s ^. ScheduleWorkStart, s ^. ScheduleWorkEnd) ) )

    let ratios = M.toList $ ML.merge
          (ML.mapMissing $ \ _ _ -> 0)
          (ML.mapMissing $ \ _ _ -> 0)
          (ML.zipWithMatched $ \ _ x y -> (if 0 == y then 0 else x / y :: Double))
          (M.fromListWith (+) . (second (fromIntegral . diffTimeToPicoseconds) <$>) $ booked)
          (M.fromListWith (+) . (second (fromIntegral . diffTimeToPicoseconds) <$>) $ planned)

    user <- maybeAuth
    setUltDestCurrent
    toolbar <- newIdent
    dlgTimeFrame <- newIdent
    defaultLayout $ do
        setTitleI MsgWorkload
        $(widgetFile "stats/workloads/workloads")

  where

      unwrap (Value eid,Value ename,Value dur) = ((eid,ename),dur)

      calcdur (Value eid,Value ename,Value start,Value end) =
          ((eid,ename),timeOfDayToTime end - timeOfDayToTime start)


getWorkloadsPerDayR :: Handler Html
getWorkloadsPerDayR = do

    booked <- (unwrap <$>) <$> runDB ( select $ do
        b :& r :& e <- from $ table @Book
            `innerJoin` table @Role `on` (\(b :& r) -> b ^. BookRole ==. just (r ^. RoleId))
            `innerJoin` table @Staff `on` (\(_ :& r :& e) -> r ^. RoleStaff ==. e ^. StaffId)

        let dur = coalesceDefault [sum_ (r ^. RoleDuration)] (val 0) :: SqlExpr (Value DiffTime)

        groupBy (e ^. StaffId, b ^. BookDay)
        return (e ^. StaffId, e ^. StaffName, b ^. BookDay, dur) )

    planned <- (calcdur <$>) <$> runDB ( select ( do
        s :& e <- from $ table @Schedule
            `innerJoin` table @Staff `on` (\(s :& e) -> s ^. ScheduleStaff ==. e ^. StaffId)
        return (e ^. StaffId, e ^. StaffName, s ^. ScheduleWorkDay, s ^. ScheduleWorkStart, s ^. ScheduleWorkEnd) ) )

    let ratios = M.toList $ ML.merge
          (ML.mapMissing $ \ _ _ -> 0)
          (ML.mapMissing $ \ _ _ -> 0)
          (ML.zipWithMatched $ \ _ x y -> (if 0 == y then 0 else x / y :: Double))
          (M.fromList . (second (fromIntegral . diffTimeToPicoseconds) <$>) $ booked)
          (M.fromList . (second (fromIntegral . diffTimeToPicoseconds) <$>) $ planned)



    user <- maybeAuth
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgWorkload
        -- $(widgetFile "stats/workloads/workloads")

  where

      unwrap (Value eid,Value ename,Value day,Value dur) = ((eid,ename,day),dur)

      calcdur (Value eid,Value ename,Value day,Value start,Value end) =
          ((eid,ename,day),timeOfDayToTime end - timeOfDayToTime start)




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
