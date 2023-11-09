{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Contacts (getContactR) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Lazy as M (Map, fromListWith, lookup, toList, fromList)
import Data.Time.Calendar
    ( dayOfWeek, toGregorian, DayPeriod (periodFirstDay, periodLastDay)
    , DayOfWeek (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
    )
import Data.Time.Calendar.Month (pattern YearMonth)
import Data.Time.Clock ( getCurrentTime, utctDay )
import Text.Hamlet (Html)
import Text.Julius (julius, rawJS)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages, newIdent)
import Yesod.Core.Widget (setTitleI, toWidget, addScriptRemote, addStylesheetRemote)
import Settings (widgetFile)

import Foundation
    ( Handler
    , Route (ProfileR, AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage
      ( MsgContactUs, MsgPhoto, MsgNavigationMenu, MsgUserProfile
      , MsgLogin, MsgNoContentYet, MsgMonday, MsgTuesday, MsgWednesday
      , MsgThursday, MsgFriday, MsgSaturday, MsgSunday, MsgNoBusinessHoursFound
      , MsgBusinessHours, MsgAddress, MsgNoBusinessAddressFound
      )
    )

import Database.Persist (Entity (Entity))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, val, where_
    , (^.), (==.)
    , orderBy, asc, between, Value (unValue), max_
    )
    
import Model
    ( ContactUs (ContactUs), BusinessHours (BusinessHours)
    , EntityField (BusinessHoursDayType, BusinessHoursDay, BusinessHoursOpen, BusinessAddr)
    , DayType (Weekday)
    , mbat, Business
    )

import Menu (menu)

getContactR :: Handler Html
getContactR = do
    contacts <- runDB $ selectOne $ from $ table @ContactUs
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    today <- utctDay <$> liftIO getCurrentTime
    htmlContainer <- newIdent

    address <- case contacts of
      Just (Entity _ (ContactUs _ _ True _ _ _ _)) -> do
         (unValue <$>) <$> runDB ( selectOne $ do
                x <- from $ table @Business
                return (x ^. BusinessAddr) )
      _ -> return Nothing
    
    schedule <- case contacts of
      Just (Entity _ (ContactUs _ _ _ True _ _ _)) -> do
        let groupByKey :: (Ord k) => (v -> k) -> [v] -> M.Map k [v]
            groupByKey key = M.fromListWith (++) . fmap (\x -> (key x,[x]))

        M.toList . groupByKey (\(Entity _ (BusinessHours _ day s e _)) -> (dayOfWeek day,(s,e))) <$> do        
            ymd <- (((toGregorian <$>) . unValue) =<<) <$> runDB ( selectOne ( do
                x <- from $ table @BusinessHours
                where_ $ x ^. BusinessHoursDayType ==. val Weekday
                return (max_ (x ^. BusinessHoursDay)) ) )
            case ymd of
              Just (y,m,_) -> runDB $ select $ do
                  x <- from $ table @BusinessHours
                  where_ $ x ^. BusinessHoursDayType ==. val Weekday
                  where_ $ x ^. BusinessHoursDay `between` ( val $ periodFirstDay (YearMonth y m)
                                                           , val $ periodLastDay (YearMonth y m)
                                                           )
                  orderBy [asc (x ^. BusinessHoursDay), asc (x ^. BusinessHoursOpen)]
                  return x
              _ -> return []
      _ -> return []
    defaultLayout $ do
        setTitleI MsgContactUs
        case contacts of
          Just (Entity _ (ContactUs _ _ _ _ True (Just lng) (Just lat))) -> do
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-language/v1.0.0/mapbox-gl-language.js"
              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css"
              toWidget [julius|
const main = document.querySelector('main');
const mapgl = document.createElement('div');
mapgl.style.height = '300px';
mapgl.style.width = '100%';
main.appendChild(mapgl);
const map = new mapboxgl.Map({
  accessToken: #{mbat},
  attributionControl: false,
  container: mapgl,
  style: 'mapbox://styles/mapbox/streets-v11',
  center: [#{rawJS $ show lng}, #{rawJS $ show lat}],
  zoom: 15
});
map.addControl(new MapboxLanguage());
map.addControl(new mapboxgl.NavigationControl());
const loc = new mapboxgl.Marker().setLngLat(
  [#{rawJS $ show lng}, #{rawJS $ show lat}]
).addTo(map);
|]
          Just (Entity _ (ContactUs _ _ _ _ True _ _)) -> do
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"
              addScriptRemote "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-language/v1.0.0/mapbox-gl-language.js"
              addStylesheetRemote "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css"
              toWidget [julius|
const main = document.querySelector('main');
const mapgl = document.createElement('div');
mapgl.style.height = '300px';
mapgl.style.width = '100%';
main.appendChild(mapgl);
const map = new mapboxgl.Map({
  accessToken: #{mbat},
  attributionControl: false,
  container: mapgl,
  style: 'mapbox://styles/mapbox/streets-v11',
  center: [0, 0],
  zoom: 0
});
map.addControl(new MapboxLanguage());
map.addControl(new mapboxgl.NavigationControl());
|]
          _ -> return ()
        $(widgetFile "contacts/contact")

