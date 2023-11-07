{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Contacts (getContactR) where

import Text.Hamlet (Html)
import Text.Julius (julius, rawJS)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages)
import Yesod.Core.Widget (setTitleI, toWidget, addScriptRemote, addStylesheetRemote)
import Settings (widgetFile)

import Database.Persist (Entity (Entity))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( selectOne, from, table )

import Foundation
    ( Handler
    , Route (ProfileR, AuthR, PhotoPlaceholderR, AccountPhotoR)
    , AppMessage
      ( MsgContactUs, MsgPhoto, MsgNavigationMenu, MsgUserProfile
      , MsgLogin, MsgNoContentYet
      )
    )
    
import Model (ContactUs (ContactUs), mbat)

import Menu (menu)

getContactR :: Handler Html
getContactR = do
    contacts <- runDB $ selectOne $ from $ table @ContactUs
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgContactUs
        case contacts of
          Just (Entity _ (ContactUs _ _ _ True (Just lng) (Just lat))) -> do
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
          Just (Entity _ (ContactUs _ _ _ True _ _)) -> do
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

