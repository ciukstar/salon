{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Common
  ( getWebAppManifestR
  , getSitemapR
  , getPhotoPlaceholderR
  , getFaviconR
  , getRobotsR
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=), Value (String))
import Data.Conduit (yield)
import Data.FileEmbed (embedFile)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.Month (pattern YearMonth)
import Foundation
    ( Handler
    , Route
      ( HomeR, ServicesR, BookOffersR, BookingsCalendarR, AboutUsR
      , ContactR, ResourcesR, StaticR, FaviconR
      )
    , ResourcesR (DocsR), AppMessage (MsgAppName, MsgMetaDescription)
    )
import Settings.StaticFiles
    (img_salon_512_png, img_salon_512_maskable_png)
import Yesod.Core
    ( TypedContent (TypedContent), ToContent (toContent)
    , typePlain, cacheSeconds, typeSvg
    )
import Yesod.Core.Handler (selectRep, getUrlRender, getMessageRender)
import Yesod.Core.Json (provideJson, array)
import Yesod.Sitemap
    (sitemap, SitemapUrl (SitemapUrl), SitemapChangeFreq (Monthly))


getWebAppManifestR :: Handler TypedContent
getWebAppManifestR = do
    urlRender <- getUrlRender
    msgRender <- getMessageRender
    selectRep $ provideJson $ object
        [ "name" .= msgRender MsgAppName
        , "short_name" .= msgRender MsgAppName
        , "description" .= msgRender MsgMetaDescription
        , "categories" .= array [String "beauty"]
        , "start_url" .= urlRender HomeR
        , "theme_color" .= String "#FFFFFF"
        , "background_color" .= String "#FFFFFF"
        , "display" .= String "standalone"
        , "icons" .= array [ object [ "src" .= urlRender (StaticR img_salon_512_png)
                                    , "type" .= String "image/png"
                                    , "sizes" .= String "512x512"
                                    ]
                           , object [ "src" .= urlRender (StaticR img_salon_512_maskable_png)
                                    , "type" .= String "image/png"
                                    , "sizes" .= String "512x512"
                                    , "purpose" .= String "maskable"
                                    ]
                           ]
        ]


getSitemapR :: Handler TypedContent
getSitemapR = sitemap $ do
    yield $ SitemapUrl FaviconR Nothing (Just Monthly) (Just 0.5)
    yield $ SitemapUrl (ResourcesR DocsR) Nothing (Just Monthly) (Just 1.0)
    yield $ SitemapUrl HomeR Nothing (Just Monthly) (Just 1.0)
    yield $ SitemapUrl ServicesR Nothing (Just Monthly) (Just 0.9)
    yield $ SitemapUrl BookOffersR Nothing (Just Monthly) (Just 0.8)
    
    today <- utctDay <$> liftIO getCurrentTime
    let (y,m,_) = toGregorian today
        month = YearMonth y m
        
    yield $ SitemapUrl (BookingsCalendarR month) Nothing (Just Monthly) (Just 0.7)
    yield $ SitemapUrl AboutUsR Nothing (Just Monthly) (Just 0.6)
    yield $ SitemapUrl ContactR Nothing (Just Monthly) (Just 0.6)


getPhotoPlaceholderR :: Handler TypedContent
getPhotoPlaceholderR = do
    cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
    return $ TypedContent typeSvg
           $ toContent $(embedFile "static/img/person_FILL0_wght400_GRAD0_opsz48.svg")


getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")


getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
