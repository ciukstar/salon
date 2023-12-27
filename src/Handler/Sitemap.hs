{-# LANGUAGE PatternSynonyms #-}

module Handler.Sitemap (getSitemapR) where

import Control.Monad.IO.Class (liftIO)
import Foundation
    ( Handler
    , Route
      ( HomeR, ServicesR, BookOffersR, BookingsCalendarR, AboutUsR
      , ContactR, ResourcesR
      )
    , ResourcesR (DocsR)
    )
import Yesod.Core.Types (TypedContent)
import Yesod.Sitemap
    (sitemap, SitemapUrl (SitemapUrl), SitemapChangeFreq (Monthly))
import Data.Conduit (yield)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.Month (pattern YearMonth)


getSitemapR :: Handler TypedContent
getSitemapR = sitemap $ do
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
    
