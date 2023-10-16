{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoDataRU (populateRU) where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet, hamlet)
import Text.Shakespeare.Text (st)
import qualified Data.ByteString.Base64 as B64 (decode)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay,utctDayTime), DiffTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.LocalTime (timeToTimeOfDay, utc, TimeZone (TimeZone))
import Control.Monad.IO.Class (MonadIO (liftIO))
import ClassyPrelude.Yesod (ReaderT)
import Yesod.Form.Fields (Textarea (Textarea))
import Yesod.Auth.Util.PasswordStore (makePassword)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert_, insert) )

import Model
    ( User (User, userName, userPassword, userAdmin, userEmail, userFullName)
    , UserPhoto (UserPhoto, userPhotoUser, userPhotoPhoto, userPhotoMime)
    , Service
      ( Service, serviceName, serviceDescr, serviceGroup, serviceOverview
      , servicePublished, serviceDuration
      )
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime, thumbnailAttribution)
    , Offer
      ( Offer, offerName, offerPrice, offerPrefix
      , offerSuffix, offerDescr, offerService
      )
    , EmplStatus (EmplStatusEmployed, EmplStatusDismissed)
    , Staff (Staff, staffName, staffStatus, staffPhone, staffMobile, staffEmail, staffUser)
    , StaffPhoto (StaffPhoto, staffPhotoPhoto, staffPhotoMime, staffPhotoStaff)
    , Role (Role, roleStaff, roleService, roleName, roleRating)
    , Contents (Contents, contentsSection, contentsContent)
    , BookStatus (BookStatusRequest)
    , Book
      ( Book, bookCustomer, bookOffer, bookRole, bookDay, bookTime, bookTzo
      , bookTz, bookStatus, bookAddr
      )
    , Business
      ( Business, businessName, businessAddr, businessTzo, businessTz
      , businessPhone, businessMobile, businessEmail
      )
    , Hist
      ( Hist, histBook, histLogtime, histDay, histTime, histAddr, histTzo
      , histStatus, histUser, histTz, histRoleName, histStaffName
      )
    )
import Data.FileEmbed (embedFile)
import Demo.DemoPhotos
    ( man01, man02, man03, man04, man05, man06
    , woman01, woman02, woman03, woman04, woman05
    )


populateRU :: MonadIO m => ReaderT SqlBackend m ()
populateRU = do
    
    (now,today,time) <- liftIO $ getCurrentTime >>= \x -> return (x ,utctDay x,timeToTimeOfDay (utctDayTime x))

    let business = Business { businessName = "Салон"
                            , businessAddr = "Россия, г. Москва, проезд Воскресенские Ворота, 1А, Москва, 109012"
                            , businessTzo = TimeZone 180 False "MSK"
                            , businessTz = "Europe/Moscow"
                            , businessPhone = Just "+7 (958) 759-52-25"
                            , businessMobile = Just "940(8537)418-74-67"
                            , businessEmail = Just "salon@mail.ru"
                            }

    insert_ business

    insert_ $ Contents { contentsSection = "CONTACTS"
                       , contentsContent = Textarea $ toStrict $ renderHtml [shamlet|
<section style="margin:0 1rem">
  <h3 style="color:gray">Звоните нам
  <dl>
    <dt>
      <i>телефон
    <dd>
      $maybe phone <- businessPhone business
        #{phone}
    <dt>
      <i>мобильный
    <dd>
      $maybe mobile <- businessMobile business
        #{mobile}
<section style="margin:0 1rem">
  <h3 style="color:gray">Напишите нам
  <dl>
    <dt>
      <i>Электронная почта
    <dd>
      $maybe email <- businessEmail business
        #{email}
<section style="margin:0 1rem">
  <h3 style="color:gray">Приходите к нам
  <dl>
    <dt>
      <i>Адрес
    <dd>
      #{businessAddr business}
<p>
  <iframe width="100%" height="400px" loding="lazy" title="Salon" style="border:none" src="https://api.mapbox.com/styles/v1/mapbox/streets-v12.html?title=false&zoomwheel=false&access_token=pk.eyJ1IjoiY2l1a3N0YXIiLCJhIjoiY2o1enNibDNsMGNrNDJ3dDhxeTJuc3luMiJ9.Jgc5GdYUMbYwGq-zRWtzfw#15/55.751244/37.618423">
|]
                       }

    
    insert_ $ Contents { contentsSection = "ABOUT_US"
                       , contentsContent = Textarea [st|
<h2 style="color:gray">Наша миссия</h2>
<p>
Миссия <i>Салона</i> проста: предложить каждому человеку, входящему в его двери, особую среду, где он может ухаживать и баловать себя, одновременно улучшая свой личный имидж и привнося ощущение благополучия в свою жизнь.
</p>
<h2 style="color:gray">Наш этос</h2>
<p>
Любой человек, пришедший в наш салон, уникален. Мы относимся к каждому уникальному клиенту с учетом его личных потребностей. Мы гордимся тем, что предлагаем услуги, которые ожидает клиент, и лечение, которое ему требуется, и мы будем постоянно переоценивать их требования в зависимости от их образа жизни и тела. Мы никогда не будем предлагать ненужные процедуры и ставим каждого клиента в приоритет. Именно поэтому мы можем с гордостью сказать, что за эти годы у нас накопилась лояльная клиентская база. Мы учимся у них, заботясь о них, и стараемся идти в ногу с последними тенденциями и доступными методами лечения, чтобы всегда удовлетворять потребности наших уважаемых клиентов и любых будущих посетителей.
</p>
<h2 style="color:gray">Наши цели</h2>
<p>
Мы продолжим предлагать новейшие методы лечения, самые инновационные методы, используя лучшие продукты на рынке. И все это в элегантной, чистой и гостеприимной обстановке под руководством обученных, профессиональных и дружелюбных терапевтов. Мы постараемся донести нашу идею о том, что каждый имеет право чувствовать себя хорошо!
</p>
|]
                       }
                             
    pass <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 pass
                   , userAdmin = True
                   , userFullName = Just "Иванов Иван Иванович"
                   , userEmail = Just "iiivanov@mail.ru"
                   }
    return ()
