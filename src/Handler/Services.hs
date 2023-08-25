{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Handler.Services
  ( getServicesR
  , getServiceThumbnailR
  , getServiceR
  , getServicesSearchR
  ) where

import Control.Monad (forM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.FileEmbed (embedFile)
import Data.Text (pack, unpack, Text)
import Text.Hamlet (Html)
import Data.Text.Encoding (encodeUtf8)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages, typeSvg, preEscapedToMarkup
    , TypedContent (TypedContent), ToContent (toContent)
    , whamlet, getRequest, YesodRequest (reqGetParams)
    )
import Yesod.Core.Handler (newIdent)
import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Yesod.Form.Input (iopt, runInputGet)
import Yesod.Form.Fields
    ( textField, intField, searchField, unTextarea, Textarea (Textarea))
import Settings (widgetFile)

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, AccountPhotoR, PhotoPlaceholderR, ServicesR
      , ServiceR, ServiceThumbnailR, ServicesSearchR, StaticR
      )
    , AppMessage
      ( MsgServices, MsgPhoto, MsgService, MsgThumbnail
      , MsgNoServicesYet, MsgBookAppointment, MsgLogout
      , MsgSearch, MsgSelect, MsgCancel, MsgCategories
      , MsgNoServicesFound, MsgStatus, MsgCategories
      , MsgCategory, MsgUnpublished, MsgPublished
      )
    )

import Yesod.Persist.Core (runDB)
import Database.Persist
    ( Entity (Entity)
    )
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Esqueleto.Experimental
    (selectOne, from, table, orderBy, asc, exists, not_
    , (^.), (==.), (%), (++.), (||.)
    , where_, val, select, isNothing, just, justList
    , valList, in_, upper_, like
    )
    
import Model
    ( Service(Service), ServiceId
    , EntityField
      ( ServiceId, ThumbnailService, ServiceGroup, OfferService
      , OfferId, ServicePublished, ServiceName, ServiceDescr
      , ServiceOverview
      )
    , Thumbnail (Thumbnail), Offer (Offer), Services (Services)
    , ServiceStatus (ServiceStatusPulished, ServiceStatusUnpublished)
    )

import Settings.StaticFiles (img_photo_FILL0_wght400_GRAD0_opsz48_svg)

getServicesSearchR :: Handler Html
getServicesSearchR = do
    formSearch <- newIdent
    dlgCategList <- newIdent
    dlgStatusList <- newIdent
    mq <- runInputGet $ iopt (searchField True) "q"
    categs <- (toSqlKey . read . unpack . snd <$>) . filter ((== "categ") . fst) . reqGetParams <$> getRequest
    stati <- (read . unpack . snd <$>) . filter ((== "status") . fst) . reqGetParams <$> getRequest
    services <- runDB $ select $ do
        x <- from $ table @Service 
        case mq of
          Just q -> where_ $ ( upper_ (x ^. ServiceName) `like` (%) ++. upper_ (val q) ++. (%) )
              ||. ( upper_ (x ^. ServiceOverview) `like` (%) ++. upper_ (just (val q)) ++. (%) )
              ||. ( upper_ (x ^. ServiceDescr) `like` (%) ++. upper_ (just (val (Textarea q))) ++. (%) )
          _ -> return ()
        case categs of
          [] -> return ()
          xs -> where_ $ x ^. ServiceGroup `in_` justList (valList xs)
        case stati of
          [ServiceStatusPulished] -> where_ $ x ^. ServicePublished
          [ServiceStatusUnpublished] -> where_ $ not_ $ x ^. ServicePublished
          _ -> return ()
        orderBy [asc (x ^. ServiceName)]
        return x
    groups <- runDB $ select $ do
        x <- from $ table @Service
        where_ $ exists $ do
            y <- from $ table @Service
            where_ $ y ^. ServiceGroup ==. just (x ^. ServiceId)
        return x
    let statusList = [(ServiceStatusPulished,MsgPublished),(ServiceStatusUnpublished,MsgUnpublished)]
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "services/search")


getServiceR :: Services -> Handler Html
getServiceR (Services sids) = do
    open <- (Just <$>) . filter ((== "o") . fst) . reqGetParams <$> getRequest
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    let sid = last sids
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x

    offer <- runDB $ select $ do
        x <- from $ table @Offer
        where_ $ x ^. OfferService ==. val sid
        orderBy [asc (x ^. OfferId)]
        return x
    
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "services/service")


getServicesR :: Handler Html
getServicesR = do
    open <- (snd <$>) . filter ((== "o") . fst) . reqGetParams <$> getRequest
    scrollY <- runInputGet (iopt textField "scrollY")
    msid <- (toSqlKey <$>) <$> runInputGet (iopt intField "sid")
    srvs <- fetchServices Nothing
    let Srvs offer = srvs
    muid <- maybeAuth
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgServices
        $(widgetFile "services/services")


buildSnippet :: [Text] -> Maybe ServiceId -> Services -> Srvs -> Widget
buildSnippet open msid (Services sids) (Srvs services) = [whamlet|
<ul.mdc-list data-mdc-auto-init=MDCList>
  $forall ((Entity sid (Service name _ overview _ _), offer),srvs@(Srvs subservices)) <- services
    $with (gid,l) <- (pack $ show $ fromSqlKey sid, length offer)
      $if (length subservices) > 0
        <details role=listitem #details#{gid} :elem gid open:open
          ontoggle="document.getElementById('iconExpand#{gid}').textContent = this.open ? 'expand_less' : 'expand_more'">
          <summary.mdc-list-item
            .mdc-list-item--with-one-line
            .mdc-list-item--with-leading-image
            .mdc-list-item--with-trailing-icon>
            <span.mdc-list-item__ripple>
            <span.mdc-list-item__start>
              <img src=@{ServiceThumbnailR sid} height=56 width=56 alt=_{MsgThumbnail} loading=lazy>
            <span.mdc-list-item__content>
              <div.mdc-list-item__primary-text>#{name}
            <span.mdc-list-item__end>
              <i.material-symbols-outlined #iconExpand#{gid}>expand_more
          $maybe overview <- overview
            <a.mdc-list-item href=@?{(ServiceR (Services (sids ++ [sid])),oqs (sids ++ [sid]))}
              .mdc-list-item--with-one-line
              .mdc-list-item--with-trailing-icon>
              <span.mdc-list-item__ripple>
              <span.mdc-list-item__content>
                <div.mdc-list-item__secondary-text>
                  <i>#{overview}
              <span.mdc-list-item__end>
                <i.material-symbols-outlined #iconExpand#{gid}>info
          ^{buildSnippet open msid (Services (sids ++ [sid])) srvs}
      $else
        <a.mdc-list-item href=@?{(ServiceR (Services (sids ++ [sid])),oqs sids)}
          :l == 0:.mdc-list-item--with-one-line
          :l == 1:.mdc-list-item--with-two-lines
          :l >= 2:.mdc-list-item--with-three-lines
          :msid == Just sid:.mdc-list-item--activated
          .mdc-list-item--with-leading-image.mdc-list-item--with-trailing-icon>
          <span.mdc-list-item__ripple>
          <span.mdc-list-item__start>
            <img src=@{ServiceThumbnailR sid} height=56 width=56 alt=_{MsgThumbnail} loading=lazy>
          <span.mdc-list-item__content>
            <div.mdc-list-item__primary-text>
              #{name}
            $forall Entity _ (Offer _ name price prefix suffix _) <- take 2 offer
              <div.mdc-list-item__secondary-text>
                #{name}:&nbsp;
                $maybe prefix <- prefix
                  #{prefix}
                $with price <- show price
                  <span.numeric-format data-value=#{price} data-minFracDigits=0 data-maxFracDigits=2>
                    #{price}
                $maybe suffix <- suffix
                  #{suffix}
          <span.mdc-list-item__end>
            <i.material-symbols-outlined>arrow_forward_ios
|]
    where
      oqs :: [ServiceId] -> [(Text,Text)]
      oqs = (<$>) (("o",) . pack . show . fromSqlKey)

newtype Srvs = Srvs [((Entity Service, [Entity Offer]), Srvs)]


fetchServices :: Maybe ServiceId -> Handler Srvs
fetchServices gid = do
    categories <- runDB $ select $ do
        x <- from $ table @Service
        where_ $ x ^. ServicePublished ==. val True
        where_ $ case gid of
          Nothing -> isNothing $ x ^. ServiceGroup
          Just sid -> x ^. ServiceGroup ==. just (val sid)
        orderBy [asc (x ^. ServiceId)]
        return x

    groups <- forM categories $ \e@(Entity sid _) -> (e,) <$> runDB ( select $ do
        x <- from $ table @Offer
        where_ $ x ^. OfferService ==. val sid
        orderBy [asc (x ^. OfferId)]
        return x )

    Srvs <$> forM groups ( \g@(Entity sid _,_) -> (g,) <$> fetchServices (Just sid) )


getServiceThumbnailR :: ServiceId -> Handler TypedContent
getServiceThumbnailR sid = do
    img <- runDB $ selectOne $ do
       x <- from $ table @Thumbnail
       where_ $ x ^. ThumbnailService ==. val sid
       return x
    return $ case img of
      Just (Entity _ (Thumbnail _ photo mime)) -> TypedContent (encodeUtf8 mime) $ toContent photo
      Nothing -> TypedContent typeSvg $ toContent $(embedFile "static/img/photo_FILL0_wght400_GRAD0_opsz48.svg")

