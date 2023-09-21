{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Handler.Services
  ( getServicesR
  , getServiceThumbnailR
  , getServiceR
  , postServiceR
  , getServicesSearchR
  , getServiceInfoR
  , getOfferR
  ) where

import Control.Monad (forM, join)
import Data.Bifunctor (Bifunctor(second, bimap))
import qualified Data.List.Safe as LS (head)
import Data.Maybe (catMaybes, fromMaybe)
import Data.FileEmbed (embedFile)
import Data.Text (pack, unpack, Text)
import Data.Text.Encoding (encodeUtf8)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages, typeSvg, preEscapedToMarkup
    , TypedContent (TypedContent), ToContent (toContent)
    , whamlet, getRequest, YesodRequest (reqGetParams)
    , setSession, redirect
    )
import Yesod.Core.Handler (newIdent)
import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Form.Input (iopt, runInputGet)
import Yesod.Form.Fields
    ( textField, intField, searchField, unTextarea
    , Textarea (Textarea), FormMessage (MsgInvalidEntry)
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq, parseHelper)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), Enctype (UrlEncoded), FieldView (fvInput)
    , Field (Field, fieldParse, fieldView, fieldEnctype)
    )
import Settings (widgetFile)

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, AccountPhotoR, PhotoPlaceholderR, ServicesR
      , ServiceR, ServiceThumbnailR, ServicesSearchR, StaticR
      , ProfileR, BookStaffR, ServiceInfoR, OfferR
      )
    , AppMessage
      ( MsgServices, MsgPhoto, MsgThumbnail, MsgNoServicesYet
      , MsgBookAppointment, MsgOffers, MsgSearch, MsgSelect, MsgCancel
      , MsgCategories, MsgNoServicesFound, MsgCategories
      , MsgCategory, MsgService, MsgDescription, MsgOffer
      )
    )

import Yesod.Persist.Core (runDB)
import Database.Persist(Entity (Entity))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, orderBy, asc, exists
    , (^.), (?.), (==.), (%), (++.), (||.), (:&) ((:&))
    , where_, val, select, isNothing, just, justList
    , valList, in_, upper_, like, innerJoin, on, leftJoin
    , Value (unValue), subSelectList, withRecursive, unionAll_
    , subSelectCount, subSelectMaybe
    )

import Model
    ( Service(Service), ServiceId
    , Thumbnail (Thumbnail), Offer (Offer), OfferId
    , Services (Services)
    , EntityField
      ( ServiceId, ThumbnailService, ServiceGroup, OfferService
      , OfferId, ServicePublished, ServiceName, ServiceDescr
      , ServiceOverview, ThumbnailAttribution
      )
    )

import Settings.StaticFiles (img_photo_FILL0_wght400_GRAD0_opsz48_svg)

import Menu (menu)
import Handler.Book (sessKeyBooking)


getServiceInfoR :: Services -> Handler Html
getServiceInfoR (Services sids) = do
    open <- (Just <$>) . filter ((== "o") . fst) . reqGetParams <$> getRequest
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    let sid = last sids
    service <- (second (join . unValue) <$>) <$> runDB ( selectOne $ do
        x :& t <- from $ table @Service `leftJoin` table @Thumbnail
            `on` (\(x :& t) -> just (x ^. ServiceId) ==. t ?. ThumbnailService)
        where_ $ x ^. ServicePublished
        where_ $ x ^. ServiceId ==. val sid
        return (x,t ?. ThumbnailAttribution) )
    offers <- case service of
      Nothing -> return []
      Just (Entity serid _,_) -> (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& s :& t <- from $ table @Offer
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail  `on` (\(_ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
        where_ $ x ^. OfferService `in_` subSelectList
              ( do
                    cte <- withRecursive
                        ( do
                              p <- from $ table @Service
                              where_ $ p ^. ServiceId ==. val serid
                              where_ $ p ^. ServicePublished
                              return p
                        )
                        unionAll_
                        (\parent -> do
                          (c :& _) <- from $ table @Service `innerJoin` parent
                              `on` (\(c :& p) -> c ^. ServiceGroup ==. just (p ^. ServiceId))
                          where_ $ c ^. ServicePublished
                          return c
                        )
                    ( ^. ServiceId) <$> from cte
              )
        orderBy [asc (s ^. ServiceId), asc (x ^. OfferId)]
        return ((x,s), t ?. ThumbnailAttribution) )
    defaultLayout $ do
        $(widgetFile "services/info")


getServicesSearchR :: Handler Html
getServicesSearchR = do
    formSearch <- newIdent
    dlgCategList <- newIdent
    mq <- runInputGet $ iopt (searchField True) "q"
    categs <- (toSqlKey . read . unpack . snd <$>) . filter ((== "categ") . fst) . reqGetParams <$> getRequest
    services <- (bimap (second unValue) unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Service
        let n :: SqlExpr (Value Int)
            n = subSelectCount $ from (table @Offer) >>= \o -> where_ $ o ^. OfferService ==. x ^. ServiceId
        let a :: SqlExpr (Value (Maybe Html))
            a = subSelectMaybe $ do
                t <- from $ table @Thumbnail
                where_ $ t ^. ThumbnailService ==. x ^. ServiceId
                return $ t ^. ThumbnailAttribution
        where_ $ x ^. ServicePublished
        case mq of
          Just q -> where_ $ ( upper_ (x ^. ServiceName) `like` (%) ++. upper_ (val q) ++. (%) )
              ||. ( upper_ (x ^. ServiceOverview) `like` (%) ++. upper_ (just (val q)) ++. (%) )
              ||. ( upper_ (x ^. ServiceDescr) `like` (%) ++. upper_ (just (val (Textarea q))) ++. (%) )
          _ -> return ()
        case categs of
          [] -> return ()
          xs -> where_ $ x ^. ServiceGroup `in_` justList (valList xs)
        orderBy [asc (x ^. ServiceName)]

        return ((x,n),a) )
        
    groups <- runDB $ select $ do
        x <- from $ table @Service
        where_ $ exists $ do
            y <- from $ table @Service
            where_ $ y ^. ServiceGroup ==. just (x ^. ServiceId)
        return x
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "services/search")


postServiceR :: Services -> Handler Html
postServiceR (Services sids) = do
    open <- (Just <$>) . filter ((== "o") . fst) . reqGetParams <$> getRequest
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    let sid = last sids
    offers <- ((\((a,b),c) -> (a,b,c)) . second (join . unValue) <$>) <$> runDB ( select $ do
        x :& s :& t <- from $ table @Offer `innerJoin` table @Service
            `on` (\(x :& s) -> x ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
        where_ $ x ^. OfferService ==. val sid
        return ((s,x),t ?. ThumbnailAttribution) )

    ((fr,fw),et) <- runFormPost $ formOffer offers
    case fr of
      FormSuccess (_,Entity oid _,_) -> do
          setSession sessKeyBooking "BOOKING_START"
          redirect (BookStaffR, [("oid",pack $ show $ fromSqlKey oid)])
      _ -> defaultLayout $ do
          setTitleI MsgOffers
          $(widgetFile "services/service")


getOfferR :: OfferId -> Services -> Handler Html
getOfferR oid (Services sids) = do
    open <- (Just <$>) . filter ((== "o") . fst) . reqGetParams <$> getRequest
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    let sid = last sids
    offers <- ((\((a,b),c) -> (a,b,c)) . second (join . unValue) <$>) <$> runDB ( select $ do
        x :& s :& t <- from $ table @Offer `innerJoin` table @Service
            `on` (\(x :& s) -> x ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
        where_ $ x ^. OfferId ==. val oid
        where_ $ s ^. ServicePublished
        return ((s,x),t ?. ThumbnailAttribution) )

    (fw,et) <- generateFormPost $ formOffer offers

    defaultLayout $ do
        setTitleI MsgOffers
        $(widgetFile "services/offer")


getServiceR :: Services -> Handler Html
getServiceR (Services sids) = do
    open <- (Just <$>) . filter ((== "o") . fst) . reqGetParams <$> getRequest
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    let sid = last sids

    offers <- ((\((a,b),c) -> (a,b,c)) . second (join . unValue) <$>) <$> runDB ( select $ do
        x :& s :& t <- from $ table @Offer `innerJoin` table @Service
            `on` (\(x :& s) -> x ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
        where_ $ x ^. OfferService ==. val sid
        where_ $ s ^. ServicePublished
        return ((s,x),t ?. ThumbnailAttribution) )

    (fw,et) <- generateFormPost $ formOffer offers

    defaultLayout $ do
        setTitleI MsgOffers
        $(widgetFile "services/service")


formOffer :: [(Entity Service,Entity Offer, Maybe Html)]
          -> Html -> MForm Handler (FormResult (Entity Service,Entity Offer,Maybe Html),Widget)
formOffer offers extra = do
    (r,v) <- mreq (optionsField offers) "" Nothing
    return ( r
           , [whamlet|
#{extra}
^{fvInput v}
|]
           )
  where

      optionsField :: [(Entity Service,Entity Offer,Maybe Html)] -> Field Handler (Entity Service, Entity Offer,Maybe Html)
      optionsField options = Field
          { fieldParse = parseHelper $ \s -> do
                case (toSqlKey <$>) . readMaybe . unpack $ s of
                  Just oid -> case LS.head (filter (\(_,Entity oid' _,_) -> oid == oid') options) of
                                Nothing -> Left (MsgInvalidEntry s)
                                Just x -> Right x
                  Nothing -> Left (MsgInvalidEntry s)
          , fieldView = \theId name _attrs _eval _isReq -> do
                $(widgetFile "services/offers")
          , fieldEnctype = UrlEncoded
          }


getServicesR :: Handler Html
getServicesR = do
    open <- (snd <$>) . filter ((== "o") . fst) . reqGetParams <$> getRequest
    scrollY <- runInputGet (iopt textField "scrollY")
    msid <- (toSqlKey <$>) <$> runInputGet (iopt intField "sid")
    srvs <- fetchServices Nothing
    let Srvs offer = srvs
    user <- maybeAuth
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgServices
        $(widgetFile "services/services")


buildSnippet :: [Text] -> Maybe ServiceId -> Services -> Srvs -> Widget
buildSnippet open msid (Services sids) (Srvs services) = [whamlet|
<nav.mdc-list data-mdc-auto-init=MDCList>
  $forall (((Entity sid (Service sname _ overview _ _ _), attrib), offers),srvs@(Srvs subservices)) <- services
    $with (gid,lof) <- (pack $ show $ fromSqlKey sid, length offers)
      $if (length subservices) > 0
        <details #details#{gid} :elem gid open:open
          ontoggle="document.getElementById('iconExpand#{gid}').textContent = this.open ? 'expand_less' : 'expand_more'">
          <summary.mdc-list-item.mdc-list-item--with-one-line
            .mdc-list-item--with-leading-image.mdc-list-item--with-trailing-icon>
            <span.mdc-list-item__ripple>
            <span.mdc-list-item__start>
              <img src=@{ServiceThumbnailR sid} height=56 width=56 alt=_{MsgThumbnail} loading=lazy>
            <span.mdc-list-item__content>
              <div.mdc-list-item__primary-text>
              <div.mdc-list-item__primary-text>#{sname}
            <span.mdc-list-item__end>
              <i.material-symbols-outlined #iconExpand#{gid}>expand_more
            $maybe attribution <- attrib
              <div style="position:absolute;bottom:0;left:4px;font-size:0.5rem;line-height:1;white-space:nowrap">
                ^{attribution}
          $maybe overview <- overview
            <a.mdc-list-item href=@?{(ServiceInfoR (Services (sids ++ [sid])),oqs (sids ++ [sid]))}
              .mdc-list-item--with-one-line.mdc-list-item--with-trailing-icon>
              <span.mdc-list-item__ripple>
              <span.mdc-list-item__content>
                <div.mdc-list-item__secondary-text>
                  <i>#{overview}
              <span.mdc-list-item__end>
                <i.material-symbols-outlined #iconExpand#{gid}>arrow_forward_ios
          ^{buildSnippet open msid (Services (sids ++ [sid])) srvs}
      $else
        <div.mdc-list-divider role=separator>
        <a.mdc-list-item.mdc-list-item--with-one-line
          :lof /= 1:href=@?{(ServiceInfoR (Services (sids ++ [sid])),oqs sids)}
          :lof == 1:href=@?{(ServiceR (Services (sids ++ [sid])),oqs sids)}
          :msid == Just sid:.mdc-list-item--activated
          .mdc-list-item--with-leading-image.mdc-list-item--with-trailing-icon>
          <span.mdc-list-item__ripple>
          <span.mdc-list-item__start>
            <img src=@{ServiceThumbnailR sid} height=56 width=56 alt=_{MsgThumbnail} loading=lazy>
          <span.mdc-list-item__content>
            <div.mdc-list-item__primary-text>
              #{sname}
            $if lof == 1
              $forall Entity _ (Offer _ oname price prefix suffix _) <- take 1 offers
                <div.mdc-list-item__secondary-text style="white-space:nowrap;overflow:hidden;text-overflow:ellipsis">
                  #{oname}:&nbsp;
                  $maybe prefix <- prefix
                    #{prefix}
                  $with price <- show price
                    <span.numeric-format data-value=#{price} data-minFracDigits=0 data-maxFracDigits=2>
                      #{price}
                  $maybe suffix <- suffix
                    #{suffix}
          <span.mdc-list-item__end>
            <i.material-symbols-outlined>arrow_forward_ios
        $maybe attribution <- attrib
          <div style="position:relative">
            <div style="position:absolute;bottom:0;left:4px;font-size:0.5rem;line-height:1">
              ^{attribution}
|]
    where
      oqs :: [ServiceId] -> [(Text,Text)]
      oqs = (<$>) (("o",) . pack . show . fromSqlKey)

newtype Srvs = Srvs [(((Entity Service, Maybe Html), [Entity Offer]), Srvs)]


fetchServices :: Maybe ServiceId -> Handler Srvs
fetchServices gid = do
    categories <- (second (join . unValue) <$>) <$> runDB ( select $ do
        x :& t <- from $ table @Service `leftJoin` table @Thumbnail
            `on` (\(x :& t) -> just (x ^. ServiceId) ==. t ?. ThumbnailService)
        where_ $ x ^. ServicePublished ==. val True
        where_ $ case gid of
          Nothing -> isNothing $ x ^. ServiceGroup
          Just sid -> x ^. ServiceGroup ==. just (val sid)
        orderBy [asc (x ^. ServiceId)]
        return (x, t ?. ThumbnailAttribution) )

    groups <- forM categories $ \e@(Entity sid _,_) -> (e,) <$> runDB ( select $ do
        x <- from $ table @Offer
        where_ $ x ^. OfferService ==. val sid
        orderBy [asc (x ^. OfferId)]
        return x )

    Srvs <$> forM groups ( \g@((Entity sid _,_),_) -> (g,) <$> fetchServices (Just sid) )


getServiceThumbnailR :: ServiceId -> Handler TypedContent
getServiceThumbnailR sid = do
    img <- runDB $ selectOne $ do
       x <- from $ table @Thumbnail
       where_ $ x ^. ThumbnailService ==. val sid
       return x
    return $ case img of
      Just (Entity _ (Thumbnail _ photo mime _)) -> TypedContent (encodeUtf8 mime) $ toContent photo
      Nothing -> TypedContent typeSvg $ toContent $(embedFile "static/img/photo_FILL0_wght400_GRAD0_opsz48.svg")
