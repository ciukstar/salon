{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Admin.Services
  ( getAdmServicesR
  , getAdmServiceCreateFormR
  , postAdmServiceR
  , getAdmServiceEditFormR
  , postAdmServicesR
  , postAdmServiceDeleteR
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Maybe as M (isNothing)
import qualified Data.List.Safe as LS (last)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , FileInfo (fileContentType), SomeMessage (SomeMessage)
    , addMessageI, fileSourceByteString, redirect, getUrlRender
    , lookupSession, getMessages
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg
    , img_photo_FILL0_wght400_GRAD0_opsz48_svg
    , img_spark_svg
    )

import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Foundation
    ( Handler, Widget
    , Route (StaticR, AuthR, PhotoPlaceholderR, AdminR, AccountPhotoR, ServiceThumbnailR)
    , AdminR
      ( AdmServiceCreateFormR, AdmServiceEditFormR, AdmServicesR, AdmServiceR
      , AdmServiceDeleteR
      )
    , AppMessage
      ( MsgServices, MsgPhoto, MsgLogout, MsgTheName
      , MsgPrice, MsgDescription, MsgRecordEdited
      , MsgService, MsgSave, MsgCancel, MsgRecordAdded, MsgImage
      , MsgSubservices, MsgAddService, MsgAddSubservice, MsgNoServicesYet
      , MsgDeleteAreYouSure, MsgYesDelete, MsgPleaseConfirm, MsgRecordDeleted
      , MsgPrefix, MsgSuffix
      )
    )

import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (replace, insert, insert_)
    )
import Model
    ( ultDestKey, ServiceId
    , Service
      ( Service, serviceName, servicePrice, serviceDescr, serviceGroup
      , servicePricePrefix, servicePriceSuffix
      )
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime)
    , Services (Services)
    , EntityField (ServiceId, ThumbnailPhoto, ThumbnailMime, ThumbnailService, ServiceGroup)
    )
import Yesod.Form
    ( FormResult(FormSuccess), runFormPost, MForm
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , mreq, textField, doubleField, mopt, textareaField, fileField, generateFormPost
    , FieldView (fvId, fvErrors, fvInput, fvLabel), hiddenField
    )

import Yesod.Persist (YesodPersist(runDB), (=.), PersistUniqueWrite (upsert))
import Database.Persist.Sql (delete)
import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, where_, val, update, set
    , (^.), (==.), (:&)((:&))
    , isNothing, select, orderBy, asc, selectQuery, just
    , on, groupBy, countRows, Value (unValue), leftJoin, fromSqlKey, toSqlKey
    )


postAdmServiceDeleteR :: Services -> Handler Html
postAdmServiceDeleteR (Services sids) = do
    runDB $ delete (last sids)
    addMessageI "info" MsgRecordDeleted
    redirect $ AdminR $ AdmServicesR (Services (init sids))


postAdmServiceR :: Services -> Handler Html
postAdmServiceR (Services sids) = do
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val (last sids)
        return x    
    ((fr,widget),enctype) <- runFormPost $ formService service Nothing
    rndr <- getUrlRender
    ult <- fromMaybe (rndr $ AdminR $ AdmServicesR (Services [])) <$> lookupSession ultDestKey
    case fr of
      FormSuccess (s,mfi) -> do
          _ <- runDB $ replace (last sids) s
          addMessageI "info" MsgRecordEdited
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                _ <- runDB $ upsert
                     (Thumbnail (last sids) bs (fileContentType fi))
                     [ThumbnailPhoto =. bs, ThumbnailMime =. fileContentType fi]
                redirect $ AdminR $ AdmServicesR (Services sids)
            Nothing -> redirect $ AdminR $ AdmServicesR (Services sids)
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "admin/services/edit")


getAdmServiceEditFormR :: Services -> Handler Html
getAdmServiceEditFormR (Services sids) = do
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val (last sids)
        return x
    (widget,enctype) <- generateFormPost $ formService service Nothing
    rndr <- getUrlRender
    ult <- fromMaybe (rndr $ AdminR $ AdmServicesR (Services sids)) <$> lookupSession ultDestKey
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/edit")

    
getAdmServiceCreateFormR :: Services -> Handler Html
getAdmServiceCreateFormR (Services sids) = do
    (widget,enctype) <- generateFormPost $ formService Nothing (LS.last sids)
    rndr <- getUrlRender
    ult <- fromMaybe (rndr $ AdminR $ AdmServicesR (Services [])) <$> lookupSession ultDestKey
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/create")


postAdmServicesR :: Services -> Handler Html
postAdmServicesR (Services sids) = do
    ((fr,widget),enctype) <- runFormPost $ formService Nothing (LS.last sids)
    rndr <- getUrlRender
    ult <- fromMaybe (rndr $ AdminR $ AdmServicesR (Services [])) <$> lookupSession ultDestKey
    case fr of
      FormSuccess (s,mfi) -> do
          sid <- runDB $ insert s
          addMessageI "info" MsgRecordAdded
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                runDB $ insert_ Thumbnail { thumbnailService = sid
                                          , thumbnailPhoto = bs
                                          , thumbnailMime = fileContentType fi
                                          }
            Nothing -> return ()
          redirect ult
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "admin/services/create")


getAdmServicesR :: Services -> Handler Html
getAdmServicesR (Services sids) = do
    muid <- maybeAuth
    service <- case sids of
      [] -> return Nothing
      (last -> sid) -> runDB $ selectOne $ do
          x <- from $ table @Service
          where_ $ x ^. ServiceId ==. val sid
          return x
    services <- runDB $ select $ do
        x <- from $ table @Service
        case sids of
          [] -> where_ $ isNothing $ x ^. ServiceGroup
          (last -> y) -> where_ $ x ^. ServiceGroup ==. just (val y)
        orderBy [asc (x ^. ServiceId)]
        return x
    setUltDestCurrent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgServices
        $(widgetFile "admin/services/services")


subservices :: [Entity Service] -> [ServiceId] -> Widget
subservices services sids = $(widgetFile "admin/services/subservices")


formService :: Maybe (Entity Service) -> Maybe ServiceId -> Html
            -> MForm Handler (FormResult (Service, Maybe FileInfo), Widget)
formService service group extra = do
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (serviceName . entityVal <$> service)
    (priceR,priceV) <- mopt doubleField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((realToFrac <$>) . servicePrice . entityVal <$> service)
    (prefR,prefV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPrefix
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (servicePricePrefix . entityVal <$> service)
    (suffR,suffV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgSuffix
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (servicePriceSuffix . entityVal <$> service)
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (serviceDescr . entityVal <$> service)
    (groupR,groupV) <- mopt hiddenField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } ( (fromSqlKey <$>) <$> ((serviceGroup . entityVal <$> service) <|> Just group) )
    (thumbnailR,thumbnailV) <- mopt fileField  FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r = (,)
            <$> ( Service
                  <$> nameR
                  <*> ((realToFrac <$>) <$> priceR)
                  <*> prefR
                  <*> suffR
                  <*> descrR
                  <*> ((toSqlKey <$>) <$> groupR)
                )
            <*> thumbnailR
    let w = $(widgetFile "admin/services/form")
    return (r,w)
