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
  , getAdmServiceR
  ) where

import Data.Bifunctor (Bifunctor(second))
import Data.Maybe (isJust, fromMaybe)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , FileInfo (fileContentType), SomeMessage (SomeMessage)
    , addMessageI, fileSourceByteString, redirect
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg
    , img_photo_FILL0_wght400_GRAD0_opsz48_svg
    )

import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Foundation
    ( Handler, Widget
    , Route (StaticR, AuthR, PhotoPlaceholderR, AdminR, AccountPhotoR, ServiceThumbnailR)
    , AdminR (AdmServiceCreateFormR, AdmServicesR, AdmServiceR)
    , AppMessage
      ( MsgServices, MsgPhoto, MsgLogout, MsgAdd, MsgTheName
      , MsgPrice, MsgMonetaryUnit, MsgDescription, MsgRecordEdited
      , MsgService, MsgSave, MsgCancel, MsgRecordAdded, MsgImage
      , MsgCategory
      )
    )

import Database.Persist (Entity (Entity, entityVal), PersistStoreWrite (replace, insert, insert_))
import Model
    ( ServiceId, Service (Service, serviceName, servicePrice, serviceMu, serviceDescr)
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime)
    , EntityField (ServiceId, ThumbnailPhoto, ThumbnailMime, ThumbnailService, ServiceGroup), Services (Services)
    )
import Yesod.Form
    ( FormResult(FormSuccess), runFormPost, MForm
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , mreq, textField, doubleField, mopt, textareaField, fileField, generateFormPost
    , FieldView (fvId, fvErrors, fvInput, fvLabel), runInputGet, iopt, intField
    )

import Yesod.Persist (YesodPersist(runDB))
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, where_, val, update, set
    , (^.), (==.), (=.), (:&)((:&))
    , isNothing, select, orderBy, asc, selectQuery, just
    , on, groupBy, countRows, Value (unValue), leftJoin
    )


getAdmServiceR :: ServiceId -> Handler Html
getAdmServiceR sid = do
    service <- (second (fromMaybe 0 . unValue) <$>) <$> runDB ( selectOne $ do
        (x :& (_,n)) <- from $ table @Service
            `leftJoin` selectQuery ( do
                                          y <- from $ table @Service
                                          where_ $ y ^. ServiceGroup ==. just (val sid)
                                          groupBy (y ^. ServiceGroup)
                                          return (y ^. ServiceGroup, countRows :: SqlExpr (Value Int))
                                    ) `on` (\(x :& (yid,_)) -> just (x ^. ServiceId) ==. yid)
        where_ $ x ^. ServiceId ==. val sid
        return (x,n) )
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/service")


postAdmServiceR :: ServiceId -> Handler Html
postAdmServiceR sid = do
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x    
    ((fr,widget),enctype) <- runFormPost $ formService service
    case fr of
      FormSuccess (s,mfi) -> do
          _ <- runDB $ replace sid s
          addMessageI "info" MsgRecordEdited
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                _ <- runDB $ update $ \img -> do
                    set img [ThumbnailPhoto =. val bs, ThumbnailMime =. val (fileContentType fi)]
                    where_ $ img ^. ThumbnailService ==. val sid
                redirect $ AdminR $ AdmServicesR (Services [])
            Nothing -> redirect $ AdminR $ AdmServicesR (Services [])
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "admin/services/edit")


getAdmServiceEditFormR :: ServiceId -> Handler Html
getAdmServiceEditFormR sid = do
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x
    (widget,enctype) <- generateFormPost $ formService service
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/edit")    

    
getAdmServiceCreateFormR :: Handler Html
getAdmServiceCreateFormR = do
    (widget,enctype) <- generateFormPost $ formService Nothing
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/create")


postAdmServicesR :: Services -> Handler Html
postAdmServicesR (Services sids) = do
    ((fr,widget),enctype) <- runFormPost $ formService Nothing
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
          redirect (AdminR $ AdmServicesR (Services []))
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
    defaultLayout $ do
        setTitleI MsgServices
        $(widgetFile "admin/services/services")


formService :: Maybe (Entity Service) -> Html -> MForm Handler (FormResult (Service, Maybe FileInfo), Widget)
formService service extra = do
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
    (muR,muV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgMonetaryUnit
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (serviceMu . entityVal <$> service)
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (serviceDescr . entityVal <$> service)
    (thumbnailR,thumbnailV) <- mopt fileField  FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r = (,)
            <$> ( Service
                  <$> nameR
                  <*> ((realToFrac <$>) <$> priceR)
                  <*> muR
                  <*> descrR
                  <*> FormSuccess Nothing
                )
            <*> thumbnailR
    let w = $(widgetFile "admin/services/form")
    return (r,w)
