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
  , getAdmServiceImageR
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text, pack)
import Data.Maybe (isJust, fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.FileEmbed (embedFile)
import qualified Data.Maybe as M (isNothing)
import qualified Data.List.Safe as LS (last)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , FileInfo (fileContentType), SomeMessage (SomeMessage)
    , addMessageI, fileSourceByteString, redirect, getMessages
    , TypedContent (TypedContent), ToContent (toContent), typeSvg
    )
import Settings (widgetFile)
import Settings.StaticFiles
    ( img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg
    , img_photo_FILL0_wght400_GRAD0_opsz48_svg
    , img_spark_svg
    )
    
import Yesod.Form
    ( FormResult(FormSuccess), Field, MForm
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvId, fvErrors, fvInput, fvLabel)
    , hiddenField, runInputGet, iopt, intField, checkM
    , mreq, textField, doubleField, mopt, textareaField, fileField
    , generateFormPost, runFormPost
    )

import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Foundation
    ( Handler, Widget
    , Route (StaticR, AuthR, PhotoPlaceholderR, AdminR, AccountPhotoR, ServiceThumbnailR)
    , AdminR
      ( AdmServiceCreateFormR, AdmServiceEditFormR, AdmServicesR, AdmServiceR
      , AdmServiceDeleteR, AdmServiceImageR
      )
    , AppMessage
      ( MsgServices, MsgPhoto, MsgLogout, MsgTheName
      , MsgPrice, MsgDescription, MsgRecordEdited
      , MsgService, MsgSave, MsgCancel, MsgRecordAdded, MsgImage
      , MsgSubservices, MsgAddService, MsgAddSubservice, MsgNoServicesYet
      , MsgDeleteAreYouSure, MsgYesDelete, MsgPleaseConfirm, MsgRecordDeleted
      , MsgPrefix, MsgSuffix, MsgServisAlreadyInTheList
      )
    )

import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (replace, insert, insert_)
    )
import Model
    ( ServiceId
    , Service
      ( Service, serviceName, servicePrice, serviceDescr, serviceGroup
      , servicePricePrefix, servicePriceSuffix
      )
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime)
    , Services (Services)
    , EntityField (ServiceId, ThumbnailPhoto, ThumbnailMime, ServiceGroup, ThumbnailService, ServiceName)
    )

import Yesod.Persist (YesodPersist(runDB), (=.), PersistUniqueWrite (upsert))
import Database.Persist.Sql (fromSqlKey, toSqlKey, delete)
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    , isNothing, select, orderBy, asc, just
    )


getAdmServiceImageR :: ServiceId -> Handler TypedContent
getAdmServiceImageR sid = do
    img <- runDB $ selectOne $ do
        x <- from $ table @Thumbnail
        where_ $ x ^. ThumbnailService ==. val sid
        return x
    return $ case img of
      Just (Entity _ (Thumbnail _ photo mime)) -> TypedContent (encodeUtf8 mime) (toContent photo)
      Nothing -> TypedContent typeSvg $ toContent $(embedFile "static/img/photo_FILL0_wght400_GRAD0_opsz48.svg")


postAdmServiceDeleteR :: Services -> Handler Html
postAdmServiceDeleteR (Services sids) = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    runDB $ delete (last sids)
    addMessageI "info" MsgRecordDeleted
    redirect (AdminR $ AdmServicesR (Services (init sids)),[("scrollY",scrollY)])


postAdmServiceR :: Services -> Handler Html
postAdmServiceR (Services sids) = do
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val (last sids)
        return x    
    ((fr,widget),enctype) <- runFormPost $ formService service Nothing
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
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
                redirect (AdminR $ AdmServicesR (Services sids),[("scrollY",scrollY)])
            Nothing -> redirect ( AdminR $ AdmServicesR (Services sids)
                                , [("sid",pack $ show $ fromSqlKey $ last sids),("scrollY",scrollY)]
                                )
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "admin/services/edit")


getAdmServiceEditFormR :: Services -> Handler Html
getAdmServiceEditFormR (Services sids) = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val (last sids)
        return x
    (widget,enctype) <- generateFormPost $ formService service Nothing
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/edit")

    
getAdmServiceCreateFormR :: Services -> Handler Html
getAdmServiceCreateFormR (Services sids) = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    (widget,enctype) <- generateFormPost $ formService Nothing (LS.last sids)
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/create")


postAdmServicesR :: Services -> Handler Html
postAdmServicesR (Services sids) = do
    ((fr,widget),enctype) <- runFormPost $ formService Nothing (LS.last sids)
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
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
          redirect ( AdminR $ AdmServicesR (Services sids)
                   , [("sid",pack $ show $ fromSqlKey sid),("scrollY",scrollY)]
                   )
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "admin/services/create")


getAdmServicesR :: Services -> Handler Html
getAdmServicesR (Services sids) = do
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    msid <- (toSqlKey <$>) <$> runInputGet (iopt intField "sid")
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


subservices :: [Entity Service] -> [ServiceId] -> Maybe ServiceId -> Widget
subservices services sids msid = $(widgetFile "admin/services/subservices")


formService :: Maybe (Entity Service) -> Maybe ServiceId -> Html
            -> MForm Handler (FormResult (Service, Maybe FileInfo), Widget)
formService service group extra = do
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
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
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Service
              where_ $ x ^. ServiceName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity sid _) -> case service of
              Nothing -> Left MsgServisAlreadyInTheList
              Just (Entity sid' _) | sid == sid' -> Right name
                                   | otherwise -> Left MsgServisAlreadyInTheList
