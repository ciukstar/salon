{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Services
  ( getServicesR
  , getServiceCreateFormR
  , postServicesR
  , getServiceThumbnailR
  , getServiceEditFormR
  , postServiceR
  , getServiceR
  ) where

import Data.Maybe (isJust)
import Text.Hamlet (Html)
import Data.Text.Encoding (encodeUtf8)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages, SomeMessage (SomeMessage), redirect
    , FileInfo (fileContentType), fileSourceByteString
    , TypedContent (TypedContent), notFound, ToContent (toContent), addMessageI
    )
import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput, fvLabel, fvErrors, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form
    ( mreq, textField, mopt, doubleField, textareaField, generateFormPost
    , runFormPost, fileField
    )
import Settings (widgetFile)

import Foundation
    ( Handler
    , Route
      ( AuthR, AccountPhotoR, PhotoPlaceholderR, ServicesR
      , ServiceCreateFormR, ServiceThumbnailR
      , ServiceR, StaticR
      )
    , AppMessage
      ( MsgServices, MsgAdd, MsgPhoto, MsgService, MsgTheName
      , MsgDescription, MsgPrice, MsgCancel, MsgSave, MsgThumbnail, MsgRecordAdded
      , MsgNoServicesYet, MsgImage, MsgRecordEdited, MsgMonetaryUnit
      ), Widget
    )

import Yesod.Persist.Core (runDB)
import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (insert, insert_, replace)
    )

import Database.Esqueleto.Experimental
    (selectOne, from, table, orderBy, asc
    , (^.), (==.), (=.)
    , where_, val, update, set, select
    )
    
import Model
    ( Service(Service, serviceName, serviceDescr, servicePrice, serviceMu), ServiceId
    , EntityField (ServiceId, ThumbnailService, ThumbnailPhoto, ThumbnailMime)
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime)
    )

import Settings.StaticFiles
    ( img_spark_svg
    , img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg
    , img_photo_FILL0_wght400_GRAD0_opsz48_svg
    )


getServiceR :: ServiceId -> Handler Html
getServiceR sid = do
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "services/service")


postServiceR :: ServiceId -> Handler Html
postServiceR sid = do
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
                redirect ServicesR
            Nothing -> redirect ServicesR
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "services/edit")


getServiceEditFormR :: ServiceId -> Handler Html
getServiceEditFormR sid = do
    service <- runDB $ selectOne $ do
        x <- from $ table @Service
        where_ $ x ^. ServiceId ==. val sid
        return x
    (widget,enctype) <- generateFormPost $ formService service
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "services/edit")    

    
getServiceCreateFormR :: Handler Html
getServiceCreateFormR = do
    (widget,enctype) <- generateFormPost $ formService Nothing
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "services/create")


postServicesR :: Handler Html
postServicesR = do
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
          redirect ServicesR
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "services/create")


getServicesR :: Handler Html
getServicesR = do
    services <- runDB $ select $ do
        x <- from $ table @Service
        orderBy [asc (x ^. ServiceId)]
        return x
    muid <- maybeAuth
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgServices
        $(widgetFile "services/services")


getServiceThumbnailR :: ServiceId -> Handler TypedContent
getServiceThumbnailR sid = do
    img <- runDB $ selectOne $ do
       x <- from $ table @Thumbnail
       where_ $ x ^. ThumbnailService ==. val sid
       return x
    case img of
      Just (Entity _ (Thumbnail _ photo mime)) -> return $ TypedContent (encodeUtf8 mime) $ toContent photo
      Nothing -> notFound


formService :: Maybe (Entity Service) -> Html -> MForm Handler (FormResult (Service, Maybe FileInfo), Widget)
formService service extra = do
    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (serviceName . entityVal <$> service)
    (priceR,priceV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (realToFrac . servicePrice . entityVal <$> service)
    (muR,muV) <- mreq textField FieldSettings
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

    let r = (,) <$> (Service <$> nameR <*> (realToFrac <$> priceR) <*> muR <*> descrR) <*> thumbnailR
    let w = $(widgetFile "services/form")
    return (r,w)

