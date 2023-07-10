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
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , getMessages, whamlet, SomeMessage (SomeMessage), redirect
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
      , ServiceCreateFormR, ImagePlaceholderR, ServiceThumbnailR
      , ServiceR
      )
    , AppMessage
      ( MsgServices, MsgAdd, MsgPhoto, MsgService, MsgTheName
      , MsgDescription, MsgPrice, MsgCancel, MsgSave, MsgThumbnail, MsgRecordAdded
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
    , where_, val, update, set
    )
    
import Model
    ( Service(Service, serviceName, serviceDescr, servicePrice), ServiceId
    , EntityField (ServiceId, ThumbnailService, ThumbnailPhoto, ThumbnailMime)
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime)
    )
import Data.Text.Encoding (encodeUtf8)


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
          addMessageI "info" MsgRecordAdded
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
    services <- runDB $ selectOne $ do
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
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (serviceDescr . entityVal <$> service)
    (priceR,priceV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (realToFrac . servicePrice . entityVal <$> service)
    (thumbnailR,thumbnailV) <- mopt fileField  FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r = (,) <$> (Service <$> nameR <*> descrR <*> (realToFrac <$> priceR)) <*> thumbnailR
    let w = [whamlet|
#{extra}
<div.form-field>
  <label for=#{fvId thumbnailV}>
    $maybe Entity sid _ <- service
      <img.thumbnail src=@{ServiceThumbnailR sid} onerror="this.src = '@{ImagePlaceholderR}'" alt=_{MsgPhoto}>
    $nothing
      <i.material-symbols-outlined style="font-size:48px">add_photo_alternate
    _{MsgThumbnail}

  ^{fvInput thumbnailV}
$forall v <- [nameV,descrV,priceV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      <span.mdc-line-ripple>
    $maybe errs <- fvErrors v 
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}
|]
    return (r,w)

