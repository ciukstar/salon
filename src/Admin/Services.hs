{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Admin.Services
  ( getAdmServicesR
  , getAdmServiceCreateFormR
  , postAdmServiceR
  , getAdmServiceEditFormR
  , postAdmServicesR
  , postAdmServiceDeleteR
  , getAdmServiceImageR
  , getAdmPricelistCreateR
  , postAdmPricelistR
  , getAdmPriceR
  , postAdmPriceR
  , getAdmPriceEditR
  , postAdmPriceDeleteR
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text, pack)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.Text.Encoding (encodeUtf8)
import Data.FileEmbed (embedFile)
import qualified Data.List.Safe as LS (last)
import Text.Hamlet (Html)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , FileInfo (fileContentType), SomeMessage (SomeMessage)
    , addMessageI, fileSourceByteString, redirect, getMessages
    , TypedContent (TypedContent), ToContent (toContent), typeSvg
    , whamlet, preEscapedToMarkup
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
    , generateFormPost, runFormPost, boolField, unTextarea, withRadioField, OptionList (OptionList), optionsPairs
    )

import Yesod.Auth (Route (LoginR, LogoutR), maybeAuth)
import Foundation
    ( Handler, Widget
    , Route (StaticR, AuthR, PhotoPlaceholderR, AdminR, AccountPhotoR, ServiceThumbnailR)
    , AdminR
      ( AdmServiceCreateFormR, AdmServiceEditFormR, AdmServicesR, AdmServiceR
      , AdmServiceDeleteR, AdmServiceImageR, AdmPricelistCreateR, AdmPricelistR
      , AdmPriceR, AdmPriceEditR, AdmPriceDeleteR
      )
    , AppMessage
      ( MsgServices, MsgPhoto, MsgLogout, MsgTheName
      , MsgPrice, MsgDescription, MsgRecordEdited
      , MsgService, MsgSave, MsgCancel, MsgRecordAdded, MsgImage
      , MsgSubservices, MsgAddService, MsgAddSubservice, MsgNoServicesYet
      , MsgDeleteAreYouSure, MsgYesDelete, MsgPleaseConfirm, MsgRecordDeleted
      , MsgPrefix, MsgSuffix, MsgServisAlreadyInTheList, MsgPricelist, MsgAddPrice
      , MsgNoPriceSetYet, MsgPriceAlreadyInTheList, MsgOverview, MsgPublished
      , MsgYes, MsgNo
      )
    )

import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (replace, insert, insert_)
    )
import Model
    ( ServiceId
    , Service
      ( Service, serviceName, serviceDescr, serviceGroup, serviceOverview, servicePublished
      )
    , Thumbnail (Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime)
    , Services (Services)
    , EntityField
      ( ServiceId, ThumbnailPhoto, ThumbnailMime, ServiceGroup, ThumbnailService
      , ServiceName, PricelistService, PricelistId, PricelistName
      )
    , Pricelist
      ( Pricelist, pricelistName, pricelistPrice, pricelistPrefix, pricelistSuffix
      , pricelistDescr
      )
    , PricelistId
    )

import Yesod.Persist (YesodPersist(runDB), (=.), PersistUniqueWrite (upsert))
import Database.Persist.Sql (fromSqlKey, toSqlKey, delete)
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    , isNothing, select, orderBy, asc, just
    )


postAdmPriceDeleteR :: PricelistId -> Services -> Handler Html
postAdmPriceDeleteR pid sids = do
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    open <- (("open",) <$>) <$> runInputGet (iopt textField "open")
    runDB $ delete pid
    addMessageI "info" MsgRecordDeleted
    redirect (AdminR $ AdmServicesR sids,catMaybes [scrollY,open])


getAdmPriceEditR :: PricelistId -> Services -> Handler Html
getAdmPriceEditR pid (Services sids) = do
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    open <- (("open",) <$>) <$> runInputGet (iopt textField "open")
    price <- runDB $ selectOne $ do
        x <- from $ table @Pricelist
        where_ $ x ^. PricelistId ==. val pid
        return x
    (widget,enctype) <- generateFormPost $ formPrice (last sids) price
    defaultLayout $ do
        setTitleI MsgPrice
        $(widgetFile "admin/services/edit-price")


postAdmPriceR :: PricelistId -> Services -> Handler Html
postAdmPriceR pid (Services sids) = do
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    open <- (("open",) <$>) <$> runInputGet (iopt textField "open")
    price <- runDB $ selectOne $ do
        x <- from $ table @Pricelist
        where_ $ x ^. PricelistId ==. val pid
        return x
    ((fr,widget),enctype) <- runFormPost $ formPrice (last sids) price
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          addMessageI "info" MsgRecordEdited
          redirect ( AdminR $ AdmPriceR pid (Services sids)
                   , catMaybes [scrollY,open,Just ("pid",pack $ show $ fromSqlKey pid)]
                   )
      _ -> defaultLayout $ do
          setTitleI MsgPrice
          $(widgetFile "admin/services/edit-price")


getAdmPriceR :: PricelistId -> Services -> Handler Html
getAdmPriceR pid sids = do
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    open <- (("open",) <$>) <$> runInputGet (iopt textField "open")
    price <- runDB $ selectOne $ do
        x <- from $ table @Pricelist
        where_ $ x ^. PricelistId ==. val pid
        return x
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPrice
        $(widgetFile "admin/services/price")


postAdmPricelistR :: Services -> Handler Html
postAdmPricelistR (Services sids) = do
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    open <- (("open",) <$>) <$> runInputGet (iopt textField "open")
    ((fr,widget),enctype) <- runFormPost $ formPrice (last sids) Nothing
    case fr of
      FormSuccess r -> do
          pid <- runDB $ insert r
          addMessageI "info" MsgRecordAdded
          redirect ( AdminR $ AdmServicesR (Services sids)
                   , catMaybes [scrollY,open,Just ("pid",pack $ show $ fromSqlKey pid)]
                   )
      _ -> defaultLayout $ do
          setTitleI MsgPrice
          $(widgetFile "admin/services/create-price")


getAdmPricelistCreateR :: Services -> Handler Html
getAdmPricelistCreateR (Services sids) = do
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    open <- (("open",) <$>) <$> runInputGet (iopt textField "open")
    (widget,enctype) <- generateFormPost $ formPrice (last sids) Nothing
    defaultLayout $ do
        setTitleI MsgPrice
        $(widgetFile "admin/services/create-price")


formPrice :: ServiceId -> Maybe (Entity Pricelist)
          -> Html -> MForm Handler (FormResult Pricelist, Widget)
formPrice sid pricelist extra = do
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (pricelistName . entityVal <$> pricelist)
    (priceR,priceV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (realToFrac . pricelistPrice . entityVal <$> pricelist)
    (prefR,prefV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPrefix
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (pricelistPrefix . entityVal <$> pricelist)
    (suffR,suffV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgSuffix
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (pricelistSuffix . entityVal <$> pricelist)
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (pricelistDescr . entityVal <$> pricelist)

    let r = Pricelist sid <$> nameR <*> (realToFrac <$> priceR) <*> prefR <*> suffR <*> descrR
    let w = [whamlet|
#{extra}

$forall v <- [nameV,priceV,prefV,suffV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid
      :isJust (fvErrors v):.mdc-text-field--with-trailing-icon>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      $maybe _ <- fvErrors v
        <i.mdc-text-field__icon.mdc-text-field__icon--trailing.material-symbols-outlined>error
      <span.mdc-line-ripple>
    $maybe errs <- fvErrors v 
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}

<div.form-field>
  <label.mdc-text-field.mdc-text-field--filled.mdc-text-field--textarea data-mdc-auto-init=MDCTextField
    :isJust (fvErrors descrV):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel descrV}
    <span.mdc-text-field__resizer>
      ^{fvInput descrV}
    <span.mdc-line-ripple>
  $maybe errs <- fvErrors descrV 
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
|]
    return (r,w)
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Pricelist
              where_ $ x ^. PricelistService ==. val sid
              where_ $ x ^. PricelistName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity pid _) -> case pricelist of
              Nothing -> Left MsgPriceAlreadyInTheList
              Just (Entity pid' _) | pid == pid' -> Right name
                                   | otherwise -> Left MsgPriceAlreadyInTheList


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
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    open <- (("open",) <$>) <$> runInputGet (iopt textField "open")
    (widget,enctype) <- generateFormPost $ formService Nothing (LS.last sids)
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/create")


postAdmServicesR :: Services -> Handler Html
postAdmServicesR (Services sids) = do
    scrollY <- (("scrollY",) <$>) <$> runInputGet (iopt textField "scrollY")
    open <- (("open",) <$>) <$> runInputGet (iopt textField "open")
    ((fr,widget),enctype) <- runFormPost $ formService Nothing (LS.last sids)
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
                   , catMaybes [Just ("sid",pack $ show $ fromSqlKey sid),scrollY,open]
                   )
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "admin/services/create")


getAdmServicesR :: Services -> Handler Html
getAdmServicesR (Services sids) = do
    open <- runInputGet (iopt textField "open")
    scrollY <- fromMaybe "0" <$> runInputGet (iopt textField "scrollY")
    mpid <- (toSqlKey <$>) <$> runInputGet (iopt intField "pid")
    msid <- (toSqlKey <$>) <$> runInputGet (iopt intField "sid")
    muid <- maybeAuth
    service <- case sids of
      [] -> return Nothing
      (last -> sid) -> runDB $ selectOne $ do
          x <- from $ table @Service
          where_ $ x ^. ServiceId ==. val sid
          return x
    pricelist <- case service of
      Just (Entity sid _) -> runDB $ select $ do
          x <- from $ table @Pricelist
          where_ $ x ^. PricelistService ==. val sid
          orderBy [asc (x ^. PricelistId)]
          return x
      Nothing -> return []
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
    (publishedR,publishedV) <- mreq (myBoolField (optionsPairs [(MsgYes,True),(MsgNo,False)])) FieldSettings
        { fsLabel = SomeMessage MsgPublished
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-radio__native-control")]
        } (servicePublished . entityVal <$> service)
    (overviewR,overviewV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgOverview
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (serviceOverview . entityVal <$> service)
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
                  <*> publishedR
                  <*> overviewR
                  <*> descrR
                  <*> ((toSqlKey <$>) <$> groupR)
                )
            <*> thumbnailR
    let w = $(widgetFile "admin/services/form")
    return (r,w)
  where

      myBoolField :: Handler (OptionList Bool) -> Field Handler Bool
      myBoolField = withRadioField
          (\_ _ -> [whamlet||])
          (\theId value _isSel text optionW -> [whamlet|
<div.mdc-form-field.mdc-touch-target-wrapper>
  <div.mdc-radio.mdc-radio--touch>
    ^{optionW}
    <div.mdc-radio__background>
      <div.mdc-radio__outer-circle>
      <div.mdc-radio__inner-circle>
    <div.mdc-radio__ripple>
    <div.mdc-radio__focus-ring>
  <label for=#{theId}-#{value}>#{text}
|])
      
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


