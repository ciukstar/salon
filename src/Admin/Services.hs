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
  , getAdmOfferCreateR
  , postAdmOfferR
  , getAdmPriceR
  , postAdmPriceR
  , getAdmPriceEditR
  , postAdmPriceDeleteR
  , getAdmServicesSearchR
  , getAdmExpertCreateR
  , postAdmExpertsR
  , getAdmExpertR
  , postAdmExpertR
  , getAdmExpertEditR
  , postAdmExpertDeleteR
  ) where

import Control.Monad (join)
import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor(second))
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.FileEmbed (embedFile)
import qualified Data.List.Safe as LS (last)
import Data.Text (Text, pack, unpack, intercalate)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (DiffTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, setUltDestCurrent
    , FileInfo (fileContentType), SomeMessage (SomeMessage)
    , addMessageI, fileSourceByteString, redirect, getMessages
    , TypedContent (TypedContent), ToContent (toContent), typeSvg
    , whamlet, preEscapedToMarkup, newIdent, getRequest
    , YesodRequest (reqGetParams), getYesod, languages, MonadTrans (lift)
    )

import Yesod.Form
    ( FormResult(FormSuccess), MForm
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvId, fvErrors, fvInput, fvLabel)
    , hiddenField, runInputGet, iopt, intField, checkM
    , mreq, textField, doubleField, mopt, textareaField, fileField
    , generateFormPost, runFormPost, unTextarea, withRadioField
    , OptionList, optionsPairs, searchField, Textarea (Textarea), check
    , htmlField, checkBool
    )
import Yesod.Form.Fields (FormMessage (MsgInvalidEntry))
import Yesod.Form.Functions (parseHelper)
import Yesod.Form.Types
    ( Field (Field, fieldParse, fieldView, fieldEnctype), Enctype (UrlEncoded) )
import Settings (widgetFile)
import Settings.StaticFiles
    ( img_add_photo_alternate_FILL0_wght400_GRAD0_opsz48_svg
    , img_photo_FILL0_wght400_GRAD0_opsz48_svg
    )

import Foundation
    ( Handler, Widget
    , Route
      ( ProfileR, StaticR, AuthR, PhotoPlaceholderR, AdminR, AccountPhotoR
      , ServiceThumbnailR
      )
    , AdminR
      ( AdmServiceCreateFormR, AdmServiceEditFormR, AdmServicesR, AdmServiceR
      , AdmServiceDeleteR, AdmServiceImageR, AdmOfferCreateR, AdmOfferR
      , AdmPriceR, AdmPriceEditR, AdmPriceDeleteR, AdmServicesSearchR
      , AdmStaffPhotoR, AdmExpertCreateR, AdmExpertsR, AdmExpertR, AdmExpertEditR
      , AdmExpertDeleteR
      )
    , AppMessage
      ( MsgServices, MsgPhoto, MsgTheName, MsgAttribution, MsgBack
      , MsgPrice, MsgDescription, MsgRecordEdited, MsgChoosePhoto
      , MsgService, MsgSave, MsgCancel, MsgRecordAdded, MsgThumbnail
      , MsgSubservices, MsgAddService, MsgAddSubservice, MsgNoServicesYet
      , MsgDeleteAreYouSure, MsgYesDelete, MsgPleaseConfirm, MsgRecordDeleted
      , MsgPrefix, MsgSuffix, MsgServisAlreadyInTheList, MsgAddOffer
      , MsgNoPriceSetYet, MsgPriceAlreadyInTheList, MsgOverview, MsgPublished
      , MsgYes, MsgNo, MsgSearch, MsgNoServicesFound, MsgSelect, MsgCategory
      , MsgCategories, MsgStatus, MsgUnpublished, MsgOffers, MsgDuration
      , MsgInvalidDurationHourMinute, MsgSymbolHour, MsgSymbolMinute
      , MsgOffer, MsgExperts, MsgNoExpertsYet, MsgAddExpert, MsgExpert
      , MsgValueNotInRange, MsgExpertAlreadyInTheList, MsgRating, MsgEmployee
      , MsgRole, MsgLogin, MsgUserProfile, MsgNavigationMenu, MsgEdit, MsgDel
      , MsgCompletionTime, MsgPatternHourMinute
      )
    )

import Database.Persist
    ( Entity (Entity, entityVal)
    , PersistStoreWrite (replace, insert, insert_)
    )
import Model
    ( ServiceId
    , Service
      ( Service, serviceName, serviceDescr, serviceGroup, serviceOverview
      , servicePublished, serviceDuration
      )
    , Thumbnail
      ( Thumbnail, thumbnailService, thumbnailPhoto, thumbnailMime
      , thumbnailAttribution
      )
    , Services (Services)
    , EntityField
      ( ServiceId, ThumbnailPhoto, ThumbnailMime, ServiceGroup, ThumbnailService
      , ServiceName, OfferService, OfferId, OfferName, ServiceOverview
      , ServiceDescr, ServicePublished, ThumbnailAttribution, RoleStaff, StaffId
      , RoleService, RoleName, RoleId, BusinessCurrency
      )
    , Offer (Offer, offerName, offerPrice, offerPrefix, offerSuffix, offerDescr, offerPublished)
    , OfferId
    , ServiceStatus (ServiceStatusPulished, ServiceStatusUnpublished)
    , Role (Role, roleStaff, roleName, roleDuration, roleRating), StaffId
    , Staff (Staff), RoleId, Business
    )

import qualified Yesod.Persist as P ((=.))
import Yesod.Persist (YesodPersist(runDB), PersistUniqueWrite (upsert))
import Database.Persist.Sql (fromSqlKey, toSqlKey, delete)
import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, in_, valList, just, justList
    , (^.), (?.), (==.), (%), (++.), (||.), (:&) ((:&)), (=.)
    , isNothing, select, orderBy, asc, upper_, like, not_, exists
    , leftJoin, on, update, set, Value (unValue), innerJoin, unValue
    )

import Menu (menu)


postAdmExpertDeleteR :: RoleId -> Services -> Handler Html
postAdmExpertDeleteR rid (Services sids) = do
    runDB $ delete rid
    addMessageI "info" MsgRecordDeleted
    stati <- reqGetParams <$> getRequest
    redirect (AdminR $ AdmServicesR (Services sids),stati)


getAdmExpertEditR :: RoleId -> Services -> Handler Html
getAdmExpertEditR rid (Services sids) = do
    stati <- reqGetParams <$> getRequest
    let sid = last sids
    role <- runDB $ selectOne $ do
        x <- from $ table @Role
        where_ $ x ^. RoleId ==. val rid
        return x
    (fw,et) <- generateFormPost $ formExpert sid role
    defaultLayout $ do
        setTitleI MsgExpert
        $(widgetFile "admin/services/expert/edit")


postAdmExpertR :: RoleId -> Services -> Handler Html
postAdmExpertR rid (Services sids) = do
    stati <- reqGetParams <$> getRequest
    let sid = last sids
    role <- runDB $ selectOne $ do
        x <- from $ table @Role
        where_ $ x ^. RoleId ==. val rid
        return x
    ((fr,fw),et) <- runFormPost $ formExpert sid role
    case fr of
      FormSuccess r -> do
          runDB $ replace rid r
          addMessageI "info" MsgRecordEdited
          redirect (AdminR $ AdmExpertR rid (Services sids),stati)
      _ -> defaultLayout $ do
          setTitleI MsgExpert
          $(widgetFile "admin/services/expert/edit")


getAdmExpertR :: RoleId -> Services -> Handler Html
getAdmExpertR rid (Services sids) = do
    stati <- reqGetParams <$> getRequest
    role <- runDB $ selectOne $ do
        r :& e :& s <- from $ table @Role
            `innerJoin` table @Staff `on` (\(r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
            `innerJoin` table @Service `on` (\(r :& _ :& s) -> r ^. RoleService ==. s ^. ServiceId)
        where_ $ r ^. RoleId ==. val rid
        return (r,e,s)
    app <- getYesod
    langs <- languages
    dlgRoleDelete <- newIdent
    defaultLayout $ do
        setTitleI MsgExpert
        $(widgetFile "admin/services/expert/expert")


postAdmExpertsR :: Services -> Handler Html
postAdmExpertsR (Services sids) = do
    stati <- reqGetParams <$> getRequest
    let sid = last sids
    ((fr,fw),et) <- runFormPost $ formExpert sid Nothing
    case fr of
      FormSuccess r -> do
          rid <- runDB $ insert r
          addMessageI "info" MsgRecordAdded
          redirect (AdminR $ AdmServicesR (Services sids),("rid",pack $ show $ fromSqlKey rid):stati)
      _ -> defaultLayout $ do
          setTitleI MsgExpert
          $(widgetFile "admin/services/expert/create")


getAdmExpertCreateR :: Services -> Handler Html
getAdmExpertCreateR (Services sids) = do
    stati <- reqGetParams <$> getRequest
    let sid = last sids
    (fw,et) <- generateFormPost $ formExpert sid Nothing
    defaultLayout $ do
        setTitleI MsgExpert
        $(widgetFile "admin/services/expert/create")


formExpert :: ServiceId -> Maybe (Entity Role) -> Html -> MForm Handler (FormResult Role,Widget)
formExpert sid role extra = do
    staff <- lift $ runDB $ select $ from $ table @Staff
    (emplR,emplV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (roleStaff . entityVal <$> role)
    (nameR,nameV) <- mreq (uniqueNameField emplR) FieldSettings
        { fsLabel = SomeMessage MsgRole
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (roleName . entityVal <$> role)
    (durationR,durationV) <- mreq hmField FieldSettings
        { fsLabel = SomeMessage MsgCompletionTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (roleDuration . entityVal <$> role)
    (ratingR,ratingV) <- mopt ratingField FieldSettings
        { fsLabel = SomeMessage MsgRating
        , fsTooltip = Nothing , fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("min","0"),("max","5")]
        } (roleRating . entityVal <$> role)

        
    let r = Role <$> emplR <*> FormSuccess sid <*> nameR <*> durationR <*> ratingR
    let w = $(widgetFile "admin/services/expert/form")
    return (r,w)
  where

      hmField :: Field Handler DiffTime
      hmField = Field
          { fieldParse = parseHelper $ \s -> case parseTimeM True defaultTimeLocale "%H:%M" . unpack $ s of
              Just x -> Right x
              Nothing -> Left $ MsgInvalidEntry s
          , fieldView = \theId name attrs v isReq -> [whamlet|
$newline never
<input type=text ##{theId} name=#{name} value=#{showVal v} *{attrs} :isReq:required>
|]
          , fieldEnctype = UrlEncoded
          }

      showVal = either id (pack . formatTime defaultTimeLocale "%H:%M")

      ratingField = checkBool (\x -> x >= 1 && x <= 5) (MsgValueNotInRange 1 5) intField

      uniqueNameField :: FormResult StaffId -> Field Handler Text
      uniqueNameField emplR = checkM (uniqueName emplR) textField

      uniqueName :: FormResult StaffId -> Text -> Handler (Either AppMessage Text)
      uniqueName emplR name = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Role
              where_ $ x ^. RoleService ==. val sid
              case emplR of
                FormSuccess eid -> where_ $ x ^. RoleStaff ==. val eid
                _ -> return ()
              where_ $ x ^. RoleName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity rid _) -> case role of
              Nothing -> Left MsgExpertAlreadyInTheList
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgExpertAlreadyInTheList


getAdmServicesSearchR :: Handler Html
getAdmServicesSearchR = do
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
        $(widgetFile "admin/services/search")


postAdmPriceDeleteR :: OfferId -> Services -> Handler Html
postAdmPriceDeleteR pid sids = do
    y <- (("y",) <$>) <$> runInputGet (iopt textField "y")
    o <- (("o",) <$>) <$> runInputGet (iopt textField "o")
    runDB $ delete pid
    addMessageI "info" MsgRecordDeleted
    redirect (AdminR $ AdmServicesR sids,catMaybes [y,o])


getAdmPriceEditR :: OfferId -> Services -> Handler Html
getAdmPriceEditR pid (Services sids) = do
    y <- (("y",) <$>) <$> runInputGet (iopt textField "y")
    o <- (("o",) <$>) <$> runInputGet (iopt textField "o")
    price <- runDB $ selectOne $ do
        x <- from $ table @Offer
        where_ $ x ^. OfferId ==. val pid
        return x
    (widget,enctype) <- generateFormPost $ formOffer (last sids) price
    defaultLayout $ do
        setTitleI MsgPrice
        $(widgetFile "admin/services/offer/edit")


postAdmPriceR :: OfferId -> Services -> Handler Html
postAdmPriceR pid (Services sids) = do
    y <- (("y",) <$>) <$> runInputGet (iopt textField "y")
    o <- (("o",) <$>) <$> runInputGet (iopt textField "o")
    price <- runDB $ selectOne $ do
        x <- from $ table @Offer
        where_ $ x ^. OfferId ==. val pid
        return x
    ((fr,widget),enctype) <- runFormPost $ formOffer (last sids) price
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          addMessageI "info" MsgRecordEdited
          redirect ( AdminR $ AdmPriceR pid (Services sids)
                   , catMaybes [y,o,Just ("pid",pack $ show $ fromSqlKey pid)]
                   )
      _ -> defaultLayout $ do
          setTitleI MsgPrice
          $(widgetFile "admin/services/offer/edit")


getAdmPriceR :: OfferId -> Services -> Handler Html
getAdmPriceR pid sids = do
    y <- (("y",) <$>) <$> runInputGet (iopt textField "y")
    o <- (("o",) <$>) <$> runInputGet (iopt textField "o")
    
    currency <- (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Business
        return $ x ^. BusinessCurrency )
        
    offer <- runDB $ selectOne $ do
        x <- from $ table @Offer
        where_ $ x ^. OfferId ==. val pid
        return x
    msgs <- getMessages
    dlgDeleteOffer <- newIdent
    defaultLayout $ do
        setTitleI MsgPrice
        $(widgetFile "admin/services/offer/offer")


postAdmOfferR :: Services -> Handler Html
postAdmOfferR (Services sids) = do
    y <- (("y",) <$>) <$> runInputGet (iopt textField "y")
    o <- (("o",) <$>) <$> runInputGet (iopt textField "o")
    ((fr,widget),enctype) <- runFormPost $ formOffer (last sids) Nothing
    case fr of
      FormSuccess r -> do
          pid <- runDB $ insert r
          addMessageI "info" MsgRecordAdded
          redirect ( AdminR $ AdmServicesR (Services sids)
                   , catMaybes [y,o,Just ("pid",pack $ show $ fromSqlKey pid)]
                   )
      _ -> defaultLayout $ do
          setTitleI MsgPrice
          $(widgetFile "admin/services/offer/create")


getAdmOfferCreateR :: Services -> Handler Html
getAdmOfferCreateR (Services sids) = do
    y <- (("y",) <$>) <$> runInputGet (iopt textField "y")
    o <- (("o",) <$>) <$> runInputGet (iopt textField "o")
    (widget,enctype) <- generateFormPost $ formOffer (last sids) Nothing
    defaultLayout $ do
        setTitleI MsgOffer
        $(widgetFile "admin/services/offer/create")


formOffer :: ServiceId -> Maybe (Entity Offer)
          -> Html -> MForm Handler (FormResult Offer, Widget)
formOffer sid offer extra = do
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (offerName . entityVal <$> offer)
    (publishedR,publishedV) <- mreq (mdcBoolField (optionsPairs [(MsgYes,True),(MsgNo,False)])) FieldSettings
        { fsLabel = SomeMessage MsgPublished
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-radio__native-control")]
        } (offerPublished . entityVal <$> offer)
    (priceR,priceV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (realToFrac . offerPrice . entityVal <$> offer)
    (prefR,prefV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPrefix
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (offerPrefix . entityVal <$> offer)
    (suffR,suffV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgSuffix
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (offerSuffix . entityVal <$> offer)
    (descrR,descrV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (offerDescr . entityVal <$> offer)

    let r = Offer sid <$> nameR <*> publishedR <*> (realToFrac <$> priceR) <*> prefR <*> suffR <*> descrR
    let w = [whamlet|
#{extra}

<div.form-field>
  <div style="padding-left:1rem;display:flex;align-items:center">
    <small style="padding;opacity:0.6">
      _{MsgPublished}<sup>* 
    ^{fvInput publishedV}
  $maybe errs <- fvErrors publishedV
    <label.mdc-text-field.mdc-text-field--invalid>
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}

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
              x <- from $ table @Offer
              where_ $ x ^. OfferService ==. val sid
              where_ $ x ^. OfferName ==. val name
              return x
          return $ case mx of
            Nothing -> Right name
            Just (Entity pid _) -> case offer of
              Nothing -> Left MsgPriceAlreadyInTheList
              Just (Entity pid' _) | pid == pid' -> Right name
                                   | otherwise -> Left MsgPriceAlreadyInTheList

      mdcBoolField :: Handler (OptionList Bool) -> Field Handler Bool
      mdcBoolField = withRadioField
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


getAdmServiceImageR :: ServiceId -> Handler TypedContent
getAdmServiceImageR sid = do
    img <- runDB $ selectOne $ do
        x <- from $ table @Thumbnail
        where_ $ x ^. ThumbnailService ==. val sid
        return x
    return $ case img of
      Just (Entity _ (Thumbnail _ photo mime _)) -> TypedContent (encodeUtf8 mime) (toContent photo)
      Nothing -> TypedContent typeSvg $ toContent $(embedFile "static/img/photo_FILL0_wght400_GRAD0_opsz48.svg")


postAdmServiceDeleteR :: Services -> Handler Html
postAdmServiceDeleteR (Services sids) = do
    y <- fromMaybe "0" <$> runInputGet (iopt textField "y")
    runDB $ delete (last sids)
    addMessageI "info" MsgRecordDeleted
    redirect (AdminR $ AdmServicesR (Services (init sids)),[("y",y)])


postAdmServiceR :: Services -> Handler Html
postAdmServiceR (Services sids) = do
    service <- runDB $ selectOne $ do
        x :& t <- from $ table @Service `leftJoin` table @Thumbnail
            `on` (\(x :& t) -> just (x ^. ServiceId) ==. t ?. ThumbnailService)
        where_ $ x ^. ServiceId ==. val (last sids)
        return (x,t)
    ((fr,widget),enctype) <- runFormPost $ formService (fst <$> service) Nothing (snd =<< service)
    y <- fromMaybe "0" <$> runInputGet (iopt textField "y")
    case fr of
      FormSuccess (s,mfi,ma) -> do
          _ <- runDB $ replace (last sids) s
          addMessageI "info" MsgRecordEdited
          runDB $ update $ \x -> do
              set x [ThumbnailAttribution =. val ma]
              where_ $ x ^. ThumbnailService ==. val (last sids)
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                _ <- runDB $ upsert
                     (Thumbnail (last sids) bs (fileContentType fi) ma)
                     [ThumbnailPhoto P.=. bs, ThumbnailMime P.=. fileContentType fi, ThumbnailAttribution P.=. ma]
                redirect (AdminR $ AdmServicesR (Services sids),[("y",y)])
            Nothing -> redirect ( AdminR $ AdmServicesR (Services sids)
                                , [("sid",pack $ show $ fromSqlKey $ last sids),("y",y)]
                                )
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "admin/services/edit")


getAdmServiceEditFormR :: Services -> Handler Html
getAdmServiceEditFormR (Services sids) = do
    y <- fromMaybe "0" <$> runInputGet (iopt textField "y")
    service <- runDB $ selectOne $ do
        x :& t <- from $ table @Service `leftJoin` table @Thumbnail
            `on` (\(x :& t) -> just (x ^. ServiceId) ==. t ?. ThumbnailService)
        where_ $ x ^. ServiceId ==. val (last sids)
        return (x,t)
    (widget,enctype) <- generateFormPost $ formService (fst <$> service) Nothing (snd =<< service)
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/edit")


getAdmServiceCreateFormR :: Services -> Handler Html
getAdmServiceCreateFormR (Services sids) = do
    y <- (("y",) <$>) <$> runInputGet (iopt textField "y")
    o <- (("o",) <$>) <$> runInputGet (iopt textField "o")
    (widget,enctype) <- generateFormPost $ formService Nothing (LS.last sids) Nothing
    defaultLayout $ do
        setTitleI MsgService
        $(widgetFile "admin/services/create")


postAdmServicesR :: Services -> Handler Html
postAdmServicesR (Services sids) = do
    y <- (("y",) <$>) <$> runInputGet (iopt textField "y")
    o <- (("o",) <$>) <$> runInputGet (iopt textField "o")
    ((fr,widget),enctype) <- runFormPost $ formService Nothing (LS.last sids) Nothing
    case fr of
      FormSuccess (s,mfi,a) -> do
          sid <- runDB $ insert s
          addMessageI "info" MsgRecordAdded
          case mfi of
            Just fi -> do
                bs <- fileSourceByteString fi
                runDB $ insert_ Thumbnail { thumbnailService = sid
                                          , thumbnailPhoto = bs
                                          , thumbnailMime = fileContentType fi
                                          , thumbnailAttribution = a
                                          }
            Nothing -> return ()
          redirect ( AdminR $ AdmServicesR (Services sids)
                   , catMaybes [Just ("sid",pack $ show $ fromSqlKey sid),y,o]
                   )
      _ -> defaultLayout $ do
          setTitleI MsgService
          $(widgetFile "admin/services/create")


getAdmServicesR :: Services -> Handler Html
getAdmServicesR (Services sids) = do
    o <- runInputGet (iopt textField "o")
    y <- fromMaybe "0" <$> runInputGet (iopt textField "y")
    mpid <- (toSqlKey <$>) <$> runInputGet (iopt intField "pid")
    msid <- (toSqlKey <$>) <$> runInputGet (iopt intField "sid")
    user <- maybeAuth
    
    currency <- (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Business
        return $ x ^. BusinessCurrency )
        
    service <- case sids of
      [] -> return Nothing
      (last -> sid) -> (second (join . unValue) <$>) <$> runDB ( selectOne $ do
          x :& t <- from $ table @Service `leftJoin` table @Thumbnail
              `on` (\(x :& t) -> just (x ^. ServiceId) ==. t ?. ThumbnailService)
          where_ $ x ^. ServiceId ==. val sid
          return (x,t ?. ThumbnailAttribution) )
    offer <- case service of
      Just (Entity sid _,_) -> runDB $ select $ do
          x <- from $ table @Offer
          where_ $ x ^. OfferService ==. val sid
          orderBy [asc (x ^. OfferId)]
          return x
      Nothing -> return []

    experts <- case service of
      Just (Entity sid _,_) -> runDB $ select $ do
        r :& e <- from $ table @Role
            `innerJoin` table @Staff `on` (\(r :& e) -> r ^. RoleStaff ==. e ^. StaffId)
        where_ $ r ^. RoleService ==. val sid
        return (r,e)
      Nothing -> return []

    services <- (second (join . unValue) <$>) <$> runDB ( select $ do
        s :& t <- from $ table @Service `leftJoin` table @Thumbnail
            `on` (\(s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
        case sids of
          [] -> where_ $ isNothing $ s ^. ServiceGroup
          (last -> x) -> where_ $ s ^. ServiceGroup ==. just (val x)
        orderBy [asc (s ^. ServiceId)]
        return (s,t ?. ThumbnailAttribution) )
    setUltDestCurrent
    msgs <- getMessages
    app <- getYesod
    langs <- languages
    btnDelete <- newIdent
    dlgDelete <- newIdent
    detailsDescription <- newIdent
    detailsOffer <- newIdent
    detailsExperts <- newIdent
    detailsSubservices <- newIdent
    fabAddOffer <- newIdent
    fabAddExpert <- newIdent
    defaultLayout $ do
        setTitleI MsgServices
        $(widgetFile "admin/services/services")


subservices :: [(Entity Service,Maybe Html)] -> [ServiceId] -> Maybe ServiceId -> Widget
subservices services sids msid = $(widgetFile "admin/services/subservices")


formService :: Maybe (Entity Service) -> Maybe ServiceId -> Maybe (Entity Thumbnail)
            -> Html -> MForm Handler (FormResult (Service, Maybe FileInfo, Maybe Html), Widget)
formService service group thumbnail extra = do
    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (serviceName . entityVal <$> service)
    (publishedR,publishedV) <- mreq (mdcBoolField (optionsPairs [(MsgYes,True),(MsgNo,False)])) FieldSettings
        { fsLabel = SomeMessage MsgPublished
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-radio__native-control")]
        } (servicePublished . entityVal <$> service)
    (overviewR,overviewV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgOverview
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (serviceOverview . entityVal <$> service)
    (durationR,durationV) <- mopt durationField FieldSettings
        { fsLabel = SomeMessage MsgDuration
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((pack . formatTime defaultTimeLocale "%H:%M" <$>) . serviceDuration . entityVal <$> service)
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
    (attributionR,attributionV) <- mopt htmlField  FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (thumbnailAttribution . entityVal <$> thumbnail)

    let r = (,,)
            <$> ( Service
                  <$> nameR
                  <*> publishedR
                  <*> overviewR
                  <*> ((parseTimeM True defaultTimeLocale "%H:%M" . unpack =<<) <$> durationR)
                  <*> descrR
                  <*> ((toSqlKey <$>) <$> groupR)
                )
            <*> thumbnailR <*> attributionR
    let w = $(widgetFile "admin/services/form")
    return (r,w)
  where

      durationField = check validateDuration textField

      validateDuration x = case (parseTimeM True defaultTimeLocale "%H:%M" (unpack x) :: Maybe DiffTime) of
                             Nothing -> Left $ MsgInvalidDurationHourMinute x
                             _ -> Right x

      mdcBoolField :: Handler (OptionList Bool) -> Field Handler Bool
      mdcBoolField = withRadioField
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


range :: Enum a => a -> a -> [a]
range a b = [a..b]
