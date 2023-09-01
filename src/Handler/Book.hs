{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Book
  ( getBookStartR
  , getBookOffersR
  , getBookStaffR
  , getBookTimeR
  , postBookR
  ) where

import Control.Monad (unless)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Fixed (Centi)
import qualified Data.List.Safe as LS (head)
import Data.Text (unpack, intercalate, pack, Text)
import Data.Time (Day)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (TimeOfDay)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage, SomeMessage (SomeMessage))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getRequest
    , YesodRequest (reqGetParams), getYesod, languages, whamlet, newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth (maybeAuth, Route (LoginR, LogoutR))
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , Field (Field, fieldParse, fieldView, fieldEnctype)
    , Enctype (UrlEncoded)
    , FieldView (fvInput, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields (multiSelectFieldList, timeField, dayField, selectFieldList)
import Yesod.Form.Functions (mreq, check, mopt, runFormGet)
import Settings (widgetFile)

import Yesod.Persist.Core (runDB)
import Database.Persist ( Entity(Entity) )
import Database.Persist.Sql ( fromSqlKey, toSqlKey, SqlBackend )

import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , orderBy, asc, desc, in_, valList
    )

import Foundation
    ( Handler, Widget
    , Route (BookStartR, BookOffersR, BookTimeR, BookStaffR, BookR, AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR)
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgBook, MsgPhoto, MsgLogout, MsgChooseServicesToBook
      , MsgServices, MsgSymbolHour, MsgSymbolMinute, MsgStaff
      , MsgNoPreference, MsgMaximumAvailability, MsgSelectStaff
      , MsgSelectAtLeastOneServicePlease, MsgOffer
      , MsgAppointmentTime, MsgRole, MsgSignUpToContinue
      , MsgSignUp, MsgSignIn, MsgSelectedServices, MsgSelectedStaff
      , MsgProceed, MsgReceptionTime, MsgOffers
      )
    )

import Model
    ( Service(Service)
    , Offer (Offer), OfferId
    , Staff (Staff)
    , Role (Role)
    , User (User)
    , EntityField
      ( StaffId, RoleId, ServiceId, OfferService, ServicePublished
      , ServiceName, RoleStaff, RoleRating, RoleService, OfferId
      )
    )


postBookR :: Handler Html
postBookR = undefined


getBookTimeR :: Handler Html
getBookTimeR = do
    offers <- runDB queryOffers
    roles <- runDB $ queryRoles []
    ((fr,fw),et) <- runFormGet $ formTime Nothing Nothing [] offers Nothing roles
    case fr of
      FormSuccess (items,role,day,time) -> do
          idFormBack <- newIdent
          let formBack = [whamlet|
<form method=get action=@{BookStaffR} enctype=#{et} novalidate ##{idFormBack} hidden>
  ^{fw}
|]
          ((fr,fw),et) <- runFormGet formCustomer
          defaultLayout $ do
              idFormNext <- newIdent
              setTitleI MsgAppointmentTime
              $(widgetFile "book/time")
      _ -> defaultLayout $ do
          idFormBack <- newIdent
          let formBack = [whamlet|
<form method=get action=@{BookOffersR} enctype=#{et} novalidate ##{idFormBack} hidden>
  ^{fw}
|]
          idFormNext <- newIdent
          let items = []
          let role = Nothing
          setTitleI MsgStaff
          $(widgetFile "book/staff")

    
getBookStaffR :: Handler Html
getBookStaffR = do
    offers <- runDB queryOffers
    roles <- runDB $ queryRoles []
    ((fr,fw),et) <- runFormGet $ formStaff [] offers roles
    case fr of
      FormSuccess (items,role) -> do
          idFormBack <- newIdent
          let formBack = [whamlet|
<form method=get action=@{BookOffersR} enctype=#{et} novalidate ##{idFormBack} hidden>
  ^{fw}
|]
          idFormNext <- newIdent
          ((fr,fw),et) <- runFormGet $ formTime Nothing Nothing items offers role roles
          defaultLayout $ do
              setTitleI MsgStaff
              $(widgetFile "book/staff")
      _ -> defaultLayout $ do
          idFormBack <- newIdent
          let formBack = [whamlet|
<form method=get action=@{BookStartR} enctype=#{et} ##{idFormBack} novalidate hidden>
  ^{fw}
|]
          idFormNext <- newIdent
          setTitleI MsgOffers
          let items = []
          $(widgetFile "book/offers")


getBookOffersR :: Handler Html
getBookOffersR = do
    muid <- maybeAuth
    oids <- (toSqlKey . read . unpack . snd <$>) . filter ((== "oid") . fst) . reqGetParams <$> getRequest
    offers <- runDB queryOffers
    ((fr,fw),et) <- runFormGet $ formOffers oids offers
    case fr of
      FormSuccess items -> do
          idFormBack <- newIdent
          let formBack = [whamlet|
<form method=get action=@{BookStartR} enctype=#{et} ##{idFormBack} novalidate hidden>
  ^{fw}
|]
          idFormNext <- newIdent
          roles <- runDB $ queryRoles items
          ((fr,fw),et) <- runFormGet $ formStaff items offers roles
          defaultLayout $ do
              setTitleI MsgOffers
              $(widgetFile "book/offers")
      _ -> defaultLayout $ do
        setTitleI MsgOffers
        $(widgetFile "book/start")


getBookStartR :: Handler Html
getBookStartR = do
    muid <- maybeAuth
    oids <- (toSqlKey . read . unpack . snd <$>) . filter ((== "oid") . fst) . reqGetParams <$> getRequest
    offers <- runDB queryOffers
    ((fr,fw),et) <- runFormGet $ formOffers oids offers
    defaultLayout $ do
        setTitleI MsgOffers
        $(widgetFile "book/start")


formCustomer :: Html -> MForm Handler (FormResult Text, Widget)
formCustomer extra = return (pure "Cutomer",[whamlet|
#{extra}
<div #customerInfo>
    <button.mdc-button.mdc-button--outlined type=button>
      <span.mdc-button__ripple>
      <span.mdc-button__focus-ring>
      <span.mdc-button__label>_{MsgSignUp}
    <div style="display:inherit;flex-direction:inherit">
      <div>
        <small>Already have an account?
      <button.mdc-button.mdc-button--outlined type=button>
        <span.mdc-button__ripple>
        <span.mdc-button__focus-ring>
        <span.mdc-button__label>_{MsgSignIn}
|])


formTime :: Maybe Day
         -> Maybe TimeOfDay
         -> [(Entity Service, Entity Offer)]
         -> [(Entity Service, Entity Offer)]
         -> Maybe (Entity Staff, Entity Role)
         -> [(Entity Staff, Entity Role)]
         -> Html
         -> MForm Handler ( FormResult ( [(Entity Service, Entity Offer)]
                                       , Maybe (Entity Staff, Entity Role)
                                       , Day
                                       , TimeOfDay
                                       )
                           , Widget
                           )
formTime day time items offers role roles extra = do

    (offersR,offersV) <- mreq (multiSelectFieldList ((MsgOffer,) <$> offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (Just items)

    (roleR,roleV) <- mopt (selectFieldList ((MsgRole,) <$> roles)) FieldSettings
        { fsLabel = SomeMessage MsgRole
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (Just role)
    
    (dayR,dayV) <- mreq dayField "" day
    (timeR,timeV) <- mreq timeField "" time
    
    let r = (,,,) <$> offersR <*> roleR <*> dayR <*> timeR
    let w = [whamlet|
#{extra}
^{fvInput offersV}
^{fvInput roleV}
$forall v <- [dayV,timeV]
  <div.form-field>
    ^{fvInput v}
    <div>
      $maybe errs <- fvErrors v
        #{errs}
|]
    return (r,w)


formStaff :: [(Entity Service, Entity Offer)]
          -> [(Entity Service, Entity Offer)]
          -> [(Entity Staff, Entity Role)]
          -> Html
          -> MForm Handler ( FormResult ( [(Entity Service, Entity Offer)]
                                        , Maybe (Entity Staff, Entity Role)
                                        )
                           , Widget
                           )
formStaff items offers roles extra = do

    (offersR,offersV) <- mreq (multiSelectFieldList ((MsgOffer,) <$> offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (Just items)

    (rolesR,rolesV) <- mopt (optionsField roles) "" Nothing

    let r = (,) <$> offersR <*> rolesR
    let w = [whamlet|
#{extra}
^{fvInput offersV}
^{fvInput rolesV}
|]
    return (r,w)
  where

      optionsField :: [(Entity Staff, Entity Role)]
                   -> Field Handler (Entity Staff, Entity Role)
      optionsField items = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) items
              _ -> Right Nothing
          , fieldView = \theId name attrs val _isReq -> do
                let isChecked (Left _) _ = False
                    isChecked (Right y) x = x == y
                $(widgetFile "book/empls")
          , fieldEnctype = UrlEncoded
          }


formOffers :: [OfferId]
           -> [(Entity Service, Entity Offer)]
           -> Html -> MForm Handler (FormResult [(Entity Service, Entity Offer)], Widget)
formOffers oids offers extra = do
    (r,v) <- mreq (check notNull (optionsField offers)) "" (Just (filter (\(_,Entity oid _) -> oid `elem` oids) offers))
    let w = [whamlet|
$maybe errs <- fvErrors v
  <div.mdc-banner role=banner data-mdc-auto-init=MDCBanner>
    <div.mdc-banner__content role=alertdialog aria-live=assertive>
      <div.mdc-banner__graphic-text-wrapper>
        <div.mdc-banner__graphic role=img style="background-color:var(--mdc-theme-error)">
          <i.mdc-banner__icon.material-symbols-outlined>warning
        <div.mdc-banner__text>
          #{errs}
      <div.mdc-banner__actions>
        <button.mdc-banner__primary-action.mdc-icon-button type=button>
          <span.mdc-icon-button__ripple>
          <i.material-symbols-outlined>close
#{extra}
^{fvInput v}
|]
    return (r,w)
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      optionsField :: [(Entity Service, Entity Offer)]
                   -> Field Handler [(Entity Service, Entity Offer)]
      optionsField items = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\(_, Entity oid _) -> oid `elem` (toSqlKey . read . unpack <$> xs))) items
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = x `elem` xs
                $(widgetFile "book/items")
          , fieldEnctype = UrlEncoded
          }


queryRoles :: [(Entity Service, Entity Offer)] -> ReaderT SqlBackend Handler [(Entity Staff, Entity Role)]
queryRoles roles = select $ do
    x :& s <- from $ table @Role
        `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
    unless (null roles) $ where_ $ x ^. RoleService `in_` valList ((\(Entity sid _,_) -> sid) <$> roles)
    orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
    return (s,x)


queryOffers :: ReaderT SqlBackend Handler [(Entity Service, Entity Offer)]
queryOffers  = select $ do
    x :& o <- from $ table @Service `innerJoin` table @Offer
        `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
    where_ $ x ^. ServicePublished
    orderBy [asc (x ^. ServiceName), asc (o ^. OfferId)]
    return (x,o)


amount :: [OfferId] -> [(Entity Service, Entity Offer)] -> Centi
amount oids xs = sum (
    (\(_,Entity _ (Offer _ _ price _ _ _)) -> price)
      <$> filter (\(_,Entity oid _) -> oid `elem` oids) xs
    )


range :: Enum a => a -> a -> [a]
range a b = [a..b]

