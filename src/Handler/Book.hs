{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Book
  ( getBookR
  , postBookR
  , getBookStaffR
  , postBookStaffR
  ) where

import Control.Monad (forM)
import Data.Fixed (Centi)
import qualified Data.List.Safe as LS (head)
import Data.Text (Text, unpack, intercalate, pack)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Traversable (traverse)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage, SomeMessage (SomeMessage))
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getRequest
    , YesodRequest (reqGetParams), getYesod, languages, whamlet
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
import Yesod.Form.Fields (hiddenField, selectFieldList, multiSelectFieldList)
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost, check, mopt)
import Settings (widgetFile)

import Yesod.Persist.Core (runDB)
import Database.Persist ( Entity(Entity) )
import Database.Persist.Sql ( fromSqlKey, toSqlKey )

import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on
    , (^.), (==.), (:&)((:&))
    , orderBy, asc, desc, in_, valList
    )

import Foundation
    ( Handler, Widget
    , Route (BookStaffR, BookR, AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR)
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgBook, MsgPhoto, MsgLogout, MsgChooseServicesToBook
      , MsgServices, MsgSymbolHour, MsgSymbolMinute, MsgStaff
      , MsgNoPreference, MsgMaximumAvailability, MsgSelectStaff
      , MsgSelectAtLeastOneServicePlease, MsgAdd, MsgOffers, MsgOffer, MsgInvalidValue, MsgAppointmentTime
      )
    )

import Model
    ( Service(Service)
    , Offer (Offer), OfferId
    , Staff (Staff)
    , Role (Role), RoleId
    , EntityField
      ( StaffId, RoleId, ServiceId, OfferService, ServicePublished
      , ServiceName, RoleStaff, RoleRating, RoleService, OfferId
      )
    )


postBookStaffR :: Handler Html
postBookStaffR = do
    offers <- runDB $ select $ do
        x :& p <- from $ table @Service `innerJoin` table @Offer
            `on` (\(x :& p) -> x ^. ServiceId ==. p ^. OfferService)
        where_ $ x ^. ServicePublished
        return (x,p)
    roles <- runDB $ select $ do
        x :& s <- from $ table @Role
            `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
        orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
        return (s, x)
    ((fr,fw),et) <- runFormPost $ formStaff [] offers roles
    case fr of
      FormSuccess r -> defaultLayout $ do
          setTitleI MsgAppointmentTime
          $(widgetFile "book/time")
      _ -> undefined


getBookStaffR :: Handler Html
getBookStaffR = do
    roles <- runDB $ select $ do
        x :& s <- from $ table @Role
            `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
        orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
        return (s, x)
    let oids = []
    defaultLayout $ do
        setTitleI MsgStaff
        -- $(widgetFile "book/staff")


postBookR :: Handler Html
postBookR = do
    offers <- runDB $ select $ do
        x :& o <- from $ table @Service `innerJoin` table @Offer
            `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
        where_ $ x ^. ServicePublished
        orderBy [asc (x ^. ServiceName),asc (o ^. OfferId)]
        return (x,o)
    ((fr,fw),et) <- runFormPost $ formOffers offers
    case fr of
      FormSuccess r -> do
          roles <- runDB $ select $ do
              x :& s <- from $ table @Role
                  `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
              where_ $ x ^. RoleService `in_` valList ((\(Entity sid _,_) -> sid) <$> r)
              orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
              return (s,x)
          (fw2,et2) <- generateFormPost $ formStaff r offers roles
          let oids = (\(_,Entity oid _) -> ("oid",pack $ show $ fromSqlKey oid)) <$> r
          defaultLayout $(widgetFile "book/staff")
      _ -> defaultLayout $ do
          muid <- maybeAuth
          oids <- (toSqlKey . read . unpack . snd <$>) . filter ((== "oid") . fst) . reqGetParams <$> getRequest
          setTitleI MsgBook
          $(widgetFile "book/book")


formStaff :: [(Entity Service, Entity Offer)]
          -> [(Entity Service, Entity Offer)]
          -> [(Entity Staff, Entity Role)]
          -> Html
          -> MForm Handler (FormResult ( [(Entity Service, Entity Offer)]
                                       , Maybe (Entity Staff, Entity Role)
                                       )
                           ,Widget
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



getBookR :: Handler Html
getBookR = do
    muid <- maybeAuth
    oids <- (toSqlKey . read . unpack . snd <$>) . filter ((== "oid") . fst) . reqGetParams <$> getRequest
    offers <- runDB $ select $ do
        x :& o <- from $ table @Service `innerJoin` table @Offer
            `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
        where_ $ x ^. ServicePublished
        orderBy [asc (x ^. ServiceName), asc (o ^. OfferId)]
        return (x,o)
    (fw,et) <- generateFormPost $ formOffers offers
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgBook
        $(widgetFile "book/book")


formOffers :: [(Entity Service, Entity Offer)]
           -> Html -> MForm Handler (FormResult [(Entity Service, Entity Offer)], Widget)
formOffers offers extra = do
    oids <- (toSqlKey . read . unpack . snd <$>) . filter ((== "oid") . fst) . reqGetParams <$> getRequest
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


amount :: [OfferId] -> [(Entity Service, Entity Offer)] -> Centi
amount oids xs = sum (
    (\(_,Entity _ (Offer _ _ price _ _ _)) -> price)
      <$> filter (\(_,Entity oid _) -> oid `elem` oids) xs
    )


range :: Enum a => a -> a -> [a]
range a b = [a..b]
