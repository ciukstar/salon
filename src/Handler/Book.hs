{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Book
  ( getBookStartR
  , getBookOffersR
  , getBookStaffR
  , getBookStaffBackR
  , getBookTimeR
  , getBookRecordR

  , getV2BookOffersR
  , postV2BookOffersR
  , getV2BookStaffR
  , postV2BookStaffR
  , getV2BookTimeR
  , postV2BookTimeR
  , getV2BookCustomerR
  , postV2BookCustomerR
  ) where

import Control.Monad (unless, forM, when, forM_)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Either (isLeft)
import Data.Maybe (maybeToList)
import Data.Fixed (Centi)
import qualified Data.List.Safe as LS (head)
import Data.Text (unpack, intercalate, pack)
import qualified Data.Text as T (null)
import Data.Time
    ( Day, TimeOfDay, getCurrentTime, UTCTime (utctDay, utctDayTime)
    , timeToTimeOfDay
    )
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage, SomeMessage (SomeMessage))
import Yesod.Core
    ( Yesod(defaultLayout), getYesod, languages, whamlet, newIdent
    , setUltDestCurrent, MonadIO (liftIO), getMessages, redirect, getRequest, YesodRequest (reqGetParams), addMessage, addMessageI
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess, FormFailure, FormMissing)
    , Field (Field, fieldParse, fieldView, fieldEnctype)
    , Enctype (UrlEncoded)
    , FieldView (fvInput, fvErrors)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields (timeField, dayField)
import Yesod.Form.Functions (mreq, check, mopt, runFormPost, runFormGet, generateFormPost, generateFormGet')
import Settings (widgetFile)

import Yesod.Persist.Core (runDB)
import Database.Persist ( Entity(Entity), PersistStoreWrite (insert) )
import Database.Persist.Sql ( fromSqlKey, toSqlKey, SqlBackend )

import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on
    , (^.), (?.), (==.), (:&)((:&))
    , orderBy, asc, desc, in_, valList, leftJoin, val, selectOne
    )

import Foundation
    ( Handler, Widget
    , Route
      ( BookStartR, BookOffersR, BookStaffBackR, BookStaffR, BookTimeR, BookRecordR
      , AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR, AccountR
      , HomeR, AppointmentsR, AppointmentR, ProfileR

      , V2BookOffersR, V2BookStaffR, V2BookTimeR, V2BookCustomerR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgBook, MsgPhoto, MsgChooseServicesToBook, MsgServices
      , MsgSymbolHour, MsgSymbolMinute, MsgStaff, MsgNoPreference
      , MsgMaximumAvailability, MsgSelectStaff, MsgInvalidFormData
      , MsgSelectAtLeastOneServicePlease, MsgOffer, MsgAppointmentTime
      , MsgRole, MsgSignUp, MsgSignIn, MsgSelectedServices
      , MsgSelectedStaff, MsgOffers, MsgCustomerInformation
      , MsgCustomer, MsgStepNofM, MsgContinue, MsgNotYourAccount
      , MsgLogin, MsgDay, MsgTime , MsgEnd, MsgAlreadyHaveAnAccount
      , MsgLoginToIdentifyCustomer, MsgSelectedTime, MsgMissingForm
      )
    )

import Model
    ( Service(Service), Offer (Offer), OfferId, Staff (Staff)
    , Role (Role), RoleId
    , User (User), Book (Book)
    , EntityField
      ( StaffId, RoleId, ServiceId, OfferService, ServicePublished, BookId
      , ServiceName, RoleStaff, RoleRating, RoleService, OfferId, BookOffer
      , BookUser, UserId, BookRole
      )
    )

import Menu (menu)


postV2BookCustomerR :: Handler Html
postV2BookCustomerR = do
    user <- maybeAuth
    offers <- runDB $ queryOffers2 []
    roles <- runDB $ queryRoles offers
    ((fr,fw),et) <- runFormPost $ formBook user Nothing Nothing [] offers Nothing roles
    case fr of
      FormSuccess (items,role,day,time,Entity uid _) -> do
          bids <- forM items $ \(_,Entity oid _) -> runDB $
              insert $ Book uid oid ((\(_,Entity rid _) -> rid) <$> role) day time
          books <- runDB $ select $ do
              x :& o :& r :& u <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `leftJoin` table @Role `on` (\(x :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
                  `innerJoin` table @User `on` (\(x :& _ :& _ :& u) -> x ^. BookUser ==. u ^. UserId)
              where_ $ x ^. BookId `in_` valList bids
              return (x,o,r,u)
          msgs <- getMessages
          defaultLayout $ do
              $(widgetFile "book/end")
      _ -> do
          rids <- filter ((== "rid") . fst) . reqGetParams <$> getRequest
          oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
          dates <- filter ((== "date") . fst) . reqGetParams <$> getRequest
          times <- filter ((== "time") . fst) . reqGetParams <$> getRequest
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCustomer
              $(widgetFile "book/v2/customer")


getV2BookCustomerR :: Handler Html
getV2BookCustomerR = do
    user <- maybeAuth
    rids <- filter ((== "rid") . fst) . reqGetParams <$> getRequest
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    dates <- filter ((== "date") . fst) . reqGetParams <$> getRequest
    times <- filter ((== "time") . fst) . reqGetParams <$> getRequest
    items <- runDB $ queryItems (toSqlKey . read . unpack . snd <$> oids)
    role <- runDB $ selectOne $ do
        x :& s <- from $ table @Role `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
        where_ $ x ^. RoleId `in_` valList (toSqlKey . read . unpack . snd <$> rids)
        return (s,x)
    (fw,et) <- generateFormPost $ formBook
        user
        ((read . unpack . snd <$>) $ LS.head dates)
        ((read . unpack . snd <$>) $ LS.head times)
        items items role (maybeToList role)
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgCustomer
        $(widgetFile "book/v2/customer")


postV2BookTimeR :: Handler Html
postV2BookTimeR = do
    user <- maybeAuth
    offers <- runDB $ queryOffers2 []
    roles <- runDB $ queryRoles offers
    ((fr,fw),et) <- runFormPost $ formTime Nothing Nothing [] offers Nothing roles
    msgs <- getMessages
    case fr of
      FormSuccess (items,role,date,time) -> do
          redirect ( V2BookCustomerR
                   , maybeToList (("rid",) . (\(_,Entity rid _) -> pack $ show $ fromSqlKey rid) <$> role)
                     <> ((\(_,Entity oid _) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
                     <> [("date",pack $ show date),("time",pack $ show time)]
                   )
      _ -> do
          oids <- filter ((== "oid") . fst)  . reqGetParams <$> getRequest
          rids <- filter ((== "rid") . fst)  . reqGetParams <$> getRequest
          defaultLayout $ do
              setTitleI MsgAppointmentTime
              $(widgetFile "book/v2/time")


getV2BookTimeR :: Handler Html
getV2BookTimeR = do
    rids <- filter ((== "rid") . fst) . reqGetParams <$> getRequest
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    date <- (read . unpack . snd <$>) . LS.head . filter ((== "date") . fst) . reqGetParams <$> getRequest
    time <- (read . unpack . snd <$>) . LS.head . filter ((== "time") . fst) . reqGetParams <$> getRequest
    items <- runDB $ queryItems (toSqlKey . read . unpack . snd <$> oids)
    role <- runDB $ selectOne $ do
        x :& s <- from $ table @Role `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
        where_ $ x ^. RoleId `in_` valList (toSqlKey . read . unpack . snd <$> rids)
        return (s,x)
    (fw,et) <- generateFormPost $ formTime date time items items role (maybeToList role)
    user <- maybeAuth
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        $(widgetFile "book/v2/time")


postV2BookStaffR :: Handler Html
postV2BookStaffR = do
    user <- maybeAuth
    offers <- runDB $ queryOffers2 []
    roles <- runDB $ queryRoles offers
    ((fr,fw),et) <- runFormPost $ formStaff [] offers Nothing roles
    msgs <- getMessages
    case fr of
      FormSuccess (items,role) -> do
          redirect ( V2BookTimeR
                   , maybeToList (("rid",) . (\(_,Entity rid _) -> pack $ show $ fromSqlKey rid) <$> role)
                     <> ((\(_,Entity oid _) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
                   )
      _ -> do
          oids <- filter ((== "oid") . fst)  . reqGetParams <$> getRequest
          defaultLayout $ do
              setTitleI MsgStaff
              $(widgetFile "book/v2/staff")


getV2BookStaffR :: Handler Html
getV2BookStaffR = do
    user <- maybeAuth
    oids <- filter ((== "oid") . fst)  . reqGetParams <$> getRequest
    rids <- filter ((== "rid") . fst)  . reqGetParams <$> getRequest
    items <- runDB $ queryOffers2 (toSqlKey . read . unpack . snd <$> oids)
    roles <- runDB $ queryRoles items
    role <- runDB $ queryRole (toSqlKey . read . unpack . snd <$> rids)
    (fw,et) <- generateFormPost $ formStaff items items role roles
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "book/v2/staff")


postV2BookOffersR :: Handler Html
postV2BookOffersR = do
    user <- maybeAuth
    offers <- runDB $ queryOffers2 []
    ((fr,fw),et) <- runFormPost $ formOffers [] offers
    case fr of
      FormSuccess items -> redirect (V2BookStaffR,(\(_,Entity oid _) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
      _ -> do          
          msgs <- getMessages
          let items = []
          defaultLayout $ do
              setTitleI MsgOffers
              $(widgetFile "book/offers/banner")


getV2BookOffersR :: Handler Html
getV2BookOffersR = do
    user <- maybeAuth
    offers <- runDB $ queryOffers2 []
    items <- do
        oids <- (toSqlKey . read . unpack . snd <$>) . filter ((== "oid") . fst)  . reqGetParams <$> getRequest
        runDB $ queryItems oids
    (fw,et) <- generateFormPost $ formOffers items offers
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgOffers
        $(widgetFile "book/offers/offers")


-- ^ Version 2

getBookRecordR :: Handler Html
getBookRecordR = do
    user <- maybeAuth
    offers <- runDB queryOffers
    roles <- runDB $ queryRoles offers
    ((fr,fw),et) <- runFormGet $ formBook user Nothing Nothing [] offers Nothing roles
    case fr of
      FormSuccess (items,role,day,time,Entity uid _) -> do
          bids <- forM items $ \(_,Entity oid _) -> runDB $
              insert $ Book uid oid ((\(_,Entity rid _) -> rid) <$> role) day time
          books <- runDB $ select $ do
              x :& o :& r :& u <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `leftJoin` table @Role `on` (\(x :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
                  `innerJoin` table @User `on` (\(x :& _ :& _ :& u) -> x ^. BookUser ==. u ^. UserId)
              where_ $ x ^. BookId `in_` valList bids
              return (x,o,r,u)
          msgs <- getMessages
          defaultLayout $ do
              $(widgetFile "book/end")

      _ -> defaultLayout $ do
          msgs <- getMessages
          idFormBack <- newIdent
          let formBack = [whamlet|
                                 <details>
                                   <summary>Form back 5
                                   <form method=get action=@{BookStaffBackR} enctype=#{et} ##{idFormBack} novalidate>
                                     ^{fw}
                                 |]
          idFormNext <- newIdent
          $(widgetFile "book/time")



getBookTimeR :: Handler Html
getBookTimeR = do
    setUltDestCurrent
    user <- maybeAuth
    offers <- runDB queryOffers
    roles <- runDB $ queryRoles offers
    ((fr,fw),et) <- runFormGet $ formTime Nothing Nothing [] offers Nothing roles
    idFormBack <- newIdent
    case fr of
      FormSuccess (items,role,day,time) -> do
          let formBack = [whamlet|
                                 <details>
                                   <summary>Form back 4
                                   <form method=get action=@{BookStaffBackR} enctype=#{et} ##{idFormBack} novalidate>
                                     ^{fw}
                                 |]
          msgs <- getMessages
          (fw,et) <- generateFormGet' $ formBook user (Just day) (Just time) items items role (maybeToList role)
          defaultLayout $ do
              idFormNext <- newIdent
              setTitleI MsgAppointmentTime
              $(widgetFile "book/time")

      _ -> do
          let formBack = [whamlet|
                                 <details>
                                   <summary>Form back 3
                                   <form method=get action=@{BookOffersR} enctype=#{et} ##{idFormBack} novalidate>
                                     ^{fw}
                                 |]
          msgs <- getMessages
          defaultLayout $ do
              idFormNext <- newIdent
              setTitleI MsgAppointmentTime
              $(widgetFile "book/staff")


getBookStaffBackR :: Handler Html
getBookStaffBackR = do
    setUltDestCurrent
    user <- maybeAuth
    offers <- runDB queryOffers
    roles <- runDB $ queryRoles offers
    ((fr,fw),et) <- runFormGet $ formStaff [] offers Nothing roles
    case fr of
      FormSuccess (items,role) -> do
          idFormBack <- newIdent
          let formBack = [whamlet|
                                 <details>
                                   <summary>Form back 2
                                   <form method=get action=@{BookOffersR} enctype=#{et} ##{idFormBack} novalidate>
                                     ^{fw}
                                 |]
          idFormNext <- newIdent
          now <- liftIO getCurrentTime
          ((fr,fw),et) <- runFormGet $ formTime Nothing Nothing items items role (maybeToList role)
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              $(widgetFile "book/staff")
      _ -> defaultLayout $ do
          msgs <- getMessages
          idFormBack <- newIdent
          let formBack = [whamlet|<form method=get action=@{BookStartR} enctype=#{et} ##{idFormBack} novalidate hidden>^{fw}|]
          idFormNext <- newIdent
          setTitleI MsgOffers
          $(widgetFile "book/offers")


getBookStaffR :: Handler Html
getBookStaffR = do
    setUltDestCurrent
    user <- maybeAuth
    offers <- runDB queryOffers
    roles <- runDB $ queryRoles offers
    ((fr,fw),et) <- runFormGet $ formStaff [] offers Nothing roles
    case fr of
      FormSuccess (items,role) -> do
          idFormBack <- newIdent
          let formBack = [whamlet|
                                 <details>
                                   <summary>Form back 2
                                   <form method=get action=@{BookOffersR} enctype=#{et} ##{idFormBack} novalidate>
                                     ^{fw}
                                 |]
          idFormNext <- newIdent
          now <- liftIO getCurrentTime
          (fw,et) <- generateFormGet' $ formTime
              Nothing
              Nothing
              items items
              role (maybeToList role)
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              $(widgetFile "book/staff")
      FormFailure errs -> defaultLayout $ do
          msgs <- getMessages
          idFormBack <- newIdent
          let formBack = [whamlet|<form method=get action=@{BookStartR} enctype=#{et} ##{idFormBack} novalidate hidden>^{fw}|]
          idFormNext <- newIdent
          setTitleI MsgOffers
          [whamlet|
<ul>
  $forall err <- errs
    <li>
      <b style="color:red">#{err}
|]
          $(widgetFile "book/offers")
      FormMissing -> defaultLayout $ do
          msgs <- getMessages
          idFormBack <- newIdent
          let formBack = [whamlet|<form method=get action=@{BookStartR} enctype=#{et} ##{idFormBack} novalidate hidden>^{fw}|]
          idFormNext <- newIdent
          setTitleI MsgOffers
          [whamlet|
<b style="color:red">Missing form
|]
          $(widgetFile "book/offers")


getBookOffersR :: Handler Html
getBookOffersR = do
    setUltDestCurrent
    user <- maybeAuth
    offers <- runDB queryOffers
    ((fr,fw),et) <- runFormGet $ formOffers [] offers
    case fr of
      FormSuccess items -> do
          idFormBack <- newIdent
          let formBack = [whamlet|
                                 <details>
                                   <summary>Form back 1
                                   <form method=get action=@{BookStartR} enctype=#{et} ##{idFormBack} novalidate>
                                     ^{fw}
                                 |]
          idFormNext <- newIdent
          roles <- runDB $ queryRoles items
          ((fr,fw),et) <- runFormGet $ formStaff items items Nothing roles
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgOffers
              $(widgetFile "book/offers")
      _ -> defaultLayout $ do
          msgs <- getMessages
          let items = []
          setTitleI MsgOffers
          $(widgetFile "book/start")


getBookStartR :: Handler Html
getBookStartR = do
    user <- maybeAuth
    offers <- runDB queryOffers
    ((fr,fw),et) <- runFormGet $ formOffers [] offers
    msgs <- getMessages
    setUltDestCurrent
    let items = case fr of
          FormSuccess xs -> xs
          _ -> []
    defaultLayout $ do
        setTitleI MsgOffers
        $(widgetFile "book/start")


formBook :: Maybe (Entity User)
         -> Maybe Day
         -> Maybe TimeOfDay
         -> [(Entity Service, Entity Offer)]
         -> [(Entity Service, Entity Offer)]
         -> Maybe (Entity Staff, Entity Role)
         -> [(Entity Staff, Entity Role)]
         -> Html -> MForm Handler ( FormResult ( [(Entity Service, Entity Offer)]
                                               , Maybe (Entity Staff, Entity Role)
                                               , Day
                                               , TimeOfDay
                                               , Entity User
                                               )
                                  , Widget
                                  )
formBook user day time items offers role roles extra = do

    (offersR,offersV) <- mreq (check notNull (offersField offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just items)

    (roleR,roleV) <- mopt (rolesField roles) FieldSettings
        { fsLabel = SomeMessage MsgRole
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just role)

    (dayR,dayV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgDay
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } day

    (timeR,timeV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } time

    app <- getYesod
    langs <- languages

    let r = (,,,,) <$> offersR <*> roleR <*> dayR <*> timeR
            <*> case user of
                  Just u -> FormSuccess u
                  Nothing -> FormFailure [renderMessage app langs MsgLoginToIdentifyCustomer]

    let w = [whamlet|
#{extra}
<details.mdc-list data-mdc-auto-init=MDCList
  ontoggle="this.querySelector('summary i.expand').textContent = this.open ? 'expand_less' : 'expand_more'">
  <summary.mdc-list-item.mdc-list-item--with-leading-icon.mdc-list-item--with-one-line.mdc-list-item--with-trailing-icon>
    <span.mdc-list-item__ripple>
    <span.mdc-list-item__start>
      <i.material-symbols-outlined>info
    <span.mdc-list-item__content>
      <div.mdc-list-item__primary-text>
        _{MsgSelectedTime}
    <span.mdc-list-item__end>
      <i.expand.material-symbols-outlined>expand_more
  $forall v <- [dayV,timeV]
    <div.form-field>
      ^{fvInput v}
      <div>
        $maybe errs <- fvErrors v
          #{errs}

<details.mdc-list data-mdc-auto-init=MDCList
  ontoggle="this.querySelector('summary i.expand').textContent = this.open ? 'expand_less' : 'expand_more'">
  <summary.mdc-list-item.mdc-list-item--with-leading-icon.mdc-list-item--with-one-line.mdc-list-item--with-trailing-icon>
    <span.mdc-list-item__ripple>
    <span.mdc-list-item__start>
      <i.material-symbols-outlined>info
    <span.mdc-list-item__content>
      <div.mdc-list-item__primary-text>
        _{MsgSelectedStaff}
    <span.mdc-list-item__end>
      <i.expand.material-symbols-outlined>expand_more
  ^{fvInput roleV}

<details.mdc-list data-mdc-auto-init=MDCList
  ontoggle="this.querySelector('summary i.expand').textContent = this.open ? 'expand_less' : 'expand_more'">
  <summary.mdc-list-item.mdc-list-item--with-leading-icon.mdc-list-item--with-one-line.mdc-list-item--with-trailing-icon>
    <span.mdc-list-item__ripple>
    <span.mdc-list-item__start>
      <i.material-symbols-outlined>info
    <span.mdc-list-item__content>
      <div.mdc-list-item__primary-text>
        _{MsgSelectedServices}
    <span.mdc-list-item__end>
      <i.expand.material-symbols-outlined>expand_more
  ^{fvInput offersV}
|]
    return (r,w)
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [(Entity Service, Entity Offer)]
                  -> Field Handler [(Entity Service, Entity Offer)]
      offersField options = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\(_, Entity oid _) -> oid `elem` (toSqlKey . read . unpack <$> xs))) options
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = x `elem` xs
                $(widgetFile "book/items")
          , fieldEnctype = UrlEncoded
          }

      rolesField :: [(Entity Staff, Entity Role)]
                 -> Field Handler (Entity Staff, Entity Role)
      rolesField options = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) | T.null x -> Right Nothing
                    | otherwise -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) options
              _ -> Right Nothing
          , fieldView = \theId name attrs val _isReq -> do
                let isChecked (Left _) _ = False
                    isChecked (Right y) x = x == y
                $(widgetFile "book/empls")
          , fieldEnctype = UrlEncoded
          }


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

    (offersR,offersV) <- mreq (check notNull (offersField offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just items)

    (roleR,roleV) <- mopt (rolesField roles) FieldSettings
        { fsLabel = SomeMessage MsgRole
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just role)

    (dayR,dayV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgDay
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } day

    (timeR,timeV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } time

    let r = (,,,) <$> offersR <*> roleR <*> dayR <*> timeR
    let w = [whamlet|
#{extra}
$forall v <- [dayV,timeV]
  <div.form-field>
    ^{fvInput v}
    <div>
      $maybe errs <- fvErrors v
        #{errs}

<details.mdc-list data-mdc-auto-init=MDCList
  ontoggle="this.querySelector('summary i.expand').textContent = this.open ? 'expand_less' : 'expand_more'">
  <summary.mdc-list-item.mdc-list-item--with-leading-icon.mdc-list-item--with-one-line.mdc-list-item--with-trailing-icon>
    <span.mdc-list-item__ripple>
    <span.mdc-list-item__start>
      <i.material-symbols-outlined>info
    <span.mdc-list-item__content>
      <div.mdc-list-item__primary-text>
        _{MsgSelectedStaff}
    <span.mdc-list-item__end>
      <i.expand.material-symbols-outlined>expand_more
  ^{fvInput roleV}

<details.mdc-list data-mdc-auto-init=MDCList
  ontoggle="this.querySelector('summary i.expand').textContent = this.open ? 'expand_less' : 'expand_more'">
  <summary.mdc-list-item.mdc-list-item--with-leading-icon.mdc-list-item--with-one-line.mdc-list-item--with-trailing-icon>
    <span.mdc-list-item__ripple>
    <span.mdc-list-item__start>
      <i.material-symbols-outlined>info
    <span.mdc-list-item__content>
      <div.mdc-list-item__primary-text>
        _{MsgSelectedServices}
    <span.mdc-list-item__end>
      <i.expand.material-symbols-outlined>expand_more
  ^{fvInput offersV}
|]
    return (r,w)
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [(Entity Service, Entity Offer)]
                  -> Field Handler [(Entity Service, Entity Offer)]
      offersField options = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\(_, Entity oid _) -> oid `elem` (toSqlKey . read . unpack <$> xs))) options
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = x `elem` xs
                $(widgetFile "book/items")
          , fieldEnctype = UrlEncoded
          }

      rolesField :: [(Entity Staff, Entity Role)]
                 -> Field Handler (Entity Staff, Entity Role)
      rolesField options = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) | T.null x -> Right Nothing
                    | otherwise -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) options
              _ -> Right Nothing
          , fieldView = \theId name attrs val _isReq -> do
                let isChecked (Left _) _ = False
                    isChecked (Right y) x = x == y
                $(widgetFile "book/empls")
          , fieldEnctype = UrlEncoded
          }


formStaff :: [(Entity Service, Entity Offer)]
          -> [(Entity Service, Entity Offer)]
          -> Maybe (Entity Staff, Entity Role)
          -> [(Entity Staff, Entity Role)]
          -> Html
          -> MForm Handler ( FormResult ( [(Entity Service, Entity Offer)]
                                        , Maybe (Entity Staff, Entity Role)
                                        )
                           , Widget
                           )
formStaff items offers role roles extra = do

    (offersR,offersV) <- mreq (check notNull (offersField offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just items)

    (rolesR,rolesV) <- mopt (rolesField roles) "" (Just role)

    let r = (,) <$> offersR <*> rolesR
    let w = [whamlet|
#{extra}
^{fvInput rolesV}

<details.mdc-list data-mdc-auto-init=MDCList
  ontoggle="this.querySelector('summary i.expand').textContent = this.open ? 'expand_less' : 'expand_more'">
  <summary.mdc-list-item.mdc-list-item--with-leading-icon.mdc-list-item--with-one-line.mdc-list-item--with-trailing-icon>
    <span.mdc-list-item__ripple>
    <span.mdc-list-item__start>
      <i.material-symbols-outlined>info
    <span.mdc-list-item__content>
      <div.mdc-list-item__primary-text>
        _{MsgSelectedServices}
    <span.mdc-list-item__end>
      <i.expand.material-symbols-outlined>expand_more
  ^{fvInput offersV}
|]
    return (r,w)
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [(Entity Service, Entity Offer)]
                  -> Field Handler [(Entity Service, Entity Offer)]
      offersField options = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\(_, Entity oid _) -> oid `elem` (toSqlKey . read . unpack <$> xs))) options
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = x `elem` xs
                $(widgetFile "book/items")
          , fieldEnctype = UrlEncoded
          }

      rolesField :: [(Entity Staff, Entity Role)]
                 -> Field Handler (Entity Staff, Entity Role)
      rolesField options = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) | T.null x -> Right Nothing
                    | otherwise -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) options
              _ -> Right Nothing
          , fieldView = \theId name attrs val _isReq -> do
                let isChecked (Left _) _ = False
                    isChecked (Right y) x = x == y
                $(widgetFile "book/empls")
          , fieldEnctype = UrlEncoded
          }


formOffers :: [(Entity Service, Entity Offer)]
           -> [(Entity Service, Entity Offer)]
           -> Html -> MForm Handler (FormResult [(Entity Service, Entity Offer)], Widget)
formOffers items offers extra = do
    (r,v) <- mreq (check notNull (offersField offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just (filter (`elem` items) offers))
    let w = [whamlet|
#{extra}
^{fvInput v}
|]
    return (r,w)
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [(Entity Service, Entity Offer)]
                  -> Field Handler [(Entity Service, Entity Offer)]
      offersField options = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\(_, Entity oid _) -> oid `elem` (toSqlKey . read . unpack <$> xs))) options
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = x `elem` xs
                $(widgetFile "book/items")
          , fieldEnctype = UrlEncoded
          }


queryRole :: [RoleId] -> ReaderT SqlBackend Handler (Maybe (Entity Staff, Entity Role))
queryRole rids = selectOne $ do
    x :& s <- from $ table @Role
        `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
    unless (null rids) $ where_ $ x ^. RoleId `in_` valList rids
    when (null rids) $ where_ $ val False
    orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
    return (s,x)


queryRoles :: [(Entity Service, Entity Offer)] -> ReaderT SqlBackend Handler [(Entity Staff, Entity Role)]
queryRoles services = select $ do
    x :& s <- from $ table @Role
        `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
    unless (null services) $ where_ $ x ^. RoleService `in_` valList ((\(Entity sid _,_) -> sid) <$> services)
    orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
    return (s,x)


queryItems :: [OfferId] -> ReaderT SqlBackend Handler [(Entity Service, Entity Offer)]
queryItems oids = select $ do
    x :& o <- from $ table @Service `innerJoin` table @Offer
        `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
    where_ $ x ^. ServicePublished
    when (null oids) $ where_ $ val False
    unless (null oids) $ where_ $ o ^. OfferId `in_` valList oids
    orderBy [asc (x ^. ServiceName), asc (o ^. OfferId)]
    return (x,o)


queryOffers2 :: [OfferId] -> ReaderT SqlBackend Handler [(Entity Service, Entity Offer)]
queryOffers2 oids = select $ do
    x :& o <- from $ table @Service `innerJoin` table @Offer
        `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
    where_ $ x ^. ServicePublished
    unless (null oids) $ where_ $ o ^. OfferId `in_` valList oids
    orderBy [asc (x ^. ServiceName), asc (o ^. OfferId)]
    return (x,o)


queryOffers :: ReaderT SqlBackend Handler [(Entity Service, Entity Offer)]
queryOffers  = select $ do
    x :& o <- from $ table @Service `innerJoin` table @Offer
        `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
    where_ $ x ^. ServicePublished
    orderBy [asc (x ^. ServiceName), asc (o ^. OfferId)]
    return (x,o)


amount :: [(Entity Service, Entity Offer)] -> [(Entity Service, Entity Offer)] -> Centi
amount oids xs = sum (
    (\(_,Entity _ (Offer _ _ price _ _ _)) -> price)
      <$> filter (`elem` oids) xs
    )


range :: Enum a => a -> a -> [a]
range a b = [a..b]
