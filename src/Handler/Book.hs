{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Book
  ( getBookR
  , getBookOffersR
  , getBookStaffR
  , getBookTimeR
  , postBookRecordR
  ) where

import Control.Monad (unless, forM)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Either (isLeft)
import Data.Maybe (maybeToList)
import Data.Fixed (Centi)
import qualified Data.List.Safe as LS (head)
import Data.Text (unpack, intercalate)
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
    , setUltDestCurrent, MonadIO (liftIO), getMessages
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
import Yesod.Form.Functions (mreq, check, mopt, runFormPost, runFormGet, generateFormPost)
import Settings (widgetFile)

import Yesod.Persist.Core (runDB)
import Database.Persist ( Entity(Entity), PersistStoreWrite (insert) )
import Database.Persist.Sql ( fromSqlKey, toSqlKey, SqlBackend )

import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on
    , (^.), (?.), (==.), (:&)((:&))
    , orderBy, asc, desc, in_, valList, leftJoin
    )

import Foundation
    ( Handler, Widget
    , Route
      ( BookR, BookOffersR, BookTimeR, BookStaffR, BookRecordR
      , AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR, AccountR
      , HomeR, AppointmentsR, AppointmentR, ProfileR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgBook, MsgPhoto, MsgChooseServicesToBook, MsgServices
      , MsgSymbolHour, MsgSymbolMinute, MsgStaff, MsgNoPreference
      , MsgMaximumAvailability, MsgSelectStaff
      , MsgSelectAtLeastOneServicePlease, MsgOffer, MsgAppointmentTime
      , MsgRole, MsgSignUp, MsgSignIn, MsgSelectedServices
      , MsgSelectedStaff, MsgOffers, MsgCustomerInformation
      , MsgCustomer, MsgStepNofM, MsgContinue, MsgNotYourAccount
      , MsgLogin, MsgDay, MsgTime , MsgEnd
      )
    )

import Model
    ( Service(Service), Offer (Offer), Staff (Staff), Role (Role)
    , User (User), Book (Book)
    , EntityField
      ( StaffId, RoleId, ServiceId, OfferService, ServicePublished, BookId
      , ServiceName, RoleStaff, RoleRating, RoleService, OfferId, BookOffer
      , BookUser, UserId, BookRole
      )
    )


postBookRecordR :: Handler Html
postBookRecordR = do
    user <- maybeAuth
    offers <- runDB queryOffers
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
              
      FormFailure errs -> defaultLayout $ do
          msgs <- getMessages
          idFormBack <- newIdent
          let formBack = [whamlet|
                                 <details>
                                   <summary>Form back 4
                                   <form method=get action=@{BookStaffR} enctype=#{et} ##{idFormBack} novalidate>
                                     ^{fw}
                                 |]
          idFormNext <- newIdent
          $(widgetFile "book/time")
          [whamlet|
<ul>
  $forall err <- errs
    <li style="color:red">#{err}
|]
      _ -> defaultLayout [whamlet|Unknown error|]
    


getBookTimeR :: Handler Html
getBookTimeR = do
    user <- maybeAuth
    offers <- runDB queryOffers
    roles <- runDB $ queryRoles offers
    ((fr,fw),et) <- runFormGet $ formTime Nothing Nothing [] offers Nothing roles
    case fr of
      FormSuccess (items,role,day,time) -> do
          idFormBack <- newIdent
          let formBack = [whamlet|
                                 <details>
                                   <summary>Form back 3
                                   <form method=get action=@{BookStaffR} enctype=#{et} ##{idFormBack} novalidate>
                                     ^{fw}
                                 |]
          (fw,et) <- generateFormPost $ formBook user (Just day) (Just time) items items role (maybeToList role)
          msgs <- getMessages
          defaultLayout $ do
              idFormNext <- newIdent
              setTitleI MsgAppointmentTime
              setUltDestCurrent
              $(widgetFile "book/time")
              
      FormFailure errs -> do
          idFormBack <- newIdent
          let formBack = [whamlet|<form method=get action=@{BookOffersR} enctype=#{et} novalidate ##{idFormBack} hidden>^{fw}|]
          idFormNext <- newIdent
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              $(widgetFile "book/staff")
              [whamlet|
<ul>
  $forall err <- errs
    <li><b style="color:red">#{err}
|]
          
      FormMissing -> defaultLayout $ do
          msgs <- getMessages
          idFormBack <- newIdent
          let formBack = [whamlet|<form method=get action=@{BookOffersR} enctype=#{et} novalidate ##{idFormBack} hidden>^{fw}|]
          idFormNext <- newIdent
          setTitleI MsgStaff
          [whamlet|
<b style="color:red">Missing form
|]
          $(widgetFile "book/staff")

    
getBookStaffR :: Handler Html
getBookStaffR = do
    offers <- runDB queryOffers
    roles <- runDB $ queryRoles offers
    ((fr,fw),et) <- runFormGet $ formStaff [] offers roles
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
          ((fr,fw),et) <- runFormGet $ formTime
              (Just $ utctDay now)
              (Just $ timeToTimeOfDay $ utctDayTime now)
              items items
              role (maybeToList role)
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgStaff
              $(widgetFile "book/staff")
      FormFailure errs -> defaultLayout $ do
          msgs <- getMessages
          idFormBack <- newIdent
          let formBack = [whamlet|<form method=get action=@{BookR} enctype=#{et} ##{idFormBack} novalidate hidden>^{fw}|]
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
          let formBack = [whamlet|<form method=get action=@{BookR} enctype=#{et} ##{idFormBack} novalidate hidden>^{fw}|]
          idFormNext <- newIdent
          setTitleI MsgOffers
          [whamlet|
<b style="color:red">Missing form
|]
          $(widgetFile "book/offers")


getBookOffersR :: Handler Html
getBookOffersR = do
    user <- maybeAuth
    offers <- runDB queryOffers
    ((fr,fw),et) <- runFormGet $ formOffers [] offers
    case fr of
      FormSuccess items -> do
          idFormBack <- newIdent
          let formBack = [whamlet|
                                 <details>
                                   <summary>Form back 1
                                   <form method=get action=@{BookR} enctype=#{et} ##{idFormBack} novalidate>
                                     ^{fw}
                                 |]
          idFormNext <- newIdent
          roles <- runDB $ queryRoles items
          ((fr,fw),et) <- runFormGet $ formStaff items items roles
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgOffers
              $(widgetFile "book/offers")
      _ -> defaultLayout $ do
          msgs <- getMessages
          let items = []
          setTitleI MsgOffers
          $(widgetFile "book/start")


getBookR :: Handler Html
getBookR = do
    user <- maybeAuth
    offers <- runDB queryOffers
    ((fr,fw),et) <- runFormGet $ formOffers [] offers
    let items = case fr of
          FormSuccess xs -> xs
          _ -> []
    msgs <- getMessages
    setUltDestCurrent
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
    
    let r = (,,,,) <$> offersR <*> roleR <*> dayR <*> timeR
            <*> (case user of Just u -> FormSuccess u; Nothing -> FormFailure ["Please login"])
    let w = [whamlet|
#{extra}
$forall v <- [dayV,timeV]
  <div.form-field>
    ^{fvInput v}
    <div>
      $maybe errs <- fvErrors v
        #{errs}
<p>
<details>
  <summary>_{MsgSelectedStaff}
  ^{fvInput roleV}
<p>
<details>
  <summary>_{MsgSelectedServices}
  ^{fvInput offersV}
|]
    return (r,w)
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [(Entity Service, Entity Offer)]
                  -> Field Handler [(Entity Service, Entity Offer)]
      offersField items = Field
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

      rolesField :: [(Entity Staff, Entity Role)]
                 -> Field Handler (Entity Staff, Entity Role)
      rolesField roles = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) | T.null x -> Right Nothing
                    | otherwise -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) roles
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
<p>
<details>
  <summary>_{MsgSelectedStaff}
  ^{fvInput roleV}
<p>
<details>
  <summary>_{MsgSelectedServices}
  ^{fvInput offersV}
|]
    return (r,w)
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [(Entity Service, Entity Offer)]
                  -> Field Handler [(Entity Service, Entity Offer)]
      offersField items = Field
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

      rolesField :: [(Entity Staff, Entity Role)]
                 -> Field Handler (Entity Staff, Entity Role)
      rolesField roles = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) | T.null x -> Right Nothing
                    | otherwise -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) roles
              _ -> Right Nothing
          , fieldView = \theId name attrs val _isReq -> do
                let isChecked (Left _) _ = False
                    isChecked (Right y) x = x == y
                $(widgetFile "book/empls")
          , fieldEnctype = UrlEncoded
          }


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
    (offersR,offersV) <- mreq (check notNull (offersField offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just items)

    (rolesR,rolesV) <- mopt (rolesField roles) "" Nothing

    let r = (,) <$> offersR <*> rolesR
    let w = [whamlet|
#{extra}
^{fvInput rolesV}
<p>
<details>
  <summary>_{MsgSelectedServices}
  ^{fvInput offersV}
|]
    return (r,w)
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [(Entity Service, Entity Offer)]
                  -> Field Handler [(Entity Service, Entity Offer)]
      offersField items = Field
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

      rolesField :: [(Entity Staff, Entity Role)]
                 -> Field Handler (Entity Staff, Entity Role)
      rolesField roles = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) | T.null x -> Right Nothing
                    | otherwise -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) roles
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

      offersField :: [(Entity Service, Entity Offer)]
                  -> Field Handler [(Entity Service, Entity Offer)]
      offersField items = Field
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


amount :: [(Entity Service, Entity Offer)] -> [(Entity Service, Entity Offer)] -> Centi
amount oids xs = sum (
    (\(_,Entity _ (Offer _ _ price _ _ _)) -> price)
      <$> filter (`elem` oids) xs
    )


range :: Enum a => a -> a -> [a]
range a b = [a..b]

