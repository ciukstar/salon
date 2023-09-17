{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Book
  ( getBookOffersR
  , postBookOffersR
  , getBookStaffR
  , postBookStaffR
  , getBookTimeR
  , postBookTimeR
  , getBookCustomerR
  , postBookCustomerR
  , getBookEndR
  , sessKeyBooking
  ) where

import Control.Monad (unless, forM, when)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Either (isLeft)
import Data.Maybe (maybeToList, isJust, fromMaybe)
import Data.Fixed (Centi)
import qualified Data.List.Safe as LS (head)
import Data.Text (unpack, intercalate, pack, Text)
import qualified Data.Text as T (null, empty)
import Data.Time
    ( Day, TimeOfDay, UTCTime (utctDay), TimeZone (timeZoneMinutes)
    , LocalTime (LocalTime), utcToLocalTime, getCurrentTime
    , minutesToTimeZone
    )
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Hamlet (Html)
import Text.Shakespeare.I18N (renderMessage, SomeMessage (SomeMessage))
import Yesod.Core
    ( Yesod(defaultLayout), YesodRequest (reqGetParams)
    , getYesod, languages, whamlet, setUltDestCurrent, getMessages
    , redirect, getRequest, addMessageI, deleteSession, lookupSession
    , setSession, MonadIO (liftIO), getPostParams
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth (maybeAuth, Route (LoginR))
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess, FormFailure)
    , Field (Field, fieldParse, fieldView, fieldEnctype)
    , Enctype (UrlEncoded)
    , FieldView (fvInput, fvErrors, fvId, fvLabel)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields (timeField, dayField, textField, intField)
import Yesod.Form.Functions (mreq, check, mopt, runFormPost, generateFormPost, checkM)
import Yesod.Form.Input (runInputPost, iopt, runInputGet)
import Settings (widgetFile)

import Yesod.Persist.Core (runDB)
import Database.Persist ( Entity(Entity), PersistStoreWrite (insert) )
import Database.Persist.Sql ( fromSqlKey, toSqlKey, SqlBackend )

import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on
    , (^.), (?.), (==.), (:&)((:&))
    , orderBy, asc, desc, in_, valList, leftJoin, val
    , selectOne, exists, not_, except_, subSelectList
    )

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR, AccountR
      , HomeR, AppointmentR, ProfileR, ServiceThumbnailR
      , BookOffersR, BookStaffR, BookTimeR, BookCustomerR, BookEndR
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
      , MsgLoginToIdentifyCustomer, MsgSelectedTime, MsgRecordAdded
      , MsgYouSuccessfullyCreatedYourBooking, MsgShowDetails
      , MsgBookNewAppointment, MsgBookingSequenceRestarted, MsgThumbnail
      , MsgAppointmentDayIsInThePast, MsgAppointmentTimeIsInThePast
      , MsgTimezone, MsgMinutes
      )
    )

import Model
    ( Service(Service), Offer (Offer), OfferId, Staff (Staff)
    , Role (Role), RoleId
    , User (User), Book (Book)
    , EntityField
      ( StaffId, RoleId, ServiceId, OfferService, ServicePublished, BookId
      , ServiceName, RoleStaff, RoleRating, RoleService, OfferId, BookOffer
      , BookUser, BookRole
      )
    )

import Menu (menu)


getBookEndR :: Handler Html
getBookEndR = do
    user <- maybeAuth
    bids <- (toSqlKey . read . unpack . snd <$>) . filter ((== "bid") . fst) . reqGetParams <$> getRequest
    books <- runDB $ select $ do
        x :& o :& s :& r :& e <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Role `on` (\(x :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
        case user of
          Nothing -> where_ $ val False
          Just (Entity uid _) -> where_ $ x ^. BookUser ==. val uid
        where_ $ x ^. BookId `in_` valList bids
        return (x,o, s,r,e)
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEnd
        $(widgetFile "book/end")


postBookCustomerR :: Handler Html
postBookCustomerR = do
    resubmit <- lookupSession sessKeyBooking
    case resubmit of
      Nothing -> do
          addMessageI "info" MsgBookingSequenceRestarted
          redirect BookOffersR
      _ -> do
          rids <- filter ((== "rid") . fst) . reqGetParams <$> getRequest
          oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
          dates <- filter ((== "date") . fst) . reqGetParams <$> getRequest
          times <- filter ((== "time") . fst) . reqGetParams <$> getRequest
          tzs <- filter ((== "tz") . fst) . reqGetParams <$> getRequest
          user <- maybeAuth
          ioffers <- runDB $ queryOffers (toSqlKey . read . unpack . snd <$> oids)
          irole <- runDB $ queryRole (toSqlKey . read . unpack . snd <$> LS.head rids)
          ((fr,fw),et) <- runFormPost $ formCustomer user Nothing Nothing Nothing ioffers ioffers irole (maybeToList irole)
          case fr of
            FormSuccess (items,role,day,time,tz,Entity uid _) -> do
                bids <- forM items $ \(_,Entity oid _) -> runDB $
                    insert $ Book uid oid ((\(_,Entity rid _) -> rid) <$> role) day time tz
                addMessageI "info" MsgRecordAdded
                deleteSession sessKeyBooking
                redirect (BookEndR, ("bid",) . pack . show . fromSqlKey <$> bids)
            _ -> do
                msgs <- getMessages
                defaultLayout $ do
                    setTitleI MsgCustomer
                    $(widgetFile "book/customer/banner")


getBookCustomerR :: Handler Html
getBookCustomerR = do
    user <- maybeAuth
    rids <- filter ((== "rid") . fst) . reqGetParams <$> getRequest
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    dates <- filter ((== "date") . fst) . reqGetParams <$> getRequest
    times <- filter ((== "time") . fst) . reqGetParams <$> getRequest
    tzs <- filter ((== "tz") . fst) . reqGetParams <$> getRequest
    items <- runDB $ queryItems (toSqlKey . read . unpack . snd <$> oids)
    role <- runDB $ selectOne $ do
        x :& s <- from $ table @Role `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
        where_ $ x ^. RoleId `in_` valList (toSqlKey . read . unpack . snd <$> rids)
        return (s,x)
    (fw,et) <- generateFormPost $ formCustomer
        user
        ((read . unpack . snd <$>) $ LS.head dates)
        ((read . unpack . snd <$>) $ LS.head times)
        ((minutesToTimeZone . read . unpack . snd <$>) $ LS.head tzs)
        items items role (maybeToList role)
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgCustomer
        $(widgetFile "book/customer/customer")


postBookTimeR :: Handler Html
postBookTimeR = do
    resubmit <- lookupSession sessKeyBooking
    case resubmit of
      Nothing -> do
          addMessageI "info" MsgBookingSequenceRestarted
          redirect BookOffersR
      _ -> do
          oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
          rids <- filter ((== "rid") . fst) . reqGetParams <$> getRequest
          user <- maybeAuth
          ioffers <- runDB $ queryOffers (toSqlKey . read . unpack . snd <$> oids)
          irole <- runDB $ queryRole (toSqlKey . read . unpack . snd <$> LS.head rids)
          ((fr,fw),et) <- runFormPost $ formTime Nothing Nothing Nothing ioffers ioffers irole (maybeToList irole)
          msgs <- getMessages
          case fr of
            FormSuccess (items,role,date,time,tz) -> do
                redirect ( BookCustomerR
                         , maybeToList (("rid",) . (\(_,Entity rid _) -> pack $ show $ fromSqlKey rid) <$> role)
                           <> ((\(_,Entity oid _) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
                           <> [("date",pack $ show date),("time",pack $ show time),("tz",pack $ show (timeZoneMinutes tz))]
                         )
            _ -> defaultLayout $ do
                setTitleI MsgAppointmentTime
                $(widgetFile "book/time/banner")


getBookTimeR :: Handler Html
getBookTimeR = do
    rids <- filter ((== "rid") . fst) . reqGetParams <$> getRequest
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    date <- (read . unpack <$>) <$> runInputGet (iopt textField "date")
    time <- (read . unpack <$>) <$> runInputGet (iopt textField "time")
    tz <- (minutesToTimeZone <$>) <$> runInputGet (iopt intField "tz")
    items <- runDB $ queryItems (toSqlKey . read . unpack . snd <$> oids)
    role <- runDB $ selectOne $ do
        x :& s <- from $ table @Role `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
        where_ $ x ^. RoleId `in_` valList (toSqlKey . read . unpack . snd <$> rids)
        return (s,x)
    (fw,et) <- generateFormPost $ formTime date time tz items items role (maybeToList role)
    user <- maybeAuth
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgAppointmentTime
        $(widgetFile "book/time/time")


postBookStaffR :: Handler Html
postBookStaffR = do
    resubmit <- lookupSession sessKeyBooking
    case resubmit of
      Nothing -> do
          addMessageI "info" MsgBookingSequenceRestarted
          redirect BookOffersR
      _ -> do
          user <- maybeAuth
          scrollY <- filter ((== "scrollY") . fst) . reqGetParams <$> getRequest
          oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
          offers <- runDB $ queryOffers (toSqlKey . read . unpack . snd <$> oids)
          roles <- runDB $ queryRoles offers
          ((fr,fw),et) <- runFormPost $ formStaff [] offers Nothing roles
          msgs <- getMessages
          case fr of
            FormSuccess (items,role) -> do
                tz <- runInputPost $ iopt textField "tz"
                redirect ( BookTimeR
                         , maybeToList (("rid",) . (\(_,Entity rid _) -> pack $ show $ fromSqlKey rid) <$> role)
                           <> ((\(_,Entity oid _) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
                           <> maybeToList (("tz",) <$> tz)
                         )
            _ -> defaultLayout $ do
                setTitleI MsgStaff
                $(widgetFile "book/staff/banner")


getBookStaffR :: Handler Html
getBookStaffR = do
    user <- maybeAuth
    scrollY <- filter ((== "scrollY") . fst) . reqGetParams <$> getRequest
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    rids <- filter ((== "rid") . fst) . reqGetParams <$> getRequest
    items <- runDB $ queryOffers (toSqlKey . read . unpack . snd <$> oids)
    roles <- runDB $ queryRoles items
    role <- runDB $ queryRole (toSqlKey . read . unpack . snd <$> rids)
    (fw,et) <- generateFormPost $ formStaff items items role roles
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "book/staff/staff")


postBookOffersR :: Handler Html
postBookOffersR = do
    scrollY <- filter ((== "scrollY") . fst) <$> getPostParams
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    user <- maybeAuth
    offers <- runDB $ queryOffers []
    ioffers <- runDB $ queryItems (toSqlKey . read . unpack . snd <$> oids)
    ((fr,fw),et) <- runFormPost $ formOffers ioffers offers
    case fr of
      FormSuccess items -> do
          setSession sessKeyBooking "BOOKING_START"
          redirect ( BookStaffR
                   , scrollY <> ((\(_,Entity oid _) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
                   )
      _ -> do
          msgs <- getMessages
          let items = ioffers
          defaultLayout $ do
              setTitleI MsgOffers
              $(widgetFile "book/offers/banner")


getBookOffersR :: Handler Html
getBookOffersR = do
    scrollY <- runInputGet (iopt textField "scrollY")
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    user <- maybeAuth
    offers <- runDB $ queryOffers []
    items <- runDB $ queryItems (toSqlKey . read . unpack . snd <$> oids)
    (fw,et) <- generateFormPost $ formOffers items offers
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgOffers
        $(widgetFile "book/offers/offers")


formCustomer :: Maybe (Entity User)
         -> Maybe Day -> Maybe TimeOfDay -> Maybe TimeZone
         -> [(Entity Service, Entity Offer)]
         -> [(Entity Service, Entity Offer)]
         -> Maybe (Entity Staff, Entity Role)
         -> [(Entity Staff, Entity Role)]
         -> Html -> MForm Handler ( FormResult ( [(Entity Service, Entity Offer)]
                                               , Maybe (Entity Staff, Entity Role)
                                               , Day, TimeOfDay, TimeZone
                                               , Entity User
                                               )
                                  , Widget
                                  )
formCustomer user day time tz items offers role roles extra = do

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
        , fsAttrs = [("hidden","hidden")]
        } day

    (timeR,timeV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } time

    (tzR,tzV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTimezone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (pack . show <$> tz)

    app <- getYesod
    langs <- languages

    let r = (,,,,,) <$> offersR <*> roleR <*> dayR <*> timeR <*> (read . unpack <$> tzR)
            <*> case user of
                  Just u -> FormSuccess u
                  Nothing -> FormFailure [renderMessage app langs MsgLoginToIdentifyCustomer]

    let w = $(widgetFile "book/customer/form")
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
                $(widgetFile "book/customer/items")
          , fieldEnctype = UrlEncoded
          }

      rolesField :: [(Entity Staff, Entity Role)]
                 -> Field Handler (Entity Staff, Entity Role)
      rolesField options = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) | T.null x -> Right Nothing
                    | otherwise -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) options
              _ -> Right Nothing
          , fieldView = \theId name attrs eval _isReq -> do
                let isChecked (Left _) _ = False
                    isChecked (Right y) x = x == y
                $(widgetFile "book/customer/empls")
          , fieldEnctype = UrlEncoded
          }


formTime :: Maybe Day -> Maybe TimeOfDay -> Maybe TimeZone
         -> [(Entity Service, Entity Offer)]
         -> [(Entity Service, Entity Offer)]
         -> Maybe (Entity Staff, Entity Role)
         -> [(Entity Staff, Entity Role)]
         -> Html
         -> MForm Handler ( FormResult ( [(Entity Service, Entity Offer)]
                                       , Maybe (Entity Staff, Entity Role)
                                       , Day, TimeOfDay, TimeZone
                                       )
                           , Widget
                           )
formTime day time tz items offers role roles extra = do

    (offersR,offersV) <- mreq (check notNull (offersField offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (Just items)

    (roleR,roleV) <- mopt (rolesField roles) FieldSettings
        { fsLabel = SomeMessage MsgRole
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (Just role)

    (dayR,dayV) <- mreq futureDayField FieldSettings
        { fsLabel = SomeMessage MsgDay
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } day

    (tzR,tzV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgTimezone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (timeZoneMinutes <$> tz)

    (timeR,timeV) <- mreq (futureTimeField dayR (minutesToTimeZone <$> tzR)) FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } time

    return ( (,,,,) <$> offersR <*> roleR <*> dayR <*> timeR <*> (minutesToTimeZone <$> tzR)
           , $(widgetFile "book/time/form")
           )
  where

      futureTimeField dayR tzR = checkM (futureTime dayR tzR) timeField

      futureTime :: FormResult Day -> FormResult TimeZone -> TimeOfDay -> Handler (Either AppMessage TimeOfDay)
      futureTime dayR tzR t = do
          now <- liftIO getCurrentTime
          return $ case (dayR,tzR) of
            (FormSuccess d, FormSuccess z) -> if LocalTime d t < utcToLocalTime z now
                then Left MsgAppointmentTimeIsInThePast
                else Right t
            _ -> Right t


      futureDayField = checkM futureDay dayField

      futureDay :: Day -> Handler (Either AppMessage Day)
      futureDay d = do
          today <- liftIO $ utctDay <$> getCurrentTime
          return $ if d < today
              then Left MsgAppointmentDayIsInThePast
              else Right d

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
                $(widgetFile "book/time/items")
          , fieldEnctype = UrlEncoded
          }

      rolesField :: [(Entity Staff, Entity Role)]
                 -> Field Handler (Entity Staff, Entity Role)
      rolesField options = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) | T.null x -> Right Nothing
                    | otherwise -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) options
              _ -> Right Nothing
          , fieldView = \theId name attrs eval _isReq -> do
                let isChecked (Left _) _ = False
                    isChecked (Right y) x = x == y
                $(widgetFile "book/time/empls")
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
                $(widgetFile "book/staff/items")
          , fieldEnctype = UrlEncoded
          }

      rolesField :: [(Entity Staff, Entity Role)]
                 -> Field Handler (Entity Staff, Entity Role)
      rolesField options = Field
          { fieldParse = \xs _ -> return $ case xs of
              (x:_) | T.null x -> Right Nothing
                    | otherwise -> (Right . LS.head . filter (\(_, Entity rid _) -> rid == toSqlKey (read $ unpack x))) options
              _ -> Right Nothing
          , fieldView = \theId name attrs eval _isReq -> do
                let isChecked (Left _) _ = False
                    isChecked (Right y) x = x == y
                $(widgetFile "book/staff/empls")
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

      offersField :: [(Entity Service, Entity Offer)] -> Field Handler [(Entity Service, Entity Offer)]
      offersField options = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\(_, Entity oid _) -> oid `elem` (toSqlKey . read . unpack <$> xs))) options
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = x `elem` xs
                $(widgetFile "book/offers/items")
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
    x :& e <- from $ table @Role `innerJoin` table @Staff `on` (\(x :& e) -> x ^. RoleStaff ==. e ^. StaffId)

    where_ $ not_ $ exists $ do
        _ <- from (
            ( do
                  r <- from $ table @Role
                  where_ $ r ^. RoleService `in_` valList ((\(Entity sid _,_) -> sid) <$> services)
                  return (r ^. RoleService)
            )
            `except_`
            ( do
                  r <- from $ table @Role
                  where_ $ r ^. RoleStaff ==. e ^. StaffId
                  return (r ^. RoleService)
            ) )
        return ()

    where_ $ not_ $ exists $ do
        s <- from $ table @Service
        where_ $ s ^. ServiceId `in_` valList ((\(Entity sid _,_) -> sid) <$> services)
        where_ $ not_ $ s ^. ServiceId `in_` subSelectList ( do
                                                                 r <- from $ table @Role
                                                                 return $ r ^. RoleService
                                                           )

    unless (null services) $ where_ $ x ^. RoleService `in_` valList ((\(Entity sid _,_) -> sid) <$> services)
    orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
    return (e,x)


queryItems :: [OfferId] -> ReaderT SqlBackend Handler [(Entity Service, Entity Offer)]
queryItems oids = select $ do
    x :& o <- from $ table @Service `innerJoin` table @Offer
        `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
    where_ $ x ^. ServicePublished
    when (null oids) $ where_ $ val False
    unless (null oids) $ where_ $ o ^. OfferId `in_` valList oids
    orderBy [asc (x ^. ServiceName), asc (o ^. OfferId)]
    return (x,o)


queryOffers :: [OfferId] -> ReaderT SqlBackend Handler [(Entity Service, Entity Offer)]
queryOffers oids = select $ do
    x :& o <- from $ table @Service `innerJoin` table @Offer
        `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
    where_ $ x ^. ServicePublished
    unless (null oids) $ where_ $ o ^. OfferId `in_` valList oids
    orderBy [asc (x ^. ServiceName), asc (o ^. OfferId)]
    return (x,o)


amount :: [(Entity Service, Entity Offer)] -> [(Entity Service, Entity Offer)] -> Centi
amount oids xs = sum (
    (\(_,Entity _ (Offer _ _ price _ _ _)) -> price)
      <$> filter (`elem` oids) xs
    )


sessKeyBooking :: Text
sessKeyBooking = "BOOKING_APPOINTMENT"


range :: Enum a => a -> a -> [a]
range a b = [a..b]


fragment :: [(Text,Text)] -> [(Text,Text)] -> Text
fragment xs oids = case (xs,oids) of
  ([],(_,x):_) -> "#" <> x
  _ -> T.empty
