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
  , getBookSearchR
  , postBookSearchR
  , sessKeyBooking
  ) where

import Control.Monad (unless, forM, when, join)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Bifunctor (Bifunctor(second))
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
    , setSession, MonadIO (liftIO), getPostParams, newIdent
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
import Yesod.Form.Fields
    ( Textarea (Textarea, unTextarea), timeField, dayField, textField, intField
    , searchField, textareaField
    )
import Yesod.Form.Functions
    ( mreq, check, mopt, runFormPost, generateFormPost, checkM)
import Yesod.Form.Input (iopt, runInputGet)
import Settings (widgetFile)

import Yesod.Persist.Core (runDB)
import Database.Persist ( Entity(Entity, entityVal), PersistStoreWrite (insert, insert_) )
import Database.Persist.Sql ( fromSqlKey, toSqlKey, SqlBackend )

import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on, upper_, like
    , (^.), (?.), (||.), (==.), (:&)((:&)), (++.), (%)
    , orderBy, asc, desc, in_, valList, leftJoin, val
    , selectOne, exists, not_, except_, subSelectList, just
    , Value (unValue), justList
    )

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, AdminR, AccountR
      , HomeR, AppointmentR, ProfileR, ServiceThumbnailR
      , BookOffersR, BookStaffR, BookTimeR, BookCustomerR, BookEndR
      , BookSearchR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgBook, MsgPhoto, MsgChooseServicesToBook, MsgServices
      , MsgSymbolHour, MsgSymbolMinute, MsgStaff, MsgNoPreference
      , MsgMaximumAvailability, MsgSelectStaff, MsgInvalidFormData
      , MsgSelectAtLeastOneServicePlease, MsgOffer, MsgAppointmentTime
      , MsgRole, MsgSignUp, MsgSignIn, MsgOffers, MsgCustomerInformation
      , MsgCustomer, MsgStepNofM, MsgContinue, MsgNotYourAccount
      , MsgLogin, MsgDay, MsgTime , MsgEnd, MsgAlreadyHaveAnAccount
      , MsgLoginToIdentifyCustomer, MsgRecordAdded, MsgShowDetails
      , MsgYouSuccessfullyCreatedYourBooking, MsgBookNewAppointment
      , MsgBookingSequenceRestarted, MsgThumbnail, MsgAppointmentDayIsInThePast
      , MsgAppointmentTimeIsInThePast, MsgTimeZone, MsgTimeZoneOffset
      , MsgMinutes, MsgSearch, MsgAddress, MsgCategory, MsgSelect, MsgCancel
      , MsgCategories, MsgNoServicesFound, MsgLocation
      )
    )

import Model
    ( Service(Service), ServiceId, Offer (Offer), OfferId, Staff (Staff)
    , Role (Role), RoleId
    , User (User), Book (Book), Thumbnail
    , BookStatus (BookStatusRequest), Hist (Hist)
    , Business (businessAddr, businessTzo, businessTz, Business)
    , EntityField
      ( StaffId, RoleId, ServiceId, OfferService, ServicePublished, BookId
      , ServiceName, RoleStaff, RoleRating, RoleService, OfferId, BookOffer
      , BookUser, BookRole, ThumbnailService, ThumbnailAttribution
      , ServiceOverview, ServiceDescr, ServiceGroup
      )
    )

import Menu (menu)


postBookSearchR :: Handler Html
postBookSearchR = do
    formSearch <- newIdent
    mq <- runInputGet $ iopt (searchField True) "q"
    y <- filter ((== "y") . fst) <$> getPostParams
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    categs <- (toSqlKey . read . unpack . snd <$>) . filter ((== "categ") . fst) . reqGetParams <$> getRequest
    offers <- runDB $ queryOffers categs mq []
    ioffers <- runDB $ queryItems categs mq (toSqlKey . read . unpack . snd <$> oids)
    ((fr,fw),et) <- runFormPost $ formOffers ioffers offers
    case fr of
      FormSuccess items -> do
          setSession sessKeyBooking "BOOKING_START"
          redirect ( BookStaffR
                   , y <> ((\((_,Entity oid _),_) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
                   )
      _ -> do
          msgs <- getMessages
          let items = ioffers
          defaultLayout $ do
              setTitleI MsgSearch
              $(widgetFile "book/search/banner")


getBookSearchR :: Handler Html
getBookSearchR = do
    formSearch <- newIdent
    dlgCategList <- newIdent
    mq <- runInputGet $ iopt (searchField True) "q"
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    categs <- (toSqlKey . read . unpack . snd <$>) . filter ((== "categ") . fst) . reqGetParams <$> getRequest
    offers <- runDB $ queryOffers categs mq []
    items <- runDB $ queryItems categs mq (toSqlKey . read . unpack . snd <$> oids)
    (fw,et) <- generateFormPost $ formOffers items offers

    groups <- runDB $ select $ do
        x <- from $ table @Service
        where_ $ x ^. ServicePublished
        where_ $ exists $ do
            y <- from $ table @Service
            where_ $ y ^. ServiceGroup ==. just (x ^. ServiceId)
            where_ $ x ^. ServicePublished
        return x

    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgSearch
        $(widgetFile "book/search/search")


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
        where_ $ s ^. ServicePublished
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
          user <- maybeAuth
          ioffers <- runDB $ queryOffers [] Nothing (toSqlKey . read . unpack . snd <$> oids)
          irole <- runDB $ queryRole (toSqlKey . read . unpack . snd <$> LS.head rids)
          ((fr,fw),et) <- runFormPost $ formCustomer
              user Nothing Nothing Nothing ioffers ioffers irole (maybeToList irole)
          case fr of
            FormSuccess (items,role,day,time,addr,tzo,tz,Entity uid _) -> do
                bids <- forM items $ \((_,Entity oid _),_) -> do
                    bid  <- runDB $ insert $
                        Book uid oid ((\(_,Entity rid _) -> rid) <$> role) day time addr tzo tz BookStatusRequest
                    now <- liftIO getCurrentTime
                    runDB $ insert_ $ Hist bid uid now day time addr tzo tz BookStatusRequest
                    return bid
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
    items <- runDB $ queryItems [] Nothing (toSqlKey . read . unpack . snd <$> oids)
    role <- runDB $ selectOne $ do
        x :& s <- from $ table @Role `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
        where_ $ x ^. RoleId `in_` valList (toSqlKey . read . unpack . snd <$> rids)
        return (s,x)
    business <- runDB $ selectOne $ from $ table @Business
    (fw,et) <- generateFormPost $ formCustomer
        user
        ((read . unpack . snd <$>) $ LS.head dates)
        ((read . unpack . snd <$>) $ LS.head times)
        business
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
          ioffers <- runDB $ queryOffers [] Nothing (toSqlKey . read . unpack . snd <$> oids)
          irole <- runDB $ queryRole (toSqlKey . read . unpack . snd <$> LS.head rids)
          ((fr,fw),et) <- runFormPost $ formTime
              Nothing Nothing Nothing ioffers ioffers irole (maybeToList irole)
          msgs <- getMessages
          case fr of
            FormSuccess (items,role,date,time,addr,tzo,tz) -> do
                redirect ( BookCustomerR
                         , maybeToList (("rid",) . (\(_,Entity rid _) -> pack $ show $ fromSqlKey rid) <$> role)
                           <> ((\((_,Entity oid _),_) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
                           <> [ ("date",pack $ show date)
                              , ("time",pack $ show time)
                              , ("tzo",pack $ show (timeZoneMinutes tzo))
                              , ("tz",tz)
                              , ("addr",unTextarea addr)
                              ]
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
    items <- runDB $ queryItems [] Nothing (toSqlKey . read . unpack . snd <$> oids)
    role <- runDB $ selectOne $ do
        x :& s <- from $ table @Role `innerJoin` table @Staff `on` (\(x :& s) -> x ^. RoleStaff ==. s ^. StaffId)
        where_ $ x ^. RoleId `in_` valList (toSqlKey . read . unpack . snd <$> rids)
        return (s,x)
    business <- runDB $ selectOne $ from $ table @Business
    (fw,et) <- generateFormPost $ formTime
        date time business items items role (maybeToList role)
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
          ys <- filter ((== "y") . fst) . reqGetParams <$> getRequest
          oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
          offers <- runDB $ queryOffers [] Nothing (toSqlKey . read . unpack . snd <$> oids)
          roles <- runDB $ queryRoles (fst <$> offers)
          ((fr,fw),et) <- runFormPost $ formStaff [] offers Nothing roles
          msgs <- getMessages
          case fr of
            FormSuccess (items,role) -> do
                redirect ( BookTimeR
                         , maybeToList (("rid",) . (\(_,Entity rid _) -> pack $ show $ fromSqlKey rid) <$> role)
                           <> ((\((_,Entity oid _),_) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
                         )
            _ -> defaultLayout $ do
                setTitleI MsgStaff
                $(widgetFile "book/staff/banner")


getBookStaffR :: Handler Html
getBookStaffR = do
    user <- maybeAuth
    ys <- filter ((== "y") . fst) . reqGetParams <$> getRequest
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    rids <- filter ((== "rid") . fst) . reqGetParams <$> getRequest
    items <- runDB $ queryOffers [] Nothing (toSqlKey . read . unpack . snd <$> oids)
    roles <- runDB $ queryRoles (fst <$> items)
    role <- runDB $ queryRole (toSqlKey . read . unpack . snd <$> rids)
    (fw,et) <- generateFormPost $ formStaff items items role roles
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgStaff
        $(widgetFile "book/staff/staff")


postBookOffersR :: Handler Html
postBookOffersR = do
    ys <- filter ((== "y") . fst) <$> getPostParams
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    user <- maybeAuth
    offers <- runDB $ queryOffers [] Nothing []
    ioffers <- runDB $ queryItems [] Nothing (toSqlKey . read . unpack . snd <$> oids)
    ((fr,fw),et) <- runFormPost $ formOffers ioffers offers
    case fr of
      FormSuccess items -> do
          setSession sessKeyBooking "BOOKING_START"
          redirect ( BookStaffR
                   , ys <> ((\((_,Entity oid _),_) -> ("oid",pack $ show $ fromSqlKey oid)) <$> items)
                   )
      _ -> do
          msgs <- getMessages
          let items = ioffers
          defaultLayout $ do
              setTitleI MsgOffers
              $(widgetFile "book/offers/banner")


getBookOffersR :: Handler Html
getBookOffersR = do
    y <- runInputGet (iopt textField "y")
    oids <- filter ((== "oid") . fst) . reqGetParams <$> getRequest
    user <- maybeAuth
    offers <- runDB $ queryOffers [] Nothing []
    items <- runDB $ queryItems [] Nothing (toSqlKey . read . unpack . snd <$> oids)
    (fw,et) <- generateFormPost $ formOffers items offers
    msgs <- getMessages
    setUltDestCurrent
    defaultLayout $ do
        setTitleI MsgOffers
        $(widgetFile "book/offers/offers")


formCustomer :: Maybe (Entity User)
             -> Maybe Day -> Maybe TimeOfDay -> Maybe (Entity Business)
             -> [((Entity Service, Entity Offer),Maybe Html)]
             -> [((Entity Service, Entity Offer),Maybe Html)]
             -> Maybe (Entity Staff, Entity Role)
             -> [(Entity Staff, Entity Role)]
             -> Html -> MForm Handler ( FormResult ( [((Entity Service, Entity Offer),Maybe Html)]
                                                   , Maybe (Entity Staff, Entity Role)
                                                   , Day, TimeOfDay, Textarea, TimeZone, Text
                                                   , Entity User
                                                   )
                                      , Widget
                                      )
formCustomer user day time business items offers role roles extra = do

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

    (addrR,addrV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (businessAddr . entityVal <$> business)

    (tzoR,tzoV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTimeZoneOffset
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (pack . show . businessTzo . entityVal <$> business)

    (tzR,tzV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTimeZone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("hidden","hidden")]
        } (businessTz . entityVal <$> business)

    app <- getYesod
    langs <- languages

    let r = (,,,,,,,) <$> offersR <*> roleR <*> dayR <*> timeR <*> addrR <*> (read . unpack <$> tzoR) <*> tzR
            <*> case user of
                  Just u -> FormSuccess u
                  Nothing -> FormFailure [renderMessage app langs MsgLoginToIdentifyCustomer]

    detailsAppointmentTime <- newIdent
    appointmentTime <- newIdent
    appointmentFullLongTime <- newIdent
    detailsStaff <- newIdent
    detailsServices <- newIdent
    return (r,$(widgetFile "book/customer/form"))
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [((Entity Service, Entity Offer),Maybe Html)]
                  -> Field Handler [((Entity Service, Entity Offer),Maybe Html)]
      offersField options = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\((_, Entity oid _),_) -> oid `elem` (toSqlKey . read . unpack <$> xs))) options
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let proj :: ((Entity Service,Entity Offer),Maybe Html) -> OfferId
                    proj ((_, Entity oid _),_) = oid
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = proj x `elem` (proj <$> xs)
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


formTime :: Maybe Day -> Maybe TimeOfDay -> Maybe (Entity Business)
         -> [((Entity Service, Entity Offer),Maybe Html)]
         -> [((Entity Service, Entity Offer),Maybe Html)]
         -> Maybe (Entity Staff, Entity Role)
         -> [(Entity Staff, Entity Role)]
         -> Html
         -> MForm Handler ( FormResult ( [((Entity Service, Entity Offer),Maybe Html)]
                                       , Maybe (Entity Staff, Entity Role)
                                       , Day, TimeOfDay, Textarea, TimeZone, Text
                                       )
                           , Widget
                           )
formTime day time business items offers role roles extra = do

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

    (addrR,addrV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgAddress
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessAddr . entityVal <$> business)

    (tzoR,tzoV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgTimeZoneOffset
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (timeZoneMinutes . businessTzo . entityVal <$> business)

    (tzR,tzV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTimeZone
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (businessTz . entityVal <$> business)

    (timeR,timeV) <- mreq (futureTimeField dayR (minutesToTimeZone <$> tzoR)) FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } time

    sectionDateTime <- newIdent
    detailsLocation <- newIdent
    detailsStaff <- newIdent
    detailsServices <- newIdent
    return ( (,,,,,,) <$> offersR <*> roleR <*> dayR <*> timeR <*> addrR <*> (minutesToTimeZone <$> tzoR) <*> tzR
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

      offersField :: [((Entity Service, Entity Offer),Maybe Html)]
                  -> Field Handler [((Entity Service, Entity Offer),Maybe Html)]
      offersField options = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\((_, Entity oid _),_) -> oid `elem` (toSqlKey . read . unpack <$> xs))) options
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let proj :: ((Entity Service,Entity Offer),Maybe Html) -> OfferId
                    proj ((_, Entity oid _),_) = oid
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = proj x `elem` (proj <$> xs)
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


formStaff :: [((Entity Service,Entity Offer),Maybe Html)]
          -> [((Entity Service,Entity Offer),Maybe Html)]
          -> Maybe (Entity Staff, Entity Role)
          -> [(Entity Staff, Entity Role)]
          -> Html
          -> MForm Handler ( FormResult ( [((Entity Service,Entity Offer),Maybe Html)]
                                        , Maybe (Entity Staff,Entity Role)
                                        )
                           , Widget
                           )
formStaff items offers role roles extra = do

    (offersR,offersV) <- mreq (check notNull (offersField offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just items)
        
    detailsServices <- newIdent
    (rolesR,rolesV) <- mopt (rolesField roles) "" (Just role)
    return ( (,) <$> offersR <*> rolesR
           , $(widgetFile "book/staff/form")
           )
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [((Entity Service,Entity Offer),Maybe Html)]
                  -> Field Handler [((Entity Service,Entity Offer),Maybe Html)]
      offersField options = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\((_,Entity oid _),_) -> oid `elem` (toSqlKey . read . unpack <$> xs))) options
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let proj :: ((Entity Service,Entity Offer),Maybe Html) -> OfferId
                    proj ((_, Entity oid _),_) = oid
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = proj x `elem` (proj <$> xs)
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


formOffers :: [((Entity Service, Entity Offer),Maybe Html)]
           -> [((Entity Service, Entity Offer),Maybe Html)]
           -> Html -> MForm Handler (FormResult [((Entity Service,Entity Offer),Maybe Html)], Widget)
formOffers items offers extra = do
    (r,v) <- mreq (check notNull (offersField offers)) FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just items)
    let w = [whamlet|
#{extra}
^{fvInput v}
|]
    return (r,w)
  where

      notNull xs = case xs of
        [] -> Left MsgSelectAtLeastOneServicePlease
        _ -> Right xs

      offersField :: [((Entity Service,Entity Offer),Maybe Html)]
                  -> Field Handler [((Entity Service,Entity Offer),Maybe Html)]
      offersField options = Field
          { fieldParse = \xs _ -> return $
            (Right . Just . filter (\((_, Entity oid _),_) -> oid `elem` (toSqlKey . read . unpack <$> xs))) options
          , fieldView = \theId name attrs vals _isReq -> do
                app <- getYesod
                langs <- languages
                let proj :: ((Entity Service,Entity Offer),Maybe Html) -> OfferId
                    proj ((_, Entity oid _),_) = oid
                let isChecked (Left _) _ = False
                    isChecked (Right xs) x = proj x `elem` (proj <$> xs)
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
        where_ $ s ^. ServicePublished
        where_ $ s ^. ServiceId `in_` valList ((\(Entity sid _,_) -> sid) <$> services)
        where_ $ not_ $ s ^. ServiceId `in_` subSelectList ( do
                                                                 r <- from $ table @Role
                                                                 return $ r ^. RoleService
                                                           )

    unless (null services) $ where_ $ x ^. RoleService `in_` valList ((\(Entity sid _,_) -> sid) <$> services)
    orderBy [desc (x ^. RoleRating), asc (x ^. RoleId)]
    return (e,x)


queryItems :: [ServiceId] -> Maybe Text -> [OfferId]
           -> ReaderT SqlBackend Handler [((Entity Service, Entity Offer),Maybe Html)]
queryItems categs mq oids = (second (join . unValue) <$>) <$> ( select $ do
    x :& o :& t <- from $ table @Service `innerJoin` table @Offer
        `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
        `leftJoin` table @Thumbnail `on` (\(x :& _ :& t) -> just (x ^. ServiceId) ==. t ?. ThumbnailService)
    where_ $ x ^. ServicePublished
    case mq of
      Just q -> where_ $ ( upper_ (x ^. ServiceName) `like` (%) ++. upper_ (val q) ++. (%) )
          ||. ( upper_ (x ^. ServiceOverview) `like` (%) ++. upper_ (just (val q)) ++. (%) )
          ||. ( upper_ (x ^. ServiceDescr) `like` (%) ++. upper_ (just (val (Textarea q))) ++. (%) )
      _ -> return ()
    case categs of
      [] -> return ()
      xs -> where_ $ x ^. ServiceGroup `in_` justList (valList xs)
    when (null oids) $ where_ $ val False
    unless (null oids) $ where_ $ o ^. OfferId `in_` valList oids
    orderBy [asc (x ^. ServiceName), asc (o ^. OfferId)]
    return ((x,o),t ?. ThumbnailAttribution) )


queryOffers :: [ServiceId] -> Maybe Text -> [OfferId]
            -> ReaderT SqlBackend Handler [((Entity Service, Entity Offer),Maybe Html)]
queryOffers categs mq oids = (second (join . unValue) <$>) <$> ( select $ do
    x :& o :& t <- from $ table @Service `innerJoin` table @Offer
        `on` (\(x :& o) -> x ^. ServiceId ==. o ^. OfferService)
        `leftJoin` table @Thumbnail `on` (\(x :& _ :& t) -> just (x ^. ServiceId) ==. t ?. ThumbnailService)
    where_ $ x ^. ServicePublished
    case mq of
      Just q -> where_ $ ( upper_ (x ^. ServiceName) `like` (%) ++. upper_ (val q) ++. (%) )
          ||. ( upper_ (x ^. ServiceOverview) `like` (%) ++. upper_ (just (val q)) ++. (%) )
          ||. ( upper_ (x ^. ServiceDescr) `like` (%) ++. upper_ (just (val (Textarea q))) ++. (%) )
      _ -> return ()
    case categs of
      [] -> return ()
      xs -> where_ $ x ^. ServiceGroup `in_` justList (valList xs)
    orderBy [asc (x ^. ServiceName)]
    unless (null oids) $ where_ $ o ^. OfferId `in_` valList oids
    orderBy [asc (x ^. ServiceName), asc (o ^. OfferId)]
    return ((x,o),t ?. ThumbnailAttribution) )


amount :: [((Entity Service,Entity Offer),Maybe Html)] -> Centi
amount oids = sum $ (\((_,Entity _ (Offer _ _ price _ _ _)),_) -> price) <$> oids


sessKeyBooking :: Text
sessKeyBooking = "BOOKING_APPOINTMENT"


range :: Enum a => a -> a -> [a]
range a b = [a..b]


fragment :: [(Text,Text)] -> [(Text,Text)] -> Text
fragment xs oids = case (xs,oids) of
  ([],(_,x):_) -> "#" <> x
  _ -> T.empty
