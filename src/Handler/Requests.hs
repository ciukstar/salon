{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Requests
  ( getRequestsR
  , getRequestR
  , getRequestsSearchR
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (intercalate, unpack, Text, pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.I18N (renderMessage)
import Yesod.Core
    ( Yesod(defaultLayout), setUltDestCurrent, getMessages
    , getYesod, languages, preEscapedToMarkup, whamlet, getRequest
    , YesodRequest (reqGetParams), newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Auth ( Route(LoginR, LogoutR), maybeAuth )
import Yesod.Form.Fields (Textarea(unTextarea, Textarea), searchField, intField)
import Yesod.Form.Types (MForm, FormResult (FormSuccess))
import Yesod.Form.Functions (generateFormPost)
import Yesod.Form.Input (runInputGet, iopt)
import Settings (widgetFile)

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR, RequestsR, RequestR
      , AppointmentCancelR, AppointmentHistR, AppointmentRescheduleR, AdminR
      , ServiceThumbnailR, RequestsSearchR, RequestsSearchR
      )
    , AdminR (AdmStaffPhotoR)
    , AppMessage
      ( MsgRequests, MsgPhoto, MsgLogin, MsgLogout, MsgSymbolHour
      , MsgSymbolMinute, MsgAwaitingApproval, MsgApproved, MsgCancelled
      , MsgPaid, MsgLoginToSeeTheRequests, MsgRequest, MsgStatus, MsgAssignee
      , MsgYes, MsgNo, MsgCancelAppointmentReally, MsgPleaseConfirm, MsgHistory
      , MsgReschedule, MsgMeetingLocation, MsgCustomer, MsgSelect, MsgCancel
      , MsgService, MsgMeetingTime, MsgAcquaintance, MsgAppoinmentStatus
      , MsgDuration, MsgApprove, MsgNoPendingRequestsYet, MsgShowAll
      , MsgAssignedToMe, MsgWithoutAssignee, MsgSearch, MsgNoRequestsFound
      , MsgFromCoworkers
      )
    )

import Database.Persist (Entity (Entity))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, innerJoin, leftJoin, on, where_, val
    , (:&)((:&)), (==.), (^.), (?.), (%), (++.), (||.), (&&.)
    , orderBy, desc, just, selectOne, valList, in_, upper_, like, isNothing_, not_
    )
    
import Model
    ( Book (Book), BookId, Offer, Service (Service), Role (Role), Staff (Staff)
    , Offer (Offer), Thumbnail (Thumbnail), User (User), Contents (Contents)
    , BookStatus
      ( BookStatusRequest, BookStatusApproved, BookStatusCancelled
      , BookStatusPaid
      )
    , Assignees (AssigneesMe, AssigneesNone, AssigneesOthers)
    , EntityField
      ( BookOffer, OfferId, OfferService, ServiceId, BookDay, BookTime
      , BookRole, RoleId, RoleStaff, StaffId, StaffUser, ContentsSection, BookId
      , ThumbnailService, BookUser, UserId, BookStatus, ServiceName, ServiceDescr
      , ServiceOverview, RoleName, OfferName, OfferPrefix, OfferSuffix, OfferDescr
      , StaffName, StaffPhone, StaffMobile, StaffEmail, UserName, UserFullName
      , UserEmail
      )
    )

import Menu (menu)
import Handler.Contacts (section)


getRequestsSearchR :: Handler Html
getRequestsSearchR = do
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    case user of
      Nothing -> defaultLayout $ do
          setTitleI MsgLogin
          $(widgetFile "requests/login")
      Just (Entity uid _) -> do
          formSearch <- newIdent
          dlgStatusList <- newIdent
          q <- runInputGet $ iopt (searchField True) "q"
          stati <- filter ((== "status") . fst) . reqGetParams <$> getRequest
          let states = mapMaybe (readMaybe . unpack . snd) stati
          owners <- filter ((== "assignee") . fst) . reqGetParams <$> getRequest
          let assignees = mapMaybe (readMaybe . unpack . snd) owners
          dlgAssignee <- newIdent
          requests <- runDB $ select $ do
              x :& o :& s :& r :& e :& c <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
                  `leftJoin` table @Role `on` (\(x :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
                  `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
                  `leftJoin` table @User `on` (\(_ :& _ :& _ :& _ :& e :& c) -> e ?. StaffUser ==. just (c ?. UserId))
              
              case q of
                Just query -> where_ $ (upper_ (s ^. ServiceName) `like` ((%) ++. upper_ (val query) ++. (%)))
                  ||. (upper_ (s ^. ServiceOverview) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (s ^. ServiceDescr) `like` ((%) ++. upper_ (just (val (Textarea query))) ++. (%)))
                  ||. (upper_ (r ?. RoleName) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (o ^. OfferName) `like` ((%) ++. upper_ (val query) ++. (%)))
                  ||. (upper_ (o ^. OfferPrefix) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (o ^. OfferSuffix) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (o ^. OfferDescr) `like` ((%) ++. upper_ (just (val (Textarea query))) ++. (%)))
                  ||. (upper_ (e ?. StaffName) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (e ?. StaffPhone) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                  ||. (upper_ (e ?. StaffMobile) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                  ||. (upper_ (e ?. StaffEmail) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                  ||. (upper_ (c ?. UserName) `like` ((%) ++. upper_ (just (val query)) ++. (%)))
                  ||. (upper_ (c ?. UserFullName) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                  ||. (upper_ (c ?. UserEmail) `like` ((%) ++. upper_ (just (just (val query))) ++. (%)))
                Nothing -> return ()
              
              case states of
                [] -> return ()
                xs -> where_ $ x ^. BookStatus `in_` valList xs

              let ors = [ ( AssigneesMe `elem` assignees
                          , e ?. StaffUser ==. just (just (val uid))
                          )
                        , ( AssigneesNone `elem` assignees
                          , isNothing_ $ e ?. StaffUser
                          )
                        , ( AssigneesOthers `elem` assignees
                          , not_ (isNothing_ $ e ?. StaffUser) &&. not_ (e ?. StaffUser ==. just (just (val uid)))
                          )
                        ]
                
              case snd <$> filter fst ors of
                [] -> return ()
                xs -> where_ $ foldr (||.) (val False) xs
                
              orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
              return (x,s)
          defaultLayout $ do
              setTitleI MsgSearch
              $(widgetFile "requests/search/search")


getRequestR :: BookId -> Handler Html
getRequestR bid = do
    stati <- reqGetParams <$> getRequest
    app <- getYesod
    langs <- languages
    location <- runDB $ selectOne $ do
        x <- from $ table @Contents
        where_ $ x ^. ContentsSection ==. val section
        return x
    request <- runDB $ selectOne $ do
        x :& o :& s :& t :& r :& e :& c <- from $ table @Book
            `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
            `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& _ :& s :& t) -> just (s ^. ServiceId) ==. t ?. ThumbnailService)
            `leftJoin` table @Role `on` (\(x :& _ :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
            `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
            `innerJoin` table @User `on` (\(x :& _ :& _ :& _ :& _ :& _ :& c) -> x ^. BookUser ==. c ^. UserId)
        where_ $ x ^. BookId ==. val bid
        return (x,o,s,t,r,e,c)
    msgs <- getMessages
    (fw,et) <- generateFormPost formCancel
    defaultLayout $ do
        setTitleI MsgRequest
        $(widgetFile "requests/request")


formCancel :: Html -> MForm Handler (FormResult (), Widget)
formCancel extra = return (FormSuccess (),[whamlet|#{extra}|])


getRequestsR :: Handler Html
getRequestsR = do
    mbid <- (toSqlKey <$>) <$> runInputGet (iopt intField "bid")
    stati <- filter ((== "status") . fst) . reqGetParams <$> getRequest
    user <- maybeAuth
    setUltDestCurrent
    msgs <- getMessages
    case user of
      Nothing -> defaultLayout $ do
          setTitleI MsgLogin
          $(widgetFile "requests/login")
      Just (Entity uid _) -> do
          formSearch <- newIdent
          dlgStatusList <- newIdent
          let states = mapMaybe (readMaybe . unpack . snd) stati
          owners <- filter ((== "assignee") . fst) . reqGetParams <$> getRequest
          let assignees = mapMaybe (readMaybe . unpack . snd) owners
          dlgAssignee <- newIdent
          requests <- runDB $ select $ do
              x :& _ :& s :& _ :& e <- from $ table @Book
                  `innerJoin` table @Offer `on` (\(x :& o) -> x ^. BookOffer ==. o ^. OfferId)
                  `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
                  `leftJoin` table @Role `on` (\(x :& _ :& _ :& r) -> x ^. BookRole ==. r ?. RoleId)
                  `leftJoin` table @Staff `on` (\(_ :& _ :& _ :& r :& e) -> r ?. RoleStaff ==. e ?. StaffId)
                            
              case states of
                [] -> return ()
                xs -> where_ $ x ^. BookStatus `in_` valList xs

              let ors = [ ( AssigneesMe `elem` assignees
                          , e ?. StaffUser ==. just (just (val uid))
                          )
                        , ( AssigneesNone `elem` assignees
                          , isNothing_ $ e ?. StaffUser
                          )
                        , ( AssigneesOthers `elem` assignees
                          , not_ (isNothing_ $ e ?. StaffUser) &&. not_ (e ?. StaffUser ==. just (just (val uid)))
                          )
                        ]
                
              case snd <$> filter fst ors of
                [] -> return ()
                xs -> where_ $ foldr (||.) (val False) xs
                
              orderBy [desc (x ^. BookDay), desc (x ^. BookTime)]
              return (x,s)
          defaultLayout $ do
              setTitleI MsgRequests
              $(widgetFile "requests/requests")


statusList :: [(BookStatus, AppMessage)]
statusList = [ (BookStatusRequest, MsgRequest)
             , (BookStatusApproved, MsgApproved)
             , (BookStatusCancelled, MsgCancelled)
             , (BookStatusPaid, MsgPaid)
             ]

assigneeList :: [(Assignees, AppMessage)]
assigneeList = [ (AssigneesMe,MsgAssignedToMe)
               , (AssigneesNone,MsgWithoutAssignee)
               , (AssigneesOthers,MsgFromCoworkers)
               ]


resolve :: BookStatus -> (Text, Text, AppMessage)
resolve BookStatusRequest = ("orange", "hourglass_top", MsgAwaitingApproval)
resolve BookStatusApproved = ("green", "verified", MsgApproved)
resolve BookStatusCancelled = ("red", "block", MsgCancelled)
resolve BookStatusPaid = ("green", "paid", MsgPaid)
