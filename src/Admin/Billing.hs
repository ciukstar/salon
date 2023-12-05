{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Admin.Billing
  ( getAdmInvoicesR
  , postAdmInvoicesR
  , getAdmInvoiceCreateR
  , getAdmInvoiceR
  , getAdmInvoiceEditR
  , postAdmInvoiceR
  , postAdmInvoiceDeleteR
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, SomeMessage (SomeMessage)
    , MonadHandler (liftHandler), redirect, addMessageI, getMessages
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost, checkM)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput, fvErrors, fvLabel, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , Field
    )
import Yesod.Form.Fields (textField, dayField, hiddenField, unTextarea, intField)
import Settings (widgetFile)

import Yesod.Persist (Entity (Entity, entityVal), PersistStoreWrite (insert_))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on
    , (==.), (^.), (:&)((:&))
    , orderBy, asc, fromSqlKey, selectOne, val, isNothing_, not_, just, Value (unValue), max_, desc
    )

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR
      , ProfileR, AdminR
      )
    , AdminR
      ( AdmInvoicesR, AdmInvoiceCreateR, AdmStaffPhotoR, AdmInvoiceR
      , AdmInvoiceDeleteR, AdmInvoiceEditR
      )
    , AppMessage
      ( MsgInvoices, MsgLogin, MsgPhoto, MsgUserProfile, MsgNavigationMenu
      , MsgNoInvoicesYet, MsgInvoice, MsgCancel, MsgSave, MsgCustomer
      , MsgEmployee, MsgInvoiceNumber, MsgStatus, MsgInvoiceDate, MsgDueDate
      , MsgBack, MsgPaid, MsgDraft, MsgOpen, MsgVoid, MsgUncollectible
      , MsgRecordAdded, MsgYesDelete, MsgDeleteAreYouSure, MsgPleaseConfirm
      , MsgEdit, MsgDel, MsgBillTo, MsgBilledFrom, MsgInvoiceAlreadyInTheList
      , MsgNumberSign
      )
    )

import Model
    ( InvoiceStatus
      ( InvoiceStatusDraft, InvoiceStatusOpen, InvoiceStatusPaid
      , InvoiceStatusUncollectible, InvoiceStatusVoid
      )
    , User (User, userName, userFullName, userEmail)
    , Invoice
      ( Invoice, invoiceNumber, invoiceStatus, invoiceDay, invoiceDueDay
      , invoiceCustomer, invoiceStaff
      )
    , EntityField
      ( UserFullName, StaffName, InvoiceId, InvoiceCustomer, UserId, InvoiceStaff
      , StaffId, StaffUser, InvoiceNumber
      )
    , Staff (Staff, staffName), InvoiceId, Business (Business)
    )

import Menu (menu)


postAdmInvoiceDeleteR :: InvoiceId -> Handler Html
postAdmInvoiceDeleteR iid = undefined


getAdmInvoiceEditR :: InvoiceId -> Handler Html
getAdmInvoiceEditR iid = undefined


postAdmInvoiceR :: InvoiceId -> Handler Html
postAdmInvoiceR iid = undefined


getAdmInvoiceR :: InvoiceId -> Handler Html
getAdmInvoiceR iid = do

    business <- runDB $ selectOne $ from $ table @Business
    
    invoice <- runDB $ selectOne $ do
        x :& c :& e <- from $ table @Invoice
            `innerJoin` table @User `on` (\(x :& c) -> x ^. InvoiceCustomer ==. c ^. UserId)
            `innerJoin` table @Staff `on` (\(x :& _ :& e) -> x ^. InvoiceStaff ==. e ^. StaffId)
        where_ $ x ^. InvoiceId ==. val iid
        return (x,c,e)

    dlgInvoiceDelete <- newIdent
    defaultLayout $ do
        setTitleI MsgInvoice
        $(widgetFile "admin/billing/invoice")


getAdmInvoiceCreateR :: Handler Html
getAdmInvoiceCreateR = do
    (fw,et) <- generateFormPost $ formInvoice Nothing
    defaultLayout $ do
        setTitleI MsgInvoice
        $(widgetFile "admin/billing/create")


postAdmInvoicesR :: Handler Html
postAdmInvoicesR = do
    ((fr,fw),et) <- runFormPost $ formInvoice Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI info MsgRecordAdded
          redirect $ AdminR AdmInvoicesR
      _ -> defaultLayout $ do
          setTitleI MsgInvoice
          $(widgetFile "admin/billing/create")


getAdmInvoicesR :: Handler Html
getAdmInvoicesR = do
    user <- maybeAuth

    invoices <- runDB $ select $ do
        x <- from $ table @Invoice
        orderBy [desc (x ^. InvoiceNumber)]
        return x

    fabAddInvoice <- newIdent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgInvoices
        $(widgetFile "admin/billing/invoices")


formInvoice :: Maybe (Entity Invoice) -> Html -> MForm Handler (FormResult Invoice,Widget)
formInvoice invoice extra = do
    (custR,custV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgCustomer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (invoiceCustomer . entityVal <$> invoice)

    emplId <- liftHandler $ do
        user <- maybeAuth
        case user of
          Just (Entity uid _) -> (unValue <$>) <$> runDB ( selectOne $ do
              x <- from $ table @Staff
              where_ $ not_ $ isNothing_ (x ^. StaffUser)
              where_ $ x ^. StaffUser ==. just (val uid)
              return $ x ^. StaffId )
          _ -> return Nothing
        
    (emplR,emplV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } ( (invoiceStaff . entityVal <$> invoice) <|> emplId)

    nextNumber <- liftHandler nextInvoiceNumber
        
    (noR,noV) <- mreq uniqueInvoiceNumberField FieldSettings
        { fsLabel = SomeMessage MsgInvoiceNumber
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((invoiceNumber . entityVal <$> invoice) <|> Just nextNumber)
        
    (statusR,statusV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgStatus
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } ((show . invoiceStatus . entityVal <$> invoice) <|> Just (show InvoiceStatusDraft))

    today <- liftIO $ utctDay <$> getCurrentTime
        
    (dayR,dayV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgInvoiceDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (Just (maybe today (invoiceDay . entityVal) invoice))
        
    (dueR,dueV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgDueDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (invoiceDueDay . entityVal <$> invoice)

    customers <- liftHandler $ runDB $ select $ do
        x <- from $ table @User
        orderBy [desc (x ^. UserId)]
        return x

    employees <- liftHandler $ runDB $ select $ do
        x <- from $ table @Staff
        orderBy [asc (x ^. StaffName)]
        return x

    return ( Invoice <$> custR <*> emplR <*> noR <*> (read <$> statusR) <*> dayR <*> dueR
           , $(widgetFile "admin/billing/form")
           )
  where
      statuses = [ (InvoiceStatusDraft, MsgDraft)
                 , (InvoiceStatusOpen, MsgOpen)
                 , (InvoiceStatusPaid, MsgPaid)
                 , (InvoiceStatusVoid, MsgVoid)
                 , (InvoiceStatusUncollectible, MsgUncollectible)
                 ]
                 
      uniqueInvoiceNumberField :: Field Handler Integer
      uniqueInvoiceNumberField = checkM uniqueInvoiceNumber intField

      uniqueInvoiceNumber :: Integer -> Handler (Either AppMessage Integer)
      uniqueInvoiceNumber no = do
          mx <- runDB $ selectOne $ do
              x <- from $ table @Invoice
              where_ $ x ^. InvoiceNumber ==. val no
              return x
          return $ case mx of
            Nothing -> Right no
            Just (Entity iid _) -> case invoice of
              Nothing -> Left MsgInvoiceAlreadyInTheList
              Just (Entity iid' _) | iid == iid' -> Right no
                                   | otherwise -> Left MsgInvoiceAlreadyInTheList


nextInvoiceNumber :: Handler Integer
nextInvoiceNumber = do
    n <- (unValue =<<) <$> runDB ( selectOne $ do
        x <- from $ table @Invoice
        return $ max_ (x ^. InvoiceNumber) )
    return $ maybe 1 (+1) n
        


info :: Text
info = "info"
