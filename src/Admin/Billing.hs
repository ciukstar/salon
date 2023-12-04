{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Admin.Billing
  ( getAdmInvoicesR
  , postAdmInvoicesR
  , getAdmInvoiceCreateR
  ) where

import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, SomeMessage (SomeMessage)
    , MonadHandler (liftHandler)
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions (generateFormPost, mreq, mopt)
import Yesod.Form.Types
    ( MForm, FormResult, FieldView (fvInput, fvErrors, fvLabel, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields (textField, dayField, hiddenField)
import Settings (widgetFile)

import Yesod.Persist (Entity (Entity, entityVal))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table
    , (^.), orderBy, asc, fromSqlKey
    )

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR
      , ProfileR, AdminR
      )
    , AdminR (AdmInvoicesR, AdmInvoiceCreateR, AdmStaffPhotoR)
    , AppMessage
      ( MsgInvoices, MsgLogin, MsgPhoto, MsgUserProfile, MsgNavigationMenu
      , MsgNoInvoicesYet, MsgInvoice, MsgCancel, MsgSave, MsgCustomer
      , MsgEmployee, MsgInvoiceNumber, MsgStatus, MsgInvoiceDate, MsgDueDate
      , MsgBack, MsgPaid, MsgDraft, MsgOpen, MsgVoid, MsgUncollectible
      )
    )

import Model
    ( InvoiceStatus
      ( InvoiceStatusDraft, InvoiceStatusOpen, InvoiceStatusPaid
      , InvoiceStatusUncollectible, InvoiceStatusVoid
      )
    , User (User)
    , Invoice
      ( Invoice, invoiceNumber, invoiceStatus, invoiceDay, invoiceDueDay
      , invoiceCustomer, invoiceStaff
      )
    , EntityField
      ( UserFullName, StaffName)
    , Staff (Staff)
    )

import Menu (menu)
import Data.Maybe (isJust)


getAdmInvoiceCreateR :: Handler Html
getAdmInvoiceCreateR = do
    (fw,et) <- generateFormPost $ formInvoice Nothing
    defaultLayout $ do
        setTitleI MsgInvoice
        $(widgetFile "admin/billing/create")


formInvoice :: Maybe (Entity Invoice) -> Html -> MForm Handler (FormResult Invoice,Widget)
formInvoice invoice extra = do    
    (custR,custV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgCustomer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (invoiceCustomer . entityVal <$> invoice)
    (emplR,emplV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (invoiceStaff . entityVal <$> invoice)
    (noR,noV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgInvoiceNumber
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (invoiceNumber . entityVal <$> invoice)
    (statusR,statusV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgStatus
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (show . invoiceStatus . entityVal <$> invoice)
    (dayR,dayV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgInvoiceDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (invoiceDay . entityVal <$> invoice)
    (dueR,dueV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgDueDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (invoiceDueDay . entityVal <$> invoice)

    customers <- liftHandler $ runDB $ select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserFullName)]
        return x

    employees <- liftHandler $ runDB $ select $ do
        x <- from $ table @Staff
        orderBy [asc (x ^. StaffName)]
        return x
    
    return ( Invoice <$> custR <*> emplR <*> noR <*> (read <$> statusR) <*> dayR <*> dueR
           , $(widgetFile "admin/billing/form")
           )
  where
      statuses = [ (InvoiceStatusDraft,MsgDraft)
                 , (InvoiceStatusOpen,MsgOpen)
                 , (InvoiceStatusPaid,MsgPaid)
                 , (InvoiceStatusVoid,MsgVoid)
                 , (InvoiceStatusUncollectible,MsgUncollectible)
                 ]


postAdmInvoicesR :: Handler Html
postAdmInvoicesR = undefined


getAdmInvoicesR :: Handler Html
getAdmInvoicesR = do
    user <- maybeAuth

    invoices <- runDB $ select $ from $ table @Invoice

    fabAddInvoice <- newIdent
    defaultLayout $ do
        setTitleI MsgInvoices
        $(widgetFile "admin/billing/invoices")
