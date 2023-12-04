{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Billing
  ( getAdmInvoicesR
  , postAdmInvoicesR
  , getAdmInvoiceCreateR
  ) where

import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, whamlet, SomeMessage (SomeMessage) )
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions (generateFormPost, mreq, mopt)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), FieldView (fvInput)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields (textField, selectFieldList, dayField)
import Settings (widgetFile)

import Yesod.Persist (Entity (Entity, entityVal))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, where_, val
    , (^.), (==.)
    )

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR
      , ProfileR, AdminR
      )
    , AdminR (AdmInvoicesR, AdmInvoiceCreateR)
    , AppMessage
      ( MsgInvoices, MsgLogin, MsgPhoto, MsgUserProfile, MsgNavigationMenu
      , MsgNoInvoicesYet, MsgInvoice, MsgCancel, MsgSave, MsgCustomer
      , MsgEmployee, MsgInvoiceNumber, MsgStatus, MsgInvoiceDate, MsgDueDate
      )
    )

import Model
    ( UserId, StaffId, InvoiceStatus
    , User (User)
    , Invoice
      ( Invoice, invoiceNumber, invoiceStatus, invoiceDay, invoiceDueDay
      , invoiceCustomer, invoiceStaff
      )
    , EntityField (InvoiceCustomer, UserId, StaffId, InvoiceStatus)
    )

import Menu (menu)


getAdmInvoiceCreateR :: Handler Html
getAdmInvoiceCreateR = do
    (fw,et) <- generateFormPost $ formInvoice Nothing
    defaultLayout $ do
        setTitleI MsgInvoice
        $(widgetFile "admin/billing/create")


formInvoice :: Maybe (Entity Invoice) -> Html -> MForm Handler (FormResult Invoice,Widget)
formInvoice invoice extra = do
    (custR,custV) <- mreq (selectFieldList ([] :: [(AppMessage,UserId)])) FieldSettings
        { fsLabel = SomeMessage MsgCustomer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (invoiceCustomer . entityVal <$> invoice)
    (emplR,emplV) <- mreq (selectFieldList ([] :: [(AppMessage,StaffId)])) FieldSettings
        { fsLabel = SomeMessage MsgEmployee
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (invoiceStaff . entityVal <$> invoice)
    (noR,noV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgInvoiceNumber
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (invoiceNumber . entityVal <$> invoice)
    (statusR,statusV) <- mreq (selectFieldList ([] :: [(AppMessage,InvoiceStatus)])) FieldSettings
        { fsLabel = SomeMessage MsgStatus
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (invoiceStatus . entityVal <$> invoice)
    (dayR,dayV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgInvoiceDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (invoiceDay . entityVal <$> invoice)
    (dueR,dueV) <- mopt dayField FieldSettings
        { fsLabel = SomeMessage MsgDueDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (invoiceDueDay . entityVal <$> invoice)
    
    return ( Invoice <$> custR <*> emplR <*> noR <*> statusR <*> dayR <*> dueR
           , [whamlet|
#{extra}
<div>
  ^{fvInput custV}
<div>
  ^{fvInput emplV}
<div>
  ^{fvInput noV}
<div>
  ^{fvInput statusV}
<div>
  ^{fvInput dayV}
<div>
  ^{fvInput dueV}
|]
           )


postAdmInvoicesR :: Handler Html
postAdmInvoicesR = undefined


getAdmInvoicesR :: Handler Html
getAdmInvoicesR = do
    user <- maybeAuth

    invoices <- runDB $ select $ from $ table @Invoice

    curr <- getCurrentRoute
    fabAddInvoice <- newIdent
    defaultLayout $ do
        setTitleI MsgInvoices
        $(widgetFile "admin/billing/invoices")
