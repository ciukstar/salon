{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Billing
  ( getInvoicesR
  , getBillingR
  ) where

import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core (Yesod(defaultLayout), newIdent)
import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Core.Widget (setTitleI)
import Settings (widgetFile)

import Yesod.Persist (Entity (Entity))
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    (select, from, table, where_, val
    , (^.), (==.)
    )

import Foundation
    ( Handler
    , Route
      ( BillingR, InvoicesR, AuthR, PhotoPlaceholderR, AccountPhotoR
      , ProfileR
      )
    , AppMessage
      ( MsgBilling, MsgInvoices, MsgLogin, MsgPhoto, MsgUserProfile, MsgSettings
      , MsgNavigationMenu, MsgNoInvoicesYet, MsgNoPaymentSettingsYet, MsgPaymentSettings
      )
    )

import Model
    ( UserId, Billing (Billing), Invoice (Invoice)
    , EntityField (BillingCustomer, InvoiceCustomer)
    )

import Menu (menu)

getBillingR :: UserId -> Handler Html
getBillingR uid = do
    user <- maybeAuth

    accounts <- runDB $ select $ do
        x <- from $ table @Billing
        where_ $ x ^. BillingCustomer ==. val uid
        return x

    curr <- getCurrentRoute
    fabAddAccount <- newIdent
    defaultLayout $ do
        setTitleI MsgPaymentSettings
        $(widgetFile "billing/billing")


getInvoicesR :: UserId -> Handler Html
getInvoicesR uid = do
    user <- maybeAuth

    invoices <- runDB $ select $ do
        x <- from $ table @Invoice
        where_ $ x ^. InvoiceCustomer ==. val uid
        return x

    curr <- getCurrentRoute
    defaultLayout $ do
        setTitleI MsgInvoices
        $(widgetFile "billing/invoices")
