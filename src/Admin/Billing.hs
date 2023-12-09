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
  , getAdmInvoiceItemsR
  , postAdmInvoiceItemsR
  , getAdmInvoiceItemCreateR
  , postAdmInvoiceItemCreateR
  , getAdmInvoiceItemR
  , postAdmInvoiceItemR
  , getAdmInvoiceItemEditR
  , postAdmInvoiceItemEditR
  , postAdmInvoiceItemDeleteR
  ) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (Bifunctor(first, second))
import Data.Fixed (Centi)
import Data.Maybe (isJust)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Text.Hamlet (Html)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, SomeMessage (SomeMessage)
    , MonadHandler (liftHandler), redirect, addMessageI, getMessages
    , getCurrentRoute, lookupPostParam, whamlet
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions
    ( generateFormPost, mreq, mopt, runFormPost, checkM
    )
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), Field
    , FieldView (fvInput, fvErrors, fvLabel, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields
    ( dayField, hiddenField, unTextarea, intField, doubleField, textField )

import Yesod.Persist
    ( Entity (Entity, entityVal, entityKey)
    , PersistStoreWrite (insert_, replace, delete)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on, Value (unValue, Value), max_, desc
    , (==.), (^.), (:&)((:&)), (?.)
    , orderBy, asc, fromSqlKey, selectOne, val, isNothing_, not_, just, leftJoin
    , toSqlKey, subSelect, sum_, SqlExpr, coalesceDefault
    )

import Foundation
    ( Handler, Widget
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR, AdminR, StaticR
      , ServiceThumbnailR
      )
    , AdminR
      ( AdmInvoicesR, AdmInvoiceCreateR, AdmStaffPhotoR, AdmInvoiceR
      , AdmInvoiceDeleteR, AdmInvoiceEditR, AdmInvoiceItemsR
      , AdmInvoiceItemCreateR, AdmInvoiceItemCreateR, AdmInvoiceItemCreateR
      , AdmInvoiceItemR, AdmInvoiceItemEditR, AdmInvoiceItemDeleteR
      )
    , AppMessage
      ( MsgInvoices, MsgLogin, MsgPhoto, MsgUserProfile, MsgNavigationMenu
      , MsgNoInvoicesYet, MsgInvoice, MsgCancel, MsgSave, MsgCustomer
      , MsgEmployee, MsgInvoiceNumber, MsgStatus, MsgInvoiceDate, MsgDueDate
      , MsgBack, MsgPaid, MsgDraft, MsgOpen, MsgVoid, MsgUncollectible
      , MsgRecordAdded, MsgYesDelete, MsgDeleteAreYouSure, MsgPleaseConfirm
      , MsgEdit, MsgDel, MsgBillTo, MsgBilledFrom, MsgInvoiceAlreadyInTheList
      , MsgNumberSign, MsgDetails, MsgInvoiceItems, MsgNoInvoiceItemsYet
      , MsgInvoiceItem, MsgOffer, MsgQuantity, MsgPrice, MsgTax, MsgVat
      , MsgAmount, MsgCurrency, MsgThumbnail, MsgRecordDeleted, MsgActionCancelled
      , MsgRecordEdited
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
      ( StaffName, InvoiceId, InvoiceCustomer, UserId, InvoiceStaff, StaffId
      , StaffUser, InvoiceNumber, ItemId, ItemInvoice, OfferService, ServiceId
      , ServiceName, ThumbnailService, ThumbnailAttribution, BusinessCurrency
      , OfferId, ItemOffer, ItemAmount
      )
    , Staff (Staff, staffName), InvoiceId, Business (Business)
    , ItemId
    , Item
      ( Item, itemOffer, itemQuantity, itemPrice, itemTax, itemVat, itemAmount
      , itemCurrency
      )
    , Offer (Offer, offerQuantity, offerPrice), Service (Service), Thumbnail
    )

import Menu (menu)
import Settings (widgetFile)
import Settings.StaticFiles (img_photo_FILL0_wght400_GRAD0_opsz48_svg)


postAdmInvoiceItemDeleteR :: InvoiceId -> ItemId -> Handler Html
postAdmInvoiceItemDeleteR iid xid = do
    ((fr,_),_) <- runFormPost formItemDelete
    case fr of
      FormSuccess _ -> do
          runDB $ delete xid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR $ AdmInvoiceItemsR iid
      _ -> do
          addMessageI info MsgActionCancelled
          redirect $ AdminR $ AdmInvoiceItemsR iid


formItemDelete :: Html -> MForm Handler (FormResult (),Widget)
formItemDelete extra = return (pure (),[whamlet|#{extra}|])


postAdmInvoiceItemR :: InvoiceId -> ItemId -> Handler Html
postAdmInvoiceItemR iid xid = do
    (item,offer) <- do
        p <- runDB $ selectOne $ do
            x :& o <- from $ table @Item
                `innerJoin` table @Offer `on` (\(x :& o) -> x ^. ItemOffer ==. o ^. OfferId)
            where_ $ x ^. ItemId ==. val xid
            return (x,o)
        return (fst <$> p, snd <$> p)

    ((fr,fw),et) <- runFormPost $ formItemCreate iid offer item
    case fr of
      FormSuccess r -> do
          runDB $ replace xid r
          addMessageI info MsgRecordEdited
          redirect $ AdminR $ AdmInvoiceItemR iid xid
      _ -> defaultLayout $ do
          setTitleI MsgInvoiceItem
          $(widgetFile "admin/billing/items/edit")


postAdmInvoiceItemEditR :: InvoiceId -> ItemId -> Handler Html
postAdmInvoiceItemEditR iid xid = do
    moid <- (toSqlKey <$>) . (readMaybe . unpack =<<) <$> lookupPostParam "oid"
    offer <- case moid of
      Just oid -> runDB $ selectOne $ do
          x <- from $ table @Offer
          where_ $ x ^. OfferId ==. val oid
          return x
      Nothing -> return Nothing

    (fw,et) <- generateFormPost $ formItemEdit iid xid offer Nothing
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/edit")


getAdmInvoiceItemEditR :: InvoiceId -> ItemId -> Handler Html
getAdmInvoiceItemEditR iid xid = do
    (item,offer) <- do
        p <- runDB $ selectOne $ do
            x :& o <- from $ table @Item
                `innerJoin` table @Offer `on` (\(x :& o) -> x ^. ItemOffer ==. o ^. OfferId)
            where_ $ x ^. ItemId ==. val xid
            return (x,o)
        return (fst <$> p, snd <$> p)

    (fw,et) <- generateFormPost $ formItemEdit iid xid offer item
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/edit")


postAdmInvoiceItemsR :: InvoiceId -> Handler Html
postAdmInvoiceItemsR iid = do
    ((fr,fw),et) <- runFormPost $ formItemCreate iid Nothing Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI info MsgRecordAdded
          redirect $ AdminR $ AdmInvoiceItemsR iid
      _ -> defaultLayout $ do
          setTitleI MsgInvoiceItem
          $(widgetFile "admin/billing/items/create")


postAdmInvoiceItemCreateR :: InvoiceId -> Handler Html
postAdmInvoiceItemCreateR iid = do
    moid <- (toSqlKey <$>) . (readMaybe . unpack =<<) <$> lookupPostParam "oid"
    offer <- case moid of
      Just oid -> runDB $ selectOne $ do
          x <- from $ table @Offer
          where_ $ x ^. OfferId ==. val oid
          return x
      Nothing -> return Nothing

    (fw,et) <- generateFormPost $ formItemCreate iid offer Nothing
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/create")


getAdmInvoiceItemCreateR :: InvoiceId -> Handler Html
getAdmInvoiceItemCreateR iid = do
    moid <- (toSqlKey <$>) <$> runInputGet (iopt intField "oid")
    offer <- case moid of
      Just oid -> runDB $ selectOne $ do
          x <- from $ table @Offer
          where_ $ x ^. OfferId ==. val oid
          return x
      Nothing -> return Nothing

    (fw,et) <- generateFormPost $ formItemCreate iid offer Nothing
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/create")


formItemEdit :: InvoiceId -> ItemId -> Maybe (Entity Offer) -> Maybe (Entity Item)
             -> Html -> MForm Handler (FormResult Item, Widget)
formItemEdit iid xid offer item extra = do

    currency <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Business
        return $ x ^. BusinessCurrency )

    offers <- liftHandler $ (second (second (join . unValue)) <$>) <$> runDB ( select ( do
        x :& s :& t <- from $ table @Offer
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& s :& t) -> t ?. ThumbnailService ==. just (s ^. ServiceId))
        orderBy [asc (s ^. ServiceName)]
        return (x,(s,t ?. ThumbnailAttribution)) ) )

    (offerR,offerV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } ((itemOffer . entityVal <$> item) <|> (entityKey <$> offer))

    (quantityR,quantityV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgQuantity
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((itemQuantity . entityVal <$> item) <|> (offerQuantity . entityVal <$> offer) <|> pure 1)

    (priceR,priceV) <- first (realToFrac <$>) <$> mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (realToFrac <$> ((itemPrice . entityVal <$> item) <|> (offerPrice . entityVal <$> offer)))

    (taxR,taxV) <- first ((realToFrac <$>) <$>) <$> mopt doubleField FieldSettings
        { fsLabel = SomeMessage MsgTax
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (((realToFrac <$>) . itemTax . entityVal <$> item) <|> pure (pure 0))

    (vatR,vatV) <- first ((realToFrac <$>) <$>) <$> mopt doubleField FieldSettings
        { fsLabel = SomeMessage MsgVat
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (((realToFrac <$>) . itemVat . entityVal <$> item) <|> pure (pure 0))

    (amountR,amountV) <- first (realToFrac <$>) <$> mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgAmount
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (realToFrac <$> ((itemAmount . entityVal <$> item) <|> (offerPrice . entityVal <$> offer)))

    (currencyR,currencyV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgCurrency
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((itemCurrency . entityVal <$> item) <|> pure currency)

    let r = Item <$> offerR <*> pure iid <*> quantityR <*> priceR <*> taxR <*> vatR <*> amountR <*> currencyR
    let w = $(widgetFile "admin/billing/items/form-edit")
    return (r,w)


formItemCreate :: InvoiceId -> Maybe (Entity Offer) -> Maybe (Entity Item)
               -> Html -> MForm Handler (FormResult Item, Widget)
formItemCreate iid offer item extra = do

    currency <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Business
        return $ x ^. BusinessCurrency )

    offers <- liftHandler $ (second (second (join . unValue)) <$>) <$> runDB ( select ( do
        x :& s :& t <- from $ table @Offer
            `innerJoin` table @Service `on` (\(x :& s) -> x ^. OfferService ==. s ^. ServiceId)
            `leftJoin` table @Thumbnail `on` (\(_ :& s :& t) -> t ?. ThumbnailService ==. just (s ^. ServiceId))
        orderBy [asc (s ^. ServiceName)]
        return (x,(s,t ?. ThumbnailAttribution)) ) )

    (offerR,offerV) <- mreq hiddenField FieldSettings
        { fsLabel = SomeMessage MsgOffer
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } ((itemOffer . entityVal <$> item) <|> (entityKey <$> offer))

    (quantityR,quantityV) <- mreq intField FieldSettings
        { fsLabel = SomeMessage MsgQuantity
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((itemQuantity . entityVal <$> item) <|> (offerQuantity . entityVal <$> offer) <|> pure 1)

    (priceR,priceV) <- first (realToFrac <$>) <$> mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgPrice
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (realToFrac <$> ((itemPrice . entityVal <$> item) <|> (offerPrice . entityVal <$> offer)))

    (taxR,taxV) <- first ((realToFrac <$>) <$>) <$> mopt doubleField FieldSettings
        { fsLabel = SomeMessage MsgTax
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (((realToFrac <$>) . itemTax . entityVal <$> item) <|> pure (pure 0))

    (vatR,vatV) <- first ((realToFrac <$>) <$>) <$> mopt doubleField FieldSettings
        { fsLabel = SomeMessage MsgVat
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (((realToFrac <$>) . itemVat . entityVal <$> item) <|> pure (pure 0))

    (amountR,amountV) <- first (realToFrac <$>) <$> mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgAmount
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (realToFrac <$> ((itemAmount . entityVal <$> item) <|> (offerPrice . entityVal <$> offer)))

    (currencyR,currencyV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgCurrency
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((itemCurrency . entityVal <$> item) <|> pure currency)

    let r = Item <$> offerR <*> pure iid <*> quantityR <*> priceR <*> taxR <*> vatR <*> amountR <*> currencyR
    let w = $(widgetFile "admin/billing/items/form-create")
    return (r,w)


getAdmInvoiceItemR :: InvoiceId -> ItemId -> Handler Html
getAdmInvoiceItemR iid xid = do
    item <- runDB $ selectOne $ do
        x :& _ :& s <- from $ table @Item
          `innerJoin` table @Offer `on` (\(x :& o) -> x ^. ItemOffer ==. o ^. OfferId)
          `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
        where_ $ x ^. ItemId ==. val xid
        return (x,s)

    dlgItemDelete <- newIdent
    msgs <- getMessages
    (fw,et) <- generateFormPost formItemDelete
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/item")


getAdmInvoiceItemsR :: InvoiceId -> Handler Html
getAdmInvoiceItemsR iid = do

    items <- zip [1 :: Int ..] <$> runDB ( select $ do
        x :& _ :& s <- from $ table @Item
          `innerJoin` table @Offer `on` (\(x :& o) -> x ^. ItemOffer ==. o ^. OfferId)
          `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
        where_ $ x ^. ItemInvoice ==. val iid
        orderBy [asc (x ^. ItemId)]
        return (x,s) )

    curr <- getCurrentRoute
    fabAddInvoiceItem <- newIdent
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgInvoiceItems
        $(widgetFile "admin/billing/items/items")


postAdmInvoiceDeleteR :: InvoiceId -> Handler Html
postAdmInvoiceDeleteR iid = do
    ((fr,_),_) <- runFormPost formInvoiceDelete
    case fr of
      FormSuccess _ -> do
          runDB $ delete iid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR AdmInvoicesR
      _ -> do
          addMessageI info MsgActionCancelled
          redirect $ AdminR AdmInvoicesR


formInvoiceDelete :: Html -> MForm Handler (FormResult (),Widget)
formInvoiceDelete extra = return (pure (),[whamlet|#{extra}|])


getAdmInvoiceEditR :: InvoiceId -> Handler Html
getAdmInvoiceEditR iid = do
    invoice <- runDB $ selectOne $ do
        x <- from $ table @Invoice
        where_ $ x ^. InvoiceId ==. val iid
        return x
    (fw,et) <- generateFormPost $ formInvoice invoice
    defaultLayout $ do
        setTitleI MsgInvoice
        $(widgetFile "admin/billing/edit")


postAdmInvoiceR :: InvoiceId -> Handler Html
postAdmInvoiceR iid = do
    invoice <- runDB $ selectOne $ do
        x <- from $ table @Invoice
        where_ $ x ^. InvoiceId ==. val iid
        return x
    ((fr,fw),et) <- runFormPost $ formInvoice invoice
    case fr of
      FormSuccess r -> do
          runDB $ replace iid r
          addMessageI info MsgRecordEdited
          redirect $ AdminR $ AdmInvoiceR iid
      _ -> defaultLayout $ do
          setTitleI MsgInvoice
          $(widgetFile "admin/billing/edit")


getAdmInvoiceR :: InvoiceId -> Handler Html
getAdmInvoiceR iid = do

    business <- runDB $ selectOne $ from $ table @Business

    invoice <- runDB $ selectOne $ do
        x :& c :& e <- from $ table @Invoice
            `innerJoin` table @User `on` (\(x :& c) -> x ^. InvoiceCustomer ==. c ^. UserId)
            `innerJoin` table @Staff `on` (\(x :& _ :& e) -> x ^. InvoiceStaff ==. e ^. StaffId)

        let a :: SqlExpr (Value (Maybe Centi))
            a = subSelect $ do
                i <- from $ table @Item
                where_ $ i ^. ItemInvoice ==. x ^. InvoiceId
                return $ coalesceDefault [sum_ (i ^. ItemAmount)] (val 0)
            
        where_ $ x ^. InvoiceId ==. val iid
        return (x,c,e,coalesceDefault [a] (val 0))

    curr <- getCurrentRoute
    dlgInvoiceDelete <- newIdent
    msgs <- getMessages
    (fw,et) <- generateFormPost formInvoiceDelete
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

    currency <- (unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Business
        return $ x ^. BusinessCurrency )

    invoices <- (second unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Invoice
        
        let a :: SqlExpr (Value (Maybe Centi))
            a = subSelect $ do
                i <- from $ table @Item
                where_ $ i ^. ItemInvoice ==. x ^. InvoiceId
                return $ coalesceDefault [sum_ (i ^. ItemAmount)] (val 0)
            
        orderBy [desc (x ^. InvoiceNumber)]
        return (x, coalesceDefault [a] (val 0)) )

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
