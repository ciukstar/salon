{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Admin.Billing
  ( getAdmInvoicesR
  , postAdmInvoicesR
  , getAdmInvoiceCreateR
  , getAdmInvoiceR
  , getAdmInvoiceEditR
  , postAdmInvoiceR
  , getAdmInvoiceReportR
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
  , getAdmInvoiceSendmailR
  , postAdmInvoiceSendmailR
  , getAdmInvoiceMailR
  , postAdmInvoiceMailDeleteR
  , getBillingMailHookR
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( tryAny, SomeException (SomeException), Exception (fromException))
import Control.Lens ((?~), sumOf, folded, to, ix, _Just)
import qualified Control.Lens as L ((^.), (^?))
import Control.Monad (join, forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Lens (AsValue(_String), key)
import Data.Bifunctor (Bifunctor(first, second))
import Data.ByteString (toStrict)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import Data.ByteString.Base64.Lazy (encode)
import Data.Complex (Complex ((:+)))
import Data.Fixed (Centi)
import Data.Function ((&))
import Data.Maybe (isJust, fromMaybe, maybeToList)
import qualified Data.List.Safe as LS (last)
import Data.Text (Text, pack, unpack, toUpper)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy as TL (empty)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Format.ISO8601 (iso8601Show)
import Graphics.PDF
    ( pdfByteString, PDFDocumentInfo (author), standardViewerPrefs
    , PDFRect (PDFRect), PDF, addPage, drawWithPage, AnyFont, black
    , Rectangle (Rectangle), PDFFont (PDFFont), mkStdFont, strokeColor
    , FontName (Times_Roman, Times_Bold), setWidth, Shape (stroke), Line (Line)
    , setStrokeAlpha, white
    )
import Graphics.PDF.Document
    ( standardDocInfo, PDFDocumentInfo (subject, compressed, viewerPreferences)
    , PDFViewerPreferences (displayDoctitle)
    )
import Graphics.PDF.Typesetting
    ( drawTextBox, paragraph, setJustification, txt, Orientation(NE)
    , Justification(LeftJustification)
    , StandardParagraphStyle(NormalParagraph), StandardStyle(Font)
    )
import Network.Mail.Mime
    ( Mail, Address (Address), renderMail', simpleMailInMemory )
import Network.HTTP.Client
    ( HttpExceptionContent(StatusCodeException)
    , HttpException (HttpExceptionRequest)
    )
import Network.Wreq
    ( post, FormParam ((:=)), responseBody, responseStatus, statusCode
    , postWith, defaults, auth, oauth2Bearer
    )
import System.Directory (doesFileExist)
import Text.Blaze.Html (preEscapedToHtml, toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Hamlet (Html, HtmlUrlI18n, ihamlet)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), TypedContent, SomeMessage (SomeMessage)
    , MonadHandler (liftHandler), redirect, addMessageI, getMessages
    , getCurrentRoute, lookupPostParam, whamlet, getYesod, lookupSession
    , getUrlRender, setSession, getMessageRender, getUrlRenderParams
    , addMessage, selectRep, provideRep, notFound, newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions
    ( generateFormPost, mreq, mopt, runFormPost, checkM
    )
import Yesod.Form.Input (runInputGet, iopt, ireq)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess, FormFailure, FormMissing), Field
    , FieldView (fvInput, fvErrors, fvLabel, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields
    ( dayField, hiddenField, unTextarea, intField, doubleField, textField
    , textareaField, emailField, checkBoxField
    )

import Yesod.Persist
    ( Entity (Entity, entityVal, entityKey)
    , PersistStoreWrite (insert_, replace, delete, insert)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Database.Esqueleto.Experimental
    ( select, from, table, where_, innerJoin, on, Value (unValue, Value), max_
    , (==.), (^.), (:&)((:&)), (?.), (=.)
    , orderBy, asc, fromSqlKey, selectOne, val, isNothing_, not_, just, leftJoin
    , toSqlKey, subSelect, sum_, SqlExpr, coalesceDefault, update, set, desc
    )

import Foundation
    ( Handler, Widget, App (appSettings)
    , Route
      ( AuthR, PhotoPlaceholderR, AccountPhotoR, ProfileR, AdminR, StaticR
      , ServiceThumbnailR, BillingMailHookR
      )
    , AdminR
      ( AdmInvoicesR, AdmInvoiceCreateR, AdmStaffPhotoR, AdmInvoiceR
      , AdmInvoiceDeleteR, AdmInvoiceEditR, AdmInvoiceItemsR
      , AdmInvoiceItemCreateR, AdmInvoiceItemCreateR, AdmInvoiceItemCreateR
      , AdmInvoiceItemR, AdmInvoiceItemEditR, AdmInvoiceItemDeleteR
      , AdmInvoiceSendmailR, AdmInvoiceMailR, AdmInvoiceMailDeleteR
      , AdmInvoiceReportR
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
      , MsgAmount, MsgCurrency, MsgThumbnail, MsgRecordDeleted, MsgFromEmail
      , MsgActionCancelled, MsgRecordEdited, MsgSend, MsgToEmail, MsgBodyEmail
      , MsgSubjectEmail, MsgMessageSent, MsgMessageNotSent, MsgAppName, MsgTime
      , MsgTheName, MsgPaymentDueDate, MsgMail, MsgNoMailYet, MsgEmail, MsgAsPdf
      , MsgRecipient, MsgSender, MsgMessage, MsgInvalidFormData, MsgSendAsHtml
      , MsgAttachPdf, MsgYes, MsgNo, MsgDownload, MsgAsHtml, MsgInvalidValue
      , MsgTotal, MsgSentAsHtml, MsgPdfAttached
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
      , OfferId, ItemOffer, ItemAmount, InvoiceMailId, InvoiceMailStatus
      , InvoiceMailTimemark, InvoiceMailInvoice, TokenApi, StoreToken, StoreVal
      , StoreKey
      )
    , Staff (Staff, staffName, staffEmail), InvoiceId
    , Business (Business, businessEmail, businessFullName)
    , ItemId, Thumbnail
    , Item
      ( Item, itemOffer, itemQuantity, itemPrice, itemTax, itemVat, itemAmount
      , itemCurrency, itemName
      )
    , Offer (Offer, offerQuantity, offerPrice), Service (Service, serviceName)
    , InvoiceMail
      ( InvoiceMail, invoiceMailRecipient, invoiceMailSender, invoiceMailSubject
      , invoiceMailBody, invoiceMailInvoice, invoiceMailRecipientName
      , invoiceMailSenderName, invoiceMailHtml, invoiceMailPdf
      )
    , InvoiceMailId
    , MailStatus (MailStatusDraft, MailStatusBounced, MailStatusDelivered)
    , _itemAmount, _itemCurrency, _itemVat, _itemTax, _itemQuantity
    , gmailAccessToken, gmailRefreshToken, Token (Token), gmail
    , StoreType ( StoreTypeDatabase, StoreTypeGoogleSecretManager )
    , Store
    )

import Menu (menu)
import Settings (widgetFile, AppSettings (appGoogleClientId, appGoogleClientSecret))
import Settings.StaticFiles (img_photo_FILL0_wght400_GRAD0_opsz48_svg)


getBillingMailHookR :: Handler Html
getBillingMailHookR = do
    rndr <- getUrlRender
    app <- appSettings <$> getYesod
    let googleClientId = appGoogleClientId app
    let googleClientSecret = appGoogleClientSecret app
    
    code <- runInputGet $ ireq textField "code"
    mid <- toSqlKey <$> runInputGet (ireq intField "state")

    r <- liftIO $ post "https://oauth2.googleapis.com/token"
         [ "code" := code
         , "redirect_uri" := rndr BillingMailHookR
         , "client_id" := googleClientId
         , "client_secret" := googleClientSecret
         , "grant_type" := ("authorization_code" :: Text)
         ]

    let accessToken = r L.^. responseBody . key "access_token" . _String
    let refreshToken = r L.^. responseBody . key "refresh_token" . _String

    setSession gmailAccessToken accessToken
    setSession gmailRefreshToken refreshToken
    
    (invoiceMail,customer,employee,invoice) <- do
        p <- runDB $ selectOne $ do
            x :& i :& c :& e <- from $ table @InvoiceMail
                `innerJoin` table @Invoice `on` (\(x :& i) -> x ^. InvoiceMailInvoice ==. i ^. InvoiceId)
                `innerJoin` table @User `on` (\(_ :& i :& c) -> i ^. InvoiceCustomer ==. c ^. UserId)
                `innerJoin` table @Staff `on` (\(_ :& i :& _ :& e) -> i ^. InvoiceStaff ==. e ^. StaffId)                
            where_ $ x ^. InvoiceMailId ==. val mid
            return (((x,i),c),e)
        return ( fst . fst . fst <$> p
               , snd . fst <$> p
               , snd <$> p
               , snd . fst . fst <$> p
               )
        
    items <- case invoice of
      Just (Entity iid _) -> runDB $ select $ do
          x <- from $ table @Item
          where_ $ x ^. ItemInvoice ==. val iid
          return x
      Nothing -> return []
          
    timesRoman <- liftIO $ mkStdFont Times_Roman
    timesBold <- liftIO $ mkStdFont Times_Bold

    case (invoiceMail,timesRoman,timesBold) of
      (Just (Entity _ imail),Right fr,Right fb) -> do
          urlRndr <- getUrlRenderParams
          transMsg <- getMessageRender
          msgRndr <- (toHtml .) <$> getMessageRender
          let html = if invoiceMailHtml imail
                  then Just $ renderIvoiceHtml customer employee invoice items msgRndr urlRndr
                  else Nothing
              pdf = if invoiceMailPdf imail
                  then Just $ renderIvoicePdf
                       ( invoicePdf customer employee invoice items transMsg (PDFRect 0 0 612 792) fr fb
                       )
                    else Nothing
              mail = buildMail imail html pdf
          raw <- liftIO $ decodeUtf8 . toStrict . encode <$> renderMail' mail

          let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken)
          response <- liftIO $ tryAny $ postWith opts
              (gmailApi $ unpack $ invoiceMailSender imail)
              (object ["raw" .= raw])

          case response of
            Left e@(SomeException _) -> case fromException e of
              Just (HttpExceptionRequest _ (StatusCodeException r' bs)) -> do
                  case r' L.^. responseStatus . statusCode of
                    403 -> do
                        addMessage warn (bs L.^. key "error" . key "message" . _String . to toHtml)
                        addMessageI info MsgMessageNotSent
                        redirect $ AdminR $ AdmInvoiceR (invoiceMailInvoice imail)
                    _ -> do
                        addMessageI info MsgMessageNotSent
                        redirect $ AdminR $ AdmInvoiceR (invoiceMailInvoice imail)
              _other -> do
                 addMessageI info MsgMessageNotSent
                 redirect $ AdminR $ AdmInvoiceR (invoiceMailInvoice imail)
            Right _ok -> do

              now' <- liftIO getCurrentTime
              runDB $ update $ \x -> do
                  set x [ InvoiceMailStatus =. val MailStatusDelivered
                        , InvoiceMailTimemark =. val now'
                        ]
                  where_ $ x ^. InvoiceMailId ==. val mid
              addMessageI info MsgMessageSent

              redirect $ AdminR $ AdmInvoiceR (invoiceMailInvoice imail)
      _otherwise -> do
          addMessageI info MsgMessageNotSent
          redirect $ AdminR AdmInvoicesR


postAdmInvoiceMailDeleteR :: InvoiceId -> InvoiceMailId -> Handler Html
postAdmInvoiceMailDeleteR iid mid = do
    ((fr,_),_) <- runFormPost formInvoiceMailDelete
    case fr of
      FormSuccess _ -> do
          runDB $ delete mid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR $ AdmInvoiceSendmailR iid
      _otherwise -> do
          addMessageI info MsgInvalidFormData
          redirect $ AdminR $ AdmInvoiceSendmailR iid


getAdmInvoiceMailR :: InvoiceId -> InvoiceMailId -> Handler Html
getAdmInvoiceMailR iid mid = do

    email <- runDB $ selectOne $ do
        x <- from $ table @InvoiceMail
        where_ $ x ^. InvoiceMailId ==. val mid
        return x

    dlgInvoiceMailDelete <- newIdent
    (fw,et) <- generateFormPost formInvoiceMailDelete
    defaultLayout $ do
        setTitleI MsgEmail
        $(widgetFile "admin/billing/mail/email")


formInvoiceMailDelete :: Html -> MForm Handler (FormResult (),Widget)
formInvoiceMailDelete extra = return (pure (),[whamlet|#{extra}|])


getAdmInvoiceSendmailR :: InvoiceId -> Handler Html
getAdmInvoiceSendmailR iid = do
    
    mail <- runDB $ select $ do
        x <- from $ table @InvoiceMail
        where_ $ x ^. InvoiceMailInvoice ==. val iid
        orderBy [desc (x ^. InvoiceMailTimemark)]
        return x
    
    curr <- getCurrentRoute
    defaultLayout $ do
        setTitleI MsgMail
        $(widgetFile "admin/billing/mail/mail")


postAdmInvoiceSendmailR :: InvoiceId -> Handler Html
postAdmInvoiceSendmailR iid = do

    (customer,employee,invoice) <- do
        p <- runDB $ selectOne $ do
            x :& c :& e <- from $ table @Invoice
                `innerJoin` table @User `on` (\(x :& c) -> x ^. InvoiceCustomer ==. c ^. UserId)
                `innerJoin` table @Staff `on` (\(x :& _ :& e) -> x ^. InvoiceStaff ==. e ^. StaffId)
            where_ $ x ^. InvoiceId ==. val iid
            return ((c,e),x)
        return (fst . fst <$> p,snd . fst <$> p,snd <$> p)

    items <- runDB $ select $ do
        x <- from $ table @Item
        where_ $ x ^. ItemInvoice ==. val iid
        return x
    
    ((fr2,_),_) <- runFormPost $ formInvoiceSendmail iid customer employee invoice
    case fr2 of
      FormSuccess imail -> do
          mid <- runDB $ insert imail
          app <- getYesod
          let googleClientId = appGoogleClientId $ appSettings app
              googleClientSecret = appGoogleClientSecret $ appSettings app

          store <- runDB $ selectOne $ do
              x <- from $ table @Token
              where_ $ x ^. TokenApi ==. val gmail
              return x

          let at = "/at/gmail_access_token"
              rt = "/rt/gmail_refresh_token"

          secrets <-  liftIO $ and <$> forM [at,rt] doesFileExist
          
          accessToken <- case (store,secrets) of
            (Nothing,True) -> Just . pack <$> liftIO ( readFile at )
            
            (Just (Entity _ (Token _ StoreTypeGoogleSecretManager)),True) -> do
                Just . pack <$> liftIO ( readFile at )
                
            (Just (Entity tid (Token _ StoreTypeDatabase)),_) -> (unValue <$>) <$> runDB ( selectOne $ do
                x <- from $ table @Store
                where_ $ x ^. StoreToken ==. val tid
                where_ $ x ^. StoreKey ==. val gmailAccessToken
                return $ x ^. StoreVal )
                
            _otherwise -> lookupSession gmailAccessToken
            
          refreshToken <- case (store,secrets) of
            (Nothing,True) -> Just . pack <$> liftIO ( readFile rt )
            (Just (Entity _ (Token _ StoreTypeGoogleSecretManager)),True) -> do
                Just . pack <$> liftIO ( readFile rt )
                
            (Just (Entity tid (Token _ StoreTypeDatabase)),_) -> (unValue <$>) <$> runDB ( selectOne $ do
                x <- from $ table @Store
                where_ $ x ^. StoreToken ==. val tid
                where_ $ x ^. StoreKey ==. val gmailRefreshToken
                return $ x ^. StoreVal )
            _otherwise -> lookupSession gmailRefreshToken
          
          timesRoman <- liftIO $ mkStdFont Times_Roman
          timesBold <- liftIO $ mkStdFont Times_Bold

          case ((accessToken,refreshToken),(timesRoman,timesBold)) of
            ((Just atoken,Just rtoken),(Right fr,Right fb)) -> do
                msgRender <- getMessageRender
                urlRndr <- getUrlRenderParams
                let html = if invoiceMailHtml imail
                        then Just $ renderIvoiceHtml customer employee invoice items (toHtml . msgRender) urlRndr
                        else Nothing
                    pdf = if invoiceMailPdf imail
                        then pure $ renderIvoicePdf
                             ( invoicePdf customer employee invoice items msgRender (PDFRect 0 0 612 792) fr fb
                             )
                          else Nothing
                    mail = buildMail imail html pdf
                    
                raw <- liftIO $ decodeUtf8 . toStrict . encode <$> renderMail' mail

                let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 atoken)
                response <- liftIO $ tryAny $ postWith
                    opts (gmailApi $ unpack $ invoiceMailSender imail) (object ["raw" .= raw])

                case response of
                  Left e@(SomeException _) -> case fromException e of
                    Just (HttpExceptionRequest _ (StatusCodeException r' bs)) -> do
                        case r' L.^. responseStatus . statusCode of
                          401 -> do
                              refreshResponse <- liftIO $ post "https://oauth2.googleapis.com/token"
                                  [ "refresh_token" := rtoken
                                  , "client_id" := googleClientId
                                  , "client_secret" := googleClientSecret
                                  , "grant_type" := ("refresh_token" :: Text)
                                  ]

                              let newAccessToken = refreshResponse L.^. responseBody . key "access_token" . _String
                              
                              setSession gmailAccessToken newAccessToken
    
                              _ <- liftIO $ postWith
                                  (defaults & auth ?~ oauth2Bearer (encodeUtf8 newAccessToken))
                                  (gmailApi $ unpack $ invoiceMailSender imail)
                                  (object ["raw" .= raw])

                              now' <- liftIO getCurrentTime
                              runDB $ update $ \x -> do
                                  set x [ InvoiceMailStatus =. val MailStatusDelivered
                                        , InvoiceMailTimemark =. val now'
                                        ]
                                  where_ $ x ^. InvoiceMailId ==. val mid
                              addMessageI info MsgMessageSent
                              redirect $ AdminR $ AdmInvoiceR iid
                          403 -> do
                              addMessage warn (bs L.^. key "error" . key "message" . _String . to toHtml)
                              addMessageI info MsgMessageNotSent
                              redirect $ AdminR $ AdmInvoiceR iid
                          _ -> do
                              addMessageI info MsgMessageNotSent
                              redirect $ AdminR $ AdmInvoiceR iid
                    _other -> do
                        addMessageI info MsgMessageNotSent
                        redirect $ AdminR $ AdmInvoiceR iid
                  Right _ok -> do

                    now' <- liftIO getCurrentTime
                    runDB $ update $ \x -> do
                        set x [ InvoiceMailStatus =. val MailStatusDelivered
                              , InvoiceMailTimemark =. val now'
                              ]
                        where_ $ x ^. InvoiceMailId ==. val mid
                    addMessageI info MsgMessageSent

                    redirect $ AdminR $ AdmInvoiceR iid

            _notokens -> do

                rndr <- getUrlRender
                r <- liftIO $ post "https://accounts.google.com/o/oauth2/v2/auth"
                    [ "redirect_uri" := rndr BillingMailHookR
                    , "response_type" := ("code" :: Text)
                    , "prompt" := ("consent" :: Text)
                    , "client_id" := googleClientId
                    , "scope" := ("https://www.googleapis.com/auth/gmail.send" :: Text)
                    , "access_type" := ("offline" :: Text)
                    , "state" := pack (show $ fromSqlKey mid)
                    ]

                return $ preEscapedToHtml $ decodeUtf8 $ toStrict (r L.^. responseBody)
      _formNotSuccess -> redirect $ AdminR $ AdmInvoiceR iid


buildMail :: InvoiceMail -> Maybe Html -> Maybe BSL.ByteString -> Mail
buildMail imail html pdf = simpleMailInMemory
    (Address (invoiceMailRecipientName imail) (invoiceMailRecipient imail))
    (Address (invoiceMailSenderName imail) (invoiceMailSender imail))
    (invoiceMailSubject imail)
    TL.empty
    (maybe (fromStrict $ maybe "" unTextarea . invoiceMailBody $ imail) renderHtml html)
    (maybeToList $ (,,) "application/pdf" "invoice.pdf" <$> pdf)


invoicePdf :: Maybe (Entity User)
           -> Maybe (Entity Staff)
           -> Maybe (Entity Invoice)
           -> [Entity Item]
           -> (AppMessage -> Text)
           -> PDFRect -> AnyFont -> AnyFont -> PDF ()
invoicePdf customer employee invoice items trans (PDFRect x _ x' y') fontr fontb = do
    
    let tfont = PDFFont fontb 32
        hfont = PDFFont fontb 18
        rfont = PDFFont fontr 14
        sbold = PDFFont fontb 10
        sfont = PDFFont fontr 10
    
    let fontLogo = Font tfont black black
        fontTitle = Font hfont black black
        fontNormal = Font rfont black black
        fontSmall = Font sfont black black
        fontSmallBold = Font sbold black black
    
    let mt@ml = 72 :: Double
    
    page <- addPage Nothing
    drawWithPage page $ do
            
        let (Rectangle (x0 :+ y0) _,logo) =
                drawTextBox (x + ml) (y' - mt) (x' - 2 * ml) 30 NE NormalParagraph fontLogo $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgAppName
        logo
            
        let (Rectangle (x1 :+ y1) (x1' :+ _),title) =
                drawTextBox x0 (y0 - 10) (x' - 2 * ml) 30 NE NormalParagraph fontTitle $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgInvoice
        title
        
        strokeColor black
        setStrokeAlpha 0.3
        setWidth 1
        stroke (Line x1 y1 x1' y1)
            
        let (Rectangle (x2 :+ y2) (x2' :+ y2'),noLabel) =
                drawTextBox x1 (y1 - 30) ((x' - 2 * ml) / 3) 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ toUpper $ trans MsgInvoiceNumber
        noLabel
            
        let (Rectangle (x3 :+ y3) (_ :+ y3'),no) =
                drawTextBox x2 (y2 - 5) (x2' - x2) 30 NE NormalParagraph fontNormal $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (pack . show . invoiceNumber . entityVal) invoice
        no
            
        let (Rectangle (x4 :+ y4) (x4' :+ y4'),billToLabel) =
                drawTextBox x3 (y3 - 20) (x2' - x2) 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ toUpper $ trans MsgBillTo
        billToLabel
            
        let (Rectangle (x5 :+ y5) (x5' :+ y5'),billTo) =
                drawTextBox x4 (y4 - 5) ((x' - 2 * ml) / 3) 30 NE NormalParagraph fontNormal $ do
                    setJustification LeftJustification
                    paragraph $ txt $ fromMaybe "" (((userFullName . entityVal) =<< customer)
                                                    <|> userName . entityVal <$> customer)
        billTo
            
        let (Rectangle (x6 :+ y6) _,billToEmail) =
                drawTextBox x5 y5 (x5' - x5) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (\email -> " (" <> email <> ")") ((userEmail . entityVal) =<< customer)
        billToEmail
            
        let (Rectangle (x7 :+ y7) (x7' :+ y7'),billedFromLabel) =
                drawTextBox x6 (y6 - 20) (x2' - x2) 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ toUpper $ trans MsgBilledFrom
        billedFromLabel
            
        let (Rectangle (x8 :+ y8) (_ :+ y8'),billedFrom) =
                drawTextBox x7 (y7 - 5) (x7' - x7) 30 NE NormalParagraph fontNormal $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (staffName . entityVal) employee
        billedFrom
            
        let (Rectangle (x9 :+ y9) _,billedFromEmail) =
                drawTextBox x8 y8 (x7' - x7) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (\email -> " (" <> email <> ")") ((staffEmail . entityVal) =<< employee)
        billedFromEmail
            
        let (Rectangle (x10 :+ _) (x10' :+ y10'),invoiceDateLabel) =
                drawTextBox x2' y2' ((x' - 2 * ml) / 3) 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ toUpper $ trans MsgInvoiceDate
        invoiceDateLabel
            
        let (Rectangle _ (_ :+ y11'),invoiceDate) =
                drawTextBox x10 y3' (x10' - x10) 30 NE NormalParagraph fontNormal $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (pack . show . invoiceDay . entityVal) invoice
        invoiceDate
            
        let (Rectangle (x12 :+ _) (x12' :+ y12'),dueDateLabel) =
                drawTextBox x4' y4' (x10' - x10) 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ toUpper $ trans MsgPaymentDueDate
        dueDateLabel
            
        let (Rectangle _ (_ :+ y13'),dueDate) =
                drawTextBox x12 y5' (x12' - x12) 30 NE NormalParagraph fontNormal $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (pack . show) (invoiceDueDay . entityVal =<< invoice)
        dueDate
            
        let (Rectangle (x14 :+ _) (x14' :+ _),statusLabel) =
                drawTextBox x10' y10' ((x' - 2 * ml) / 3) 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ toUpper $ trans MsgStatus
        statusLabel
            
        let (_,status) =
                drawTextBox x14 y11' (x14' - x14) 30 NE NormalParagraph fontNormal $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (trans . (\case
                                                              InvoiceStatusDraft -> MsgDraft
                                                              InvoiceStatusOpen -> MsgOpen
                                                              InvoiceStatusPaid -> MsgPaid
                                                              InvoiceStatusUncollectible -> MsgUncollectible
                                                              InvoiceStatusVoid -> MsgVoid
                                                        ) . invoiceStatus . entityVal) invoice
        status
            
        let (Rectangle (x16 :+ _) (x16' :+ _),amountLabel) =
                drawTextBox x12' y12' (x14' - x14) 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ toUpper $ trans MsgAmount
        amountLabel
            
        let (_,amount) =
                drawTextBox x16 y13' (x16' - x16) 30 NE NormalParagraph fontNormal $ do
                    setJustification LeftJustification
                    paragraph $ txt $ pack $ show (items & sumOf (folded . to entityVal . _itemAmount))
        amount
            
        let (Rectangle (x18 :+ _) (x18' :+ _),currencyLabel) =
                drawTextBox x16 y7' (x16' - x16) 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ toUpper $ trans MsgCurrency
        currencyLabel
            
        let (_,currency) =
                drawTextBox x18 y8' (x18' - x18) 30 NE NormalParagraph fontNormal $ do
                    setJustification LeftJustification
                    paragraph $ txt $ fromMaybe "" $ join $ items L.^? ix 0 . to entityVal . _itemCurrency
        currency
            
        let (Rectangle (x20 :+ y20) (x20' :+ _),details) =
                drawTextBox x9 (y9 - 30) (x' - 2 * ml) 30 NE NormalParagraph fontTitle $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgDetails
        details
        
        strokeColor black
        setStrokeAlpha 0.3
        setWidth 1
        stroke (Line x20 y20 x20' y20)

            
        let (Rectangle (x21 :+ y21) (x21' :+ y21'),headerName) =
                drawTextBox x20 (y20 - 20) 158 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgTheName
        headerName
            
        let (Rectangle (x22 :+ _) (x22' :+ y22'),headerQuantity) =
                drawTextBox x21' y21' 60 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgQuantity
        headerQuantity
            
        let (Rectangle (x23 :+ _) (x23' :+ y23'),headerPrice) =
                drawTextBox x22' y22' 60 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgPrice
        headerPrice
            
        let (Rectangle (x24 :+ _) (x24' :+ y24'),headerTax) =
                drawTextBox x23' y23' 40 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgTax
        headerTax
            
        let (Rectangle (x25 :+ _) (x25' :+ y25'),headerVat) =
                drawTextBox x24' y24' 40 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgVat
        headerVat
            
        let (Rectangle (x26 :+ _) (x26' :+ y26'),headerAmount) =
                drawTextBox x25' y25' 60 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgAmount
        headerAmount
            
        let (Rectangle (x27 :+ _) (x27' :+ _),headerCurrency) =
                drawTextBox x26' y26' 50 30 NE NormalParagraph fontSmallBold $ do
                    setJustification LeftJustification
                    paragraph $ txt $ trans MsgCurrency
        headerCurrency
                     
        
        lastRow <- LS.last <$> forM (zip [1 ..] items) ( \(i,Entity _ (Item _ _ name q p t v a c)) -> do
            
            let (r1@(Rectangle _ (_ :+ b1')),v1) =
                    drawTextBox x21 (y21 - i * 30) (x21' - x21) 30 NE NormalParagraph fontSmall $ do
                        setJustification LeftJustification
                        paragraph $ txt name
            v1
            
            let (r2,v2) = drawTextBox x22 b1' (x22' - x22) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ pack $ show q
            v2
            
            let (r3,v3) = drawTextBox x23 b1' (x23' - x23) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ pack $ show p
            v3
            
            let (r4,v4) = drawTextBox x24 b1' (x24' - x24) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (pack . show) t
            v4
            
            let (r5,v5) = drawTextBox x25 b1' (x25' - x25) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (pack . show) v
            v5
            
            let (r6,v6) = drawTextBox x26 b1' (x26' - x26) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ pack $ show a
            v6
            
            let (r7,v7) = drawTextBox x27 b1' (x27' - x27) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ fromMaybe "" c
            v7

            return (r1,r2,r3,r4,r5,r6,r7) )

        case lastRow of
          Just ( Rectangle (a1 :+ b1) (a1' :+ _)
               , Rectangle (a2 :+ _) (a2' :+ _)
               , Rectangle (a3 :+ _) (a3' :+ _)
               , Rectangle (a4 :+ _) (a4' :+ _)
               , Rectangle (a5 :+ _) (a5' :+ _)
               , Rectangle (a6 :+ _) (a6' :+ _)
               , Rectangle (a7 :+ _) (a7' :+ _)) -> do

            let (Rectangle (x28 :+ y28) (x28' :+ _),total) =
                    drawTextBox a1 (b1 - 30) (a7' - a1) 30 NE NormalParagraph fontTitle $ do
                        setJustification LeftJustification
                        paragraph $ txt $ trans MsgTotal
            total
            
            strokeColor black
            setStrokeAlpha 0.3
            setWidth 1
            stroke (Line x28 y28 x28' y28)


            let (r1@(Rectangle _ (x29' :+ y29')),_) =
                    drawTextBox x28 (y28 - 20) (a1' - a1) 30 NE NormalParagraph fontSmallBold $ do
                        setJustification LeftJustification
                        paragraph $ txt $ trans MsgTotal
                        
            strokeColor white
            setStrokeAlpha 0
            setWidth 0
            stroke r1

            let (Rectangle _ (x30' :+ y30'),footerQuantity) =
                    drawTextBox x29' y29' (a2' - a2) 30 NE NormalParagraph fontSmallBold $ do
                        setJustification LeftJustification
                        paragraph $ txt $ pack $ show $ items & sumOf (folded . to entityVal . _itemQuantity)
            footerQuantity

            let (r3@(Rectangle _ (x31' :+ y31')),_) =
                    drawTextBox x30' y30' (a3' - a3) 30 NE NormalParagraph fontSmallBold $ do
                        setJustification LeftJustification
                        paragraph $ txt $ trans MsgPrice
              
            strokeColor white
            setStrokeAlpha 0
            setWidth 0
            stroke r3

            let (Rectangle _ (x32' :+ y32'),footerTax) =
                    drawTextBox x31' y31' (a4' - a4) 30 NE NormalParagraph fontSmallBold $ do
                        setJustification LeftJustification
                        paragraph $ txt $ pack $ show $ items & sumOf (folded . to entityVal . _itemTax . _Just)
            footerTax

            let (Rectangle _ (x33' :+ y33'),footerVat) =
                    drawTextBox x32' y32' (a5' - a5) 30 NE NormalParagraph fontSmallBold $ do
                        setJustification LeftJustification
                        paragraph $ txt $ pack $ show $ items & sumOf (folded . to entityVal . _itemVat . _Just)
            footerVat

            let (Rectangle _ (x34' :+ y34'),footerAmount) =
                    drawTextBox x33' y33' (a6' - a6) 30 NE NormalParagraph fontSmallBold $ do
                        setJustification LeftJustification
                        paragraph $ txt $ pack $ show $ items & sumOf (folded . to entityVal . _itemAmount)
            footerAmount

            let (_,footerCurrency) =
                    drawTextBox x34' y34' (a7' - a7) 30 NE NormalParagraph fontSmallBold $ do
                        setJustification LeftJustification
                        paragraph $ txt $ fromMaybe " " (join $ items L.^? ix 0 . to entityVal . _itemCurrency)
                            
            footerCurrency
            
          Nothing -> return ()
            

renderIvoicePdf :: PDF () -> BSL.ByteString
renderIvoicePdf = pdfByteString
    standardDocInfo { author = "Sergiu Starciuc"
                    , subject = "Invoice"
                    , compressed = True
                    , viewerPreferences = standardViewerPrefs { displayDoctitle = True }
                    }
    (PDFRect 0 0 612 792)
    
    
renderIvoiceHtml :: Maybe (Entity User)
                 -> Maybe (Entity Staff)
                 -> Maybe (Entity Invoice)
                 -> [Entity Item]
                 -> HtmlUrlI18n AppMessage (Route App)
renderIvoiceHtml customer employee invoice items = [ihamlet|
<h1>_{MsgAppName}
<h2>_{MsgInvoice}  
$maybe Entity _ (User uname _ _ _ _ _ cname cemail) <- customer
  $maybe Entity _ (Staff ename _ _ _ eemail _) <- employee
    $maybe Entity _ (Invoice _ _ no status day due) <- invoice
      <table cellspacing=0 cellpadding=10 border=0>
        <tbody>
          <tr>
            <td style="vertical-align:text-top">
              <div style="font-weight:bold;text-transform:uppercase">
                _{MsgInvoiceNumber}
              <div>#{no}
            <td style="vertical-align:text-top">
              <div style="font-weight:bold;text-transform:uppercase">
                _{MsgInvoiceDate}
              <div>#{show day}
            <td style="vertical-align:text-top">
              <div style="font-weight:bold;text-transform:uppercase">
                _{MsgStatus}
              <div>
                $case status
                  $of InvoiceStatusDraft
                    _{MsgDraft}
                  $of InvoiceStatusOpen
                    _{MsgOpen}
                  $of InvoiceStatusPaid
                    _{MsgPaid}
                  $of InvoiceStatusUncollectible
                    _{MsgUncollectible}
                  $of InvoiceStatusVoid
                    _{MsgVoid}

          <tr>
            <td style="vertical-align:text-top">
              <div style="font-weight:bold;text-transform:uppercase">
                _{MsgBillTo}
              <div>
                $maybe name <- cname
                  #{name}
                $nothing
                  #{uname}
                $maybe email <- cemail
                  <div>
                    <small>#{email}
            <td style="vertical-align:text-top">
              <div style="font-weight:bold;text-transform:uppercase">
                _{MsgPaymentDueDate}
              <div>
                $maybe due <- due
                  #{show due}
            <td style="vertical-align:text-top">
              <div style="font-weight:bold;text-transform:uppercase">
                _{MsgAmount}
              <div>#{show amount}

          <tr>
            <td style="vertical-align:text-top">
              <div style="font-weight:bold;text-transform:uppercase">
                _{MsgBilledFrom}
              <div>
                #{ename}
                $maybe email <- eemail
                  <div>
                    <small>#{email}
            <td style="vertical-align:text-top">
            <td style="vertical-align:text-top">
              <div style="font-weight:bold;text-transform:uppercase">
                _{MsgCurrency}
              <div>
                $maybe c <- currency
                  #{c}


<h2>_{MsgDetails}
<table style="border:1px solid rgba(0,0,0,0.3);border-collapse:collapse">
  <thead>
    <tr>
      $forall h <- [MsgTheName,MsgQuantity,MsgPrice,MsgTax,MsgVat,MsgAmount,MsgCurrency]
        <th style="border:1px solid rgba(0,0,0,0.3);padding:1rem">_{h}
  <tbody>
    $forall Entity _ (Item _ _ name q p t v a c) <- items
      <tr>
        <td style="border:1px solid rgba(0,0,0,0.3);padding:1rem">#{name}
        <td style="border:1px solid rgba(0,0,0,0.3);padding:1rem">#{show q}
        <td style="border:1px solid rgba(0,0,0,0.3);padding:1rem">#{show p}
        <td style="border:1px solid rgba(0,0,0,0.3);padding:1rem">
          $maybe tax <- t
            #{show tax}
        <td style="border:1px solid rgba(0,0,0,0.3);padding:1rem">
          $maybe vat <- v
            #{show vat}
        <td style="border:1px solid rgba(0,0,0,0.3);padding:1rem">#{show a}
        <td style="border:1px solid rgba(0,0,0,0.3);padding:1rem">
          $maybe currency <- c
            #{currency}
  <tfoot>
    <tr>
      <th style="border:1px solid rgba(0,0,0,0.3);padding:1rem">_{MsgTotal}
      <th style="border:1px solid rgba(0,0,0,0.3);padding:1rem">
        #{show quantity}  
      <th style="border:1px solid rgba(0,0,0,0.3);padding:1rem">
      <th style="border:1px solid rgba(0,0,0,0.3);padding:1rem">
        #{show tax}
      <th style="border:1px solid rgba(0,0,0,0.3);padding:1rem">
        #{show vat}
      <th style="border:1px solid rgba(0,0,0,0.3);padding:1rem">
        #{show amount}
      <th style="border:1px solid rgba(0,0,0,0.3);padding:1rem">
        $maybe c <- currency
          #{c}
|]
    where
      quantity = items & sumOf (folded . to entityVal . _itemQuantity)
      tax = items & sumOf (folded . to entityVal . _itemTax . _Just)
      vat = items & sumOf (folded . to entityVal . _itemVat . _Just)
      amount = items & sumOf (folded . to entityVal . _itemAmount)
      currency = join $ items L.^? ix 0 . to entityVal . _itemCurrency


gmailApi :: String -> String
gmailApi = printf "https://gmail.googleapis.com/gmail/v1/users/%s/messages/send"


formInvoiceSendmail :: InvoiceId -> Maybe (Entity User) -> Maybe (Entity Staff) -> Maybe (Entity Invoice)
                    -> Html -> MForm Handler (FormResult InvoiceMail,Widget)
formInvoiceSendmail iid customer employee invoice extra = do

    now <- liftIO getCurrentTime
    
    (toR,toV) <- mreq emailField FieldSettings
        { fsLabel = SomeMessage MsgToEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userEmail . entityVal =<< customer)
        
    (recipientR,recipientV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgRecipient
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userFullName . entityVal <$> customer)

    business <- liftHandler $ runDB $ selectOne $ from $ table @Business
        
    (fromR,fromV) <- mreq emailField FieldSettings
        { fsLabel = SomeMessage MsgFromEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((businessEmail . entityVal =<< business) <|> (staffEmail . entityVal =<< employee))
        
    (senderR,senderV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgSender
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ( Just ((unTextarea <$>) . businessFullName . entityVal =<< business)
            <|> Just (staffName . entityVal <$> employee)
          )

    msgRender <- getMessageRender
        
    (subjectR,subjectV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgSubjectEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ( (((msgRender MsgInvoice <> " ") <> msgRender MsgNumberSign) <>)
            . pack . show . invoiceNumber . entityVal <$> invoice
          )
        
    (bodyR,bodyV) <- mopt textareaField FieldSettings
        { fsLabel = SomeMessage MsgBodyEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } Nothing
        
    (htmlR,htmlV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgSendAsHtml
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (Just True)
        
    (pdfR,pdfV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAttachPdf
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-checkbox__native-control")]
        } (Just True)

    let r = InvoiceMail iid MailStatusDraft now
            <$> toR <*> recipientR
            <*> fromR <*> senderR
            <*> subjectR
            <*> bodyR
            <*> htmlR
            <*> pdfR
            
    let w = [whamlet|
#{extra}
$forall v <- [toV,recipientV,fromV,senderV,subjectV]
  <div.form-field>
    <label.mdc-text-field.mdc-text-field--filled data-mdc-auto-init=MDCTextField
      :isJust (fvErrors v):.mdc-text-field--invalid>
      <span.mdc-text-field__ripple>
      <span.mdc-floating-label>#{fvLabel v}
      ^{fvInput v}
      <div.mdc-line-ripple>
    $maybe errs <- fvErrors v
      <div.mdc-text-field-helper-line>
        <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
          #{errs}
          
<div.form-field>
  <label.mdc-text-field.mdc-text-field--textarea.mdc-text-field--filled data-mdc-auto-init=MDCTextField
    :isJust (fvErrors bodyV):.mdc-text-field--invalid>
    <span.mdc-text-field__ripple>
    <span.mdc-floating-label>#{fvLabel bodyV}
    <span.mdc-text-field__resizer>
      ^{fvInput bodyV}
    <div.mdc-line-ripple>
  $maybe errs <- fvErrors bodyV
    <div.mdc-text-field-helper-line>
      <div.mdc-text-field-helper-text.mdc-text-field-helper-text--validation-msg aria-hidden=true>
        #{errs}
        
$forall (r,v) <- [(htmlR,htmlV),(pdfR,pdfV)]
  <div.mdc-form-field.form-field data-mdc-auto-init=MDCFormField style="display:flex;flex-direction:row">
    ^{fvInput v}
    $with selected <- resolveSelected r
      <button.mdc-switch type=button role=switch #switch#{fvId v} data-mdc-auto-init=MDCSwitch
        :selected:.mdc-switch--selected :selected:aria-checked=true
        :not selected:.mdc-switch--unselected :not selected:aria-checked=false
        onclick="document.getElementById('#{fvId v}').checked = !this.MDCSwitch.selected">
        <div.mdc-switch__track>
        <div.mdc-switch__handle-track>
          <div.mdc-switch__handle>
            <div.mdc-switch__shadow>
              <div.mdc-elevation-overlay>
            <div.mdc-switch__ripple>
            <div.mdc-switch__icons>
              <svg.mdc-switch__icon.mdc-switch__icon--on viewBox="0 0 24 24">
                <path d="M19.69,5.23L8.96,15.96l-4.23-4.23L2.96,13.5l6,6L21.46,7L19.69,5.23z">
              <svg.mdc-switch__icon.mdc-switch__icon--off viewBox="0 0 24 24">
                <path d="M20 13H4v-2h16v2z">

      <span.mdc-switch__focus-ring-wrapper>
        <span.mdc-switch__focus-ring>
      <label for=switch#{fvId v}>#{fvLabel v}
|]
    return (r,w)
  where
      resolveSelected r = case r of FormSuccess x -> x ; _otherwise -> True


postAdmInvoiceItemDeleteR :: InvoiceId -> ItemId -> Handler Html
postAdmInvoiceItemDeleteR iid xid = do
    ((fr,_),_) <- runFormPost formItemDelete
    case fr of
      FormSuccess _ -> do
          runDB $ delete xid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR $ AdmInvoiceItemsR iid
      _otherwise -> do
          addMessageI info MsgActionCancelled
          redirect $ AdminR $ AdmInvoiceItemsR iid


formItemDelete :: Html -> MForm Handler (FormResult (),Widget)
formItemDelete extra = return (pure (),[whamlet|#{extra}|])


postAdmInvoiceItemR :: InvoiceId -> ItemId -> Handler Html
postAdmInvoiceItemR iid xid = do
    (item,offer,service) <- do
        p <- runDB $ selectOne $ do
            x :& o :& s <- from $ table @Item
                `innerJoin` table @Offer `on` (\(x :& o) -> x ^. ItemOffer ==. o ^. OfferId)
                `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            where_ $ x ^. ItemId ==. val xid
            return ((x,o),s)
        return (fst . fst <$> p,snd . fst <$> p, snd <$> p)

    ((fr,fw),et) <- runFormPost $ formItemCreate iid offer service item
    case fr of
      FormSuccess r -> do
          runDB $ replace xid r
          addMessageI info MsgRecordEdited
          redirect $ AdminR $ AdmInvoiceItemR iid xid
      _otherwise -> defaultLayout $ do
          setTitleI MsgInvoiceItem
          $(widgetFile "admin/billing/items/edit")


postAdmInvoiceItemEditR :: InvoiceId -> ItemId -> Handler Html
postAdmInvoiceItemEditR iid xid = do
    moid <- (toSqlKey <$>) . (readMaybe . unpack =<<) <$> lookupPostParam "oid"
    (offer,service) <- case moid of
      Just oid -> do
          p <- runDB $ selectOne $ do
              o :& s <- from $ table @Offer
                  `innerJoin` table @Service `on` (\(o :& s) -> o ^. OfferService ==. s ^. ServiceId)
              where_ $ o ^. OfferId ==. val oid
              return (o,s)
          return (fst <$> p,snd <$> p)
      Nothing -> return (Nothing,Nothing)

    (fw,et) <- generateFormPost $ formItemEdit iid xid offer service Nothing
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/edit")


getAdmInvoiceItemEditR :: InvoiceId -> ItemId -> Handler Html
getAdmInvoiceItemEditR iid xid = do
    (item,offer,service) <- do
        p <- runDB $ selectOne $ do
            x :& o :& s <- from $ table @Item
                `innerJoin` table @Offer `on` (\(x :& o) -> x ^. ItemOffer ==. o ^. OfferId)
                `innerJoin` table @Service `on` (\(_ :& o :& s) -> o ^. OfferService ==. s ^. ServiceId)
            where_ $ x ^. ItemId ==. val xid
            return ((x,o),s)
        return (fst . fst <$> p,snd . fst <$> p, snd <$> p)

    (fw,et) <- generateFormPost $ formItemEdit iid xid offer service item
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/edit")


postAdmInvoiceItemsR :: InvoiceId -> Handler Html
postAdmInvoiceItemsR iid = do
    ((fr,fw),et) <- runFormPost $ formItemCreate iid Nothing Nothing Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI info MsgRecordAdded
          redirect $ AdminR $ AdmInvoiceItemsR iid
      _otherwise -> defaultLayout $ do
          setTitleI MsgInvoiceItem
          $(widgetFile "admin/billing/items/create")


postAdmInvoiceItemCreateR :: InvoiceId -> Handler Html
postAdmInvoiceItemCreateR iid = do
    moid <- (toSqlKey <$>) . (readMaybe . unpack =<<) <$> lookupPostParam "oid"
    (offer,service) <- case moid of
      Just oid -> do
          p <- runDB $ selectOne $ do
              o :& s <- from $ table @Offer
                  `innerJoin` table @Service `on` (\(o :& s) -> o ^. OfferService ==. s ^. ServiceId)
              where_ $ o ^. OfferId ==. val oid
              return (o,s)
          return (fst <$> p,snd <$> p)
      Nothing -> return (Nothing,Nothing)

    (fw,et) <- generateFormPost $ formItemCreate iid offer service Nothing
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/create")


getAdmInvoiceItemCreateR :: InvoiceId -> Handler Html
getAdmInvoiceItemCreateR iid = do
    moid <- (toSqlKey <$>) <$> runInputGet (iopt intField "oid")
    (offer,service) <- case moid of
      Just oid -> do
          p <- runDB $ selectOne $ do
              o :& s <- from $ table @Offer
                  `innerJoin` table @Service `on` (\(o :& s) -> o ^. OfferService ==. s ^. ServiceId)
              where_ $ o ^. OfferId ==. val oid
              return (o,s)
          return (fst <$> p,snd <$> p)
      Nothing -> return (Nothing,Nothing)

    (fw,et) <- generateFormPost $ formItemCreate iid offer service Nothing
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/create")


formItemEdit :: InvoiceId -> ItemId -> Maybe (Entity Offer) -> Maybe (Entity Service) -> Maybe (Entity Item)
             -> Html -> MForm Handler (FormResult Item, Widget)
formItemEdit iid xid offer service item extra = do

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

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((itemName . entityVal <$> item) <|> (serviceName . entityVal <$> service))

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

    let r = Item <$> offerR <*> pure iid <*> nameR <*> quantityR <*> priceR <*> taxR <*> vatR <*> amountR <*> currencyR
    let w = $(widgetFile "admin/billing/items/form-edit")
    return (r,w)


formItemCreate :: InvoiceId -> Maybe (Entity Offer) -> Maybe (Entity Service) -> Maybe (Entity Item)
               -> Html -> MForm Handler (FormResult Item, Widget)
formItemCreate iid offer service item extra = do

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

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgTheName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ((itemName . entityVal <$> item) <|> (serviceName . entityVal <$> service))

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

    let r = Item <$> offerR <*> pure iid <*> nameR <*> quantityR <*> priceR
                 <*> taxR <*> vatR <*> amountR <*> currencyR
            
    let w = $(widgetFile "admin/billing/items/form-create")
    return (r,w)


getAdmInvoiceItemR :: InvoiceId -> ItemId -> Handler Html
getAdmInvoiceItemR iid xid = do
    item <- runDB $ selectOne $ do
        x <- from $ table @Item
        where_ $ x ^. ItemId ==. val xid
        return x

    dlgItemDelete <- newIdent
    msgs <- getMessages
    (fw,et) <- generateFormPost formItemDelete
    defaultLayout $ do
        setTitleI MsgInvoiceItem
        $(widgetFile "admin/billing/items/item")


getAdmInvoiceItemsR :: InvoiceId -> Handler Html
getAdmInvoiceItemsR iid = do

    items <- zip [1 :: Int ..] <$> runDB ( select $ do
        x <- from $ table @Item
        where_ $ x ^. ItemInvoice ==. val iid
        orderBy [asc (x ^. ItemId)]
        return x )

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
      _otherwise -> do
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
      _otherwise -> defaultLayout $ do
          setTitleI MsgInvoice
          $(widgetFile "admin/billing/edit")


getAdmInvoiceReportR :: InvoiceId -> Handler TypedContent
getAdmInvoiceReportR iid = do
    (customer,employee,invoice) <- do
        p <- runDB $ selectOne $ do
            x :& c :& e <- from $ table @Invoice
                `innerJoin` table @User `on` (\(x :& c) -> x ^. InvoiceCustomer ==. c ^. UserId)
                `innerJoin` table @Staff `on` (\(x :& _ :& e) -> x ^. InvoiceStaff ==. e ^. StaffId)
            where_ $ x ^. InvoiceId ==. val iid
            return ((c,e),x)
        return (fst . fst <$> p,snd . fst <$> p,snd <$> p)
        
    items <- runDB $ select $ do
        x <- from $ table @Item
        where_ $ x ^. ItemInvoice ==. val iid
        orderBy [asc (x ^. ItemId)]
        return x
        
    msgRender <- getMessageRender
    urlRender <- getUrlRenderParams

    selectRep $ do
        provideRep $ do
            timesRoman <- liftIO $ mkStdFont Times_Roman
            timesBold <- liftIO $ mkStdFont Times_Bold
            case (timesRoman,timesBold) of
              (Right fr,Right fb) -> 
                  return $ invoicePdf customer employee invoice items msgRender (PDFRect 0 0 612 792) fr fb
              _otherwise -> notFound
        provideRep $ do
            return $ renderIvoiceHtml customer employee invoice items (toHtml . msgRender) urlRender


getAdmInvoiceR :: InvoiceId -> Handler Html
getAdmInvoiceR iid = do
    (customer,employee,invoice,amount) <- do
        p <- runDB $ selectOne $ do
            x :& c :& e <- from $ table @Invoice
                `innerJoin` table @User `on` (\(x :& c) -> x ^. InvoiceCustomer ==. c ^. UserId)
                `innerJoin` table @Staff `on` (\(x :& _ :& e) -> x ^. InvoiceStaff ==. e ^. StaffId)
            where_ $ x ^. InvoiceId ==. val iid

            let a :: SqlExpr (Value (Maybe Centi))
                a = subSelect $ do
                    i <- from $ table @Item
                    where_ $ i ^. ItemInvoice ==. x ^. InvoiceId
                    return $ coalesceDefault [sum_ (i ^. ItemAmount)] (val 0)
                
            return (((c,e),x),coalesceDefault [a] (val 0))
        return ( fst . fst . fst <$> p
               , snd . fst . fst <$> p
               , snd . fst <$> p
               , snd <$> p
               )

    business <- runDB $ selectOne $ from $ table @Business

    curr <- getCurrentRoute
    dlgInvoiceDelete <- newIdent
    msgs <- getMessages
    (fw,et) <- generateFormPost formInvoiceDelete
    (fw2,et2) <- generateFormPost $ formInvoiceSendmail iid customer employee invoice

    dlgInvoiceSendmail <- newIdent
    menuDownload <- newIdent
    formSendmailInvoice <- newIdent
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
      _otherwise -> defaultLayout $ do
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
          Nothing -> return Nothing

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

    msgRender <- liftHandler getMessageRender

    let f :: FormResult (Maybe a) -> FormResult a
        f (FormSuccess (Just a)) = FormSuccess a
        f (FormSuccess Nothing) = FormFailure [msgRender MsgInvalidValue]
        f (FormFailure xs) = FormFailure xs
        f FormMissing = FormMissing

    (statusR,statusV) <- first (f . (readMaybe <$>)) <$> mreq hiddenField FieldSettings
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

    return ( Invoice <$> custR <*> emplR <*> noR <*> statusR <*> dayR <*> dueR
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


warn :: Text
warn = "warn"


info :: Text
info = "info"
