{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

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
  , postAdmInvoiceSendmailR
  , getBillingMailHookR
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( tryAny, SomeException (SomeException), Exception (fromException))
import Control.Lens ((?~), sumOf, folded, to)
import qualified Control.Lens as L ((^.), (^?))
import Control.Monad (join, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Lens (AsValue(_String), key, AsNumber (_Integer))
import Data.Bifunctor (Bifunctor(first, second))
import Data.ByteString (toStrict)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.ByteString.Base64.Lazy (encode)
import Data.Complex (Complex ((:+)))
import Data.Fixed (Centi)
import Data.Function ((&))
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Graphics.PDF
    ( pdfByteString, PDFDocumentInfo (author), standardViewerPrefs
    , PDFRect (PDFRect), PDF, addPage, drawWithPage, AnyFont, black
    , Rectangle (Rectangle), PDFFont (PDFFont), mkStdFont, startNewLine
    , FontName (Times_Roman, Times_Bold), setFont, drawText, textStart
    , displayText
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
import Text.Blaze.Html (preEscapedToHtml, toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Read (readMaybe)
import Text.Hamlet (Html, HtmlUrlI18n, ihamlet)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, SomeMessage (SomeMessage)
    , MonadHandler (liftHandler), redirect, addMessageI, getMessages
    , getCurrentRoute, lookupPostParam, whamlet, RenderMessage (renderMessage)
    , getYesod, languages, lookupSession, getUrlRender, setSession
    , getMessageRender, getUrlRenderParams
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Functions
    ( generateFormPost, mreq, mopt, runFormPost, checkM
    )
import Yesod.Form.Input (runInputGet, iopt, ireq)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess), Field
    , FieldView (fvInput, fvErrors, fvLabel, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Form.Fields
    ( dayField, hiddenField, unTextarea, intField, doubleField, textField
    , textareaField, Textarea, emailField
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
      , AdmInvoiceSendmailR
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
      , MsgSubjectEmail, MsgMessageSent, MsgMessageNotSent, MsgAppName
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
      , InvoiceMailTimemark, InvoiceMailInvoice
      )
    , Staff (Staff, staffName, staffEmail), InvoiceId, Business (Business)
    , ItemId
    , Item
      ( Item, itemOffer, itemQuantity, itemPrice, itemTax, itemVat, itemAmount
      , itemCurrency
      )
    , Offer (Offer, offerQuantity, offerPrice), Service (Service), Thumbnail
    , InvoiceMail
      ( InvoiceMail, invoiceMailStatus, invoiceMailTimemark, invoiceMailRecipient
      , invoiceMailSender, invoiceMailSubject, invoiceMailBody, invoiceMailInvoice
      , invoiceMailRecipientName, invoiceMailSenderName
      )
    , MailStatus (MailStatusDraft, MailStatusDelivered)
    , _itemAmount
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

    let _status = r L.^. responseStatus . statusCode

    let accessToken = r L.^. responseBody . key "access_token" . _String
    let refreshToken = r L.^. responseBody . key "refresh_token" . _String
    let _tokenType = r L.^. responseBody . key "token_type" . _String
    let _scope = r L.^. responseBody . key "scope" . _String
    let _expiresIn = r L.^? responseBody . key "expires_in" . _Integer

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
        return (fst . fst . fst <$> p,snd . fst <$> p,snd <$> p,snd . fst . fst <$> p)
        
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
          let html = renderIvoiceBody customer employee invoice items msgRndr urlRndr
              pdf = renderIvoicePdf (invoicePdf customer employee invoice items transMsg (PDFRect 0 0 612 792) fr fb)
              mail = buildMail imail html pdf
          raw <- liftIO $ decodeUtf8 . toStrict . encode <$> renderMail' mail

          let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken)
          _ <- liftIO $ postWith opts gmailApi (object ["raw" .= raw])
          addMessageI info MsgMessageSent
          redirect $ AdminR $ AdmInvoiceR (invoiceMailInvoice imail)
      _otherwise -> do
          addMessageI info MsgMessageNotSent
          redirect $ AdminR AdmInvoicesR


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
    
    ((fr2,_),_) <- runFormPost $ formInvoiceSendmail customer employee invoice
    case fr2 of
      FormSuccess (recipient,sender,subj,body) -> do

          now <- liftIO getCurrentTime

          let imail = InvoiceMail
                  { invoiceMailInvoice = iid
                  , invoiceMailStatus = MailStatusDraft
                  , invoiceMailTimemark = now
                  , invoiceMailRecipient = recipient
                  , invoiceMailRecipientName = (userFullName . entityVal =<< customer)
                                               <|> (userName . entityVal <$> customer)
                  , invoiceMailSender = sender
                  , invoiceMailSenderName = staffName . entityVal <$> employee
                  , invoiceMailSubject = subj
                  , invoiceMailBody = unTextarea body
                  }

          mid <- runDB $ insert imail
          app <- getYesod
          langs <- languages
          
          let transMsg = renderMessage app langs :: AppMessage -> Text
          let googleClientId = appGoogleClientId $ appSettings app
              googleClientSecret = appGoogleClientSecret $ appSettings app

          accessToken <- lookupSession gmailAccessToken
          refreshToken <- lookupSession gmailRefreshToken
          
          timesRoman <- liftIO $ mkStdFont Times_Roman
          timesBold <- liftIO $ mkStdFont Times_Bold

          case ((accessToken,refreshToken),(timesRoman,timesBold)) of
            ((Just atoken,Just rtoken),(Right fr,Right fb)) -> do
                msgRndr <- (toHtml .) <$> getMessageRender
                urlRndr <- getUrlRenderParams
                let html = renderIvoiceBody customer employee invoice items msgRndr urlRndr
                    pdf = renderIvoicePdf (invoicePdf customer employee invoice items transMsg (PDFRect 0 0 612 792) fr fb)
                    mail = buildMail imail html pdf
                    
                raw <- liftIO $ decodeUtf8 . toStrict . encode <$> renderMail' mail

                let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 atoken)
                response <- liftIO $ tryAny $ postWith opts gmailApi (object ["raw" .= raw])


                case response of
                  Left e@(SomeException _) -> case fromException e of
                    Just (HttpExceptionRequest _ (StatusCodeException r' _)) -> do
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
                                  gmailApi
                                  (object ["raw" .= raw])

                              now' <- liftIO getCurrentTime
                              runDB $ update $ \x -> do
                                  set x [ InvoiceMailStatus =. val MailStatusDelivered
                                        , InvoiceMailTimemark =. val now'
                                        ]
                                  where_ $ x ^. InvoiceMailId ==. val mid
                              addMessageI info MsgMessageSent
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


buildMail :: InvoiceMail -> Html -> L.ByteString -> Mail
buildMail imail html pdf = simpleMailInMemory
    (Address (invoiceMailRecipientName imail) (invoiceMailRecipient imail))
    (Address (invoiceMailSenderName imail) (invoiceMailSender imail))
    (invoiceMailSubject imail)
    (fromStrict $ invoiceMailBody imail)
    (renderHtml html)
    [("application/pdf", "invoice.pdf", pdf)] 


invoicePdf :: Maybe (Entity User)
           -> Maybe (Entity Staff)
           -> Maybe (Entity Invoice)
           -> [Entity Item]
           -> (AppMessage -> Text)
           -> PDFRect -> AnyFont -> AnyFont -> PDF ()
invoicePdf customer employee invoice items trans (PDFRect x _ _ y') fontr fontb = do
    
    let tfont = PDFFont fontb 32
        hfont = PDFFont fontb 18
        rfont = PDFFont fontr 14
        bfont = PDFFont fontb 14
        sfont = PDFFont fontr 10
    
    let mt@ml = 72 :: Double
    
    page <- addPage Nothing
    drawWithPage page $ do
        drawText $ do
            setFont tfont
            textStart (x + ml) (y' - mt)
            displayText $ trans MsgAppName
        drawText $ do
            startNewLine
            setFont hfont
            textStart (x + ml) (y' - mt - 1 * 30)
            displayText $ trans MsgInvoice
        drawText $ do
            startNewLine
            setFont bfont
            textStart (x + ml) (y' - mt - 2 * 30)
            displayText $ trans MsgInvoiceNumber
            displayText ": "
            setFont rfont
            displayText $ maybe "" (pack . show . invoiceNumber . entityVal) invoice
        drawText $ do
            startNewLine
            setFont bfont
            textStart (x + ml) (y' - mt - 3 * 30)
            displayText $ trans MsgBillTo
            displayText ": "
            setFont rfont
            displayText $ fromMaybe "" (((userFullName . entityVal) =<< customer) <|> userName . entityVal <$> customer)
            setFont sfont
            displayText $ maybe "" (\email -> " (" <> email <> ")") ((userEmail . entityVal) =<< customer)
        drawText $ do
            startNewLine
            setFont bfont
            textStart (x + ml) (y' - mt - 4 * 30)
            displayText $ trans MsgBilledFrom
            displayText ": "
            setFont rfont
            displayText $ maybe "" (staffName . entityVal) employee
            setFont sfont
            displayText $ maybe "" (\email -> " (" <> email <> ")") ((staffEmail . entityVal) =<< employee)
        drawText $ do
            startNewLine
            setFont bfont
            textStart (x + ml) (y' - mt - 5 * 30)
            displayText $ trans MsgInvoiceDate
            displayText ": "
            setFont rfont
            displayText $ maybe "" (pack . show . invoiceDay . entityVal) invoice
        drawText $ do
            startNewLine
            setFont bfont
            textStart (x + ml) (y' - mt - 6 * 30)
            displayText $ trans MsgDueDate
            displayText ": "
            setFont rfont
            displayText $ maybe "" (pack . show) (invoiceDueDay . entityVal =<< invoice)
        drawText $ do
            startNewLine
            setFont bfont
            textStart (x + ml) (y' - mt - 7 * 30)
            displayText $ trans MsgStatus
            displayText ": "
            setFont rfont
            displayText $ maybe "" (trans . (\case
                                                  InvoiceStatusDraft -> MsgDraft
                                                  InvoiceStatusOpen -> MsgOpen
                                                  InvoiceStatusPaid -> MsgPaid
                                                  InvoiceStatusUncollectible -> MsgUncollectible
                                                  InvoiceStatusVoid -> MsgVoid
                                            ) . invoiceStatus . entityVal) invoice
        drawText $ do
            startNewLine
            setFont bfont
            textStart (x + ml) (y' - mt - 8 * 30)
            displayText $ trans MsgAmount
            displayText ": "
            setFont rfont
            displayText $ pack $ show (items & sumOf (folded . to entityVal . _itemAmount))
            
        drawText $ do
            startNewLine
            setFont hfont
            textStart (x + ml) (y' - mt - 9 * 30)
            displayText $ trans MsgDetails
    
        let fontBold = Font bfont black black
            fontSmall = Font sfont black black

        let dx1 = x + ml
            dy1 = y' - mt - 10 * 30
            
        let (Rectangle (x0 :+ y0) _,hq) = drawTextBox dx1 dy1 (dx1 + 78) 30 NE NormalParagraph fontBold $ do
                setJustification LeftJustification
                paragraph $ txt $ trans MsgQuantity
            
        let (Rectangle (x1 :+ _) _,hp) = drawTextBox (x0 + 78) dy1 (x0 + 78) 30 NE NormalParagraph fontBold $ do
                setJustification LeftJustification
                paragraph $ txt $ trans MsgPrice
            
        let (Rectangle (x2 :+ _) _,ht) = drawTextBox (x1 + 78) dy1 (x1 + 78) 30 NE NormalParagraph fontBold $ do
                setJustification LeftJustification
                paragraph $ txt $ trans MsgTax
            
        let (Rectangle (x3 :+ _) _,hv) = drawTextBox (x2 + 78) dy1 (x2 + 78) 30 NE NormalParagraph fontBold $ do
                setJustification LeftJustification
                paragraph $ txt $ trans MsgVat
            
        let (Rectangle (x4 :+ _) _,ha) = drawTextBox (x3 + 78) dy1 (x3 + 78) 30 NE NormalParagraph fontBold $ do
                setJustification LeftJustification
                paragraph $ txt $ trans MsgAmount
            
        let (_,hc) = drawTextBox (x4 + 78) dy1 (x4 + 78) 30 NE NormalParagraph fontBold $ do
                setJustification LeftJustification
                paragraph $ txt $ trans MsgCurrency
        
        hq >> hp >> ht >> hv >> ha >> hc
        
        forM_ (zip [1 ..] items) $ \(i,Entity _ (Item _ _ q p t v a c)) -> do
            let (_,v1) = drawTextBox (x0 + 0 * 78) (y0 - i * 30) (x0 + 78) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ pack $ show q
                        
            let (_,v2) = drawTextBox (x0 + 1 * 78) (y0 - i * 30) (x0 + 78) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ pack $ show p
                        
            let (_,v3) = drawTextBox (x0 + 2 * 78) (y0 - i * 30) (x0 + 78) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (pack . show) t
                        
            let (_,v4) = drawTextBox (x0 + 3 * 78) (y0 - i * 30) (x0 + 78) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ maybe "" (pack . show) v
                        
            let (_,v5) = drawTextBox (x0 + 4 * 78) (y0 - i * 30) (x0 + 78) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ pack $ show a
                        
            let (_,v6) = drawTextBox (x0 + 5 * 78) (y0 - i * 30) (x0 + 78) 30 NE NormalParagraph fontSmall $ do
                    setJustification LeftJustification
                    paragraph $ txt $ fromMaybe "" c
                        
            v1 >> v2 >> v3 >> v4 >> v5 >> v6
            

renderIvoicePdf :: PDF () -> L.ByteString
renderIvoicePdf = pdfByteString
    standardDocInfo { author = "Sergiu Starciuc"
                    , subject = "Invoice"
                    , compressed = True
                    , viewerPreferences = standardViewerPrefs { displayDoctitle = True }
                    }
    (PDFRect 0 0 612 792)
    
    
renderIvoiceBody :: Maybe (Entity User)
                 -> Maybe (Entity Staff)
                 -> Maybe (Entity Invoice)
                 -> [Entity Item]
                 -> HtmlUrlI18n AppMessage (Route App)
renderIvoiceBody customer employee invoice items = [ihamlet|
<h1>_{MsgAppName}
<h2>_{MsgInvoice}
$maybe Entity _ (User uname _ _ _ _ _ cname cemail) <- customer
  $maybe Entity _ (Staff ename _ _ _ eemail _) <- employee
    $maybe Entity _ (Invoice _ _ no status day due) <- invoice
      <dl>
        <dt>_{MsgInvoiceNumber}
        <dd>#{no}

        <dt>_{MsgBillTo}
        <dd>
          $maybe name <- cname
            #{name}
          $nothing
            #{uname}
          $maybe email <- cemail
            <div>
              <small>#{email}

        <dt>_{MsgBilledFrom}
        <dd>
          #{ename}
          $maybe email <- eemail
            <div>
              <small>#{email}
          
        <dt>_{MsgInvoiceDate}
        <dd>#{show day}

        <dt>_{MsgDueDate}
        <dd>
          $maybe due <- due
            #{show due}

        <dt>_{MsgStatus}
        <dd>
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

        <dt>_{MsgAmount}
        <dd>#{show amount}

<h2>_{MsgDetails}
<table>
  <thead>
    <tr>
      <th>_{MsgQuantity}
      <th>_{MsgPrice}
      <th>_{MsgTax}
      <th>_{MsgVat}
      <th>_{MsgAmount}
      <th>_{MsgCurrency}
  <tbody>
  $forall Entity _ (Item _ _ q p t v a c) <- items
    <tr>
      <td>#{show q}
      <td>#{show p}
      <td>
        $maybe tax <- t
          #{show tax}
      <td>
        $maybe vat <- v
          #{show vat}
      <td>#{show a}
      <td>
        $maybe currency <- c
          #{currency}
|]
    where
      amount = items & sumOf (folded . to entityVal . _itemAmount) 



    
gmailAccessToken :: Text
gmailAccessToken = "gmail_access_token"


gmailRefreshToken :: Text
gmailRefreshToken = "gmail_refresh_token"


gmailApi :: String
gmailApi = "https://gmail.googleapis.com/gmail/v1/users/me/messages/send"


formInvoiceSendmail :: Maybe (Entity User) -> Maybe (Entity Staff) -> Maybe (Entity Invoice)
                    -> Html -> MForm Handler (FormResult (Text,Text,Text,Textarea),Widget)
formInvoiceSendmail customer employee invoice extra = do
    (toR,toV) <- mreq emailField FieldSettings
        { fsLabel = SomeMessage MsgToEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (userEmail . entityVal =<< customer)
        
    (fromR,fromV) <- mreq emailField FieldSettings
        { fsLabel = SomeMessage MsgFromEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } (staffEmail . entityVal =<< employee)

    trans <- getYesod >>= \a -> languages >>= \l -> return (renderMessage a l)
        
    (subjectR,subjectV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgSubjectEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input")]
        } ( (((trans MsgInvoice <> " ") <> trans MsgNumberSign) <>)
            . pack . show . invoiceNumber . entityVal <$> invoice
          )
        
    (bodyR,bodyV) <- mreq textareaField FieldSettings
        { fsLabel = SomeMessage MsgBodyEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","mdc-text-field__input"),("rows","8")]
        } Nothing

    let r = (,,,) <$> toR <*> fromR <*> subjectR <*> bodyR
    let w = [whamlet|
#{extra}
$forall v <- [toV,fromV,subjectV]
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
|]
    return (r,w)


postAdmInvoiceItemDeleteR :: InvoiceId -> ItemId -> Handler Html
postAdmInvoiceItemDeleteR iid xid = do
    ((fr,_),_) <- runFormPost formItemDelete
    case fr of
      FormSuccess _ -> do
          runDB $ delete xid
          addMessageI info MsgRecordDeleted
          redirect $ AdminR $ AdmInvoiceItemsR iid
      _x -> do
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
        
    (fw2,et2) <- do
        (cust,empl,inv) <- do
            p <- runDB $ selectOne $ do
                x :& c :& e <- from $ table @Invoice
                    `innerJoin` table @User `on` (\(x :& c) -> x ^. InvoiceCustomer ==. c ^. UserId)
                    `innerJoin` table @Staff `on` (\(x :& _ :& e) -> x ^. InvoiceStaff ==. e ^. StaffId)
                where_ $ x ^. InvoiceId ==. val iid
                return ((c,e),x)
            return (fst . fst <$> p,snd . fst <$> p,snd <$> p)
        generateFormPost $ formInvoiceSendmail cust empl inv
    
    dlgInvoiceSendmail <- newIdent
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
