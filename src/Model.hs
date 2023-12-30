{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeOperators              #-}

module Model where

import Control.Monad (Monad, mapM)
import Data.Ord (Ord)
import Data.Proxy (Proxy)
import Data.Time
    ( TimeOfDay, LocalTime, DiffTime, diffTimeToPicoseconds
    , picosecondsToDiffTime, localTimeToUTC, utc, utcToLocalTime
    , TimeZone (timeZoneMinutes), minutesToTimeZone, nominalDiffTimeToSeconds
    , secondsToNominalDiffTime
    )
import Data.Time.Calendar (Year, Day, DayOfWeek)
import Data.Time.Calendar.Month (Month (MkMonth))
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Prelude (Integer, Int, fromIntegral, Double)
import Data.Either (Either (Left, Right))
import Data.Bool (Bool (True))
import Data.Fixed ( Centi )
import Data.Function ((.),($))
import Data.Maybe (Maybe (Just))
import ClassyPrelude.Yesod
    ( Typeable , Text , ByteString , mkMigrate , mkPersist
    , persistFileWith , share , sqlSettings , Textarea
    , derivePersistField, PersistValue (PersistUTCTime)
    , truncate
    )
import Graphics.PDF
    ( pdfByteString, PDFDocumentInfo (author), standardViewerPrefs
    , PDFRect (PDFRect), PDF
    )
import Graphics.PDF.Document
    ( displayDoctitle, viewerPreferences, compressed, subject, standardDocInfo )
import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))
import Yesod.Core.Content
    ( ToContent (toContent), Content, ToTypedContent (toTypedContent)
    , HasContentType (getContentType), TypedContent (TypedContent)
    )
import Yesod.Core.Dispatch
    ( PathMultiPiece, toPathMultiPiece, fromPathMultiPiece
    , PathPiece, toPathPiece, fromPathPiece
    )
import Yesod.Core.Types (ContentType)
import Data.Text (pack, unpack)
import Text.Hamlet (Html)
import Text.Show (Show, show)
import Text.Read (Read, readMaybe, read)
import Data.Eq (Eq)
import Data.Functor ((<$>))
import Database.Esqueleto.Experimental (SqlString)
import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Types
    ( PersistValue (PersistInt64, PersistText)
    , SqlType (SqlDayTime, SqlInt64, SqlInt32, SqlString)
    )
import Database.Persist.Sql (fromSqlKey, toSqlKey, PersistFieldSql, sqlType)
import Control.Lens (makeLensesFor)


data AuthenticationType = UserAuthTypePassword | UserAuthTypeGoogle
    deriving (Show, Read, Eq, Ord)
derivePersistField "AuthenticationType"
                

data StoreType = StoreTypeGoogleSecretManager
               | StoreTypeDatabase
               | StoreTypeSession
    deriving (Show, Read, Eq, Ord)
derivePersistField "StoreType"


data PayMethod = PayAtVenue | PayNow
    deriving (Show, Read, Eq, Ord)
derivePersistField "PayMethod"


data DayType = Weekday | Weekend | Holiday
    deriving (Show, Read, Eq, Ord)
derivePersistField "DayType"


data MailStatus = MailStatusDraft | MailStatusBounced | MailStatusDelivered
  deriving (Show, Read, Eq)
derivePersistField "MailStatus"


data ServiceStatus = ServiceStatusPulished | ServiceStatusUnpublished
  deriving (Show, Read, Eq)

data Assignees = AssigneesMe | AssigneesNone | AssigneesOthers
    deriving (Show, Read, Eq)

data InvoiceStatus = InvoiceStatusDraft
                   | InvoiceStatusOpen
                   | InvoiceStatusPaid
                   | InvoiceStatusVoid
                   | InvoiceStatusUncollectible
    deriving (Show, Read, Eq, Ord)
derivePersistField "InvoiceStatus"

data BookStatus = BookStatusRequest
                | BookStatusAdjusted
                | BookStatusApproved
                | BookStatusCancelled
                | BookStatusPaid
    deriving (Show, Read, Eq, Ord)
derivePersistField "BookStatus"

data EmplStatus = EmplStatusAvailable | EmplStatusUnavailable
    deriving (Show, Read, Eq)
derivePersistField "EmplStatus"


instance PersistField DayOfWeek where
    toPersistValue :: DayOfWeek -> PersistValue
    toPersistValue x = PersistText (pack $ show x)

    fromPersistValue :: PersistValue -> Either Text DayOfWeek
    fromPersistValue (PersistText x) = Right (read $ unpack x)
    fromPersistValue _ = Left "Invalid DayOfWeek"


instance PersistFieldSql DayOfWeek where
    sqlType :: Proxy DayOfWeek -> SqlType
    sqlType _ = SqlString


instance PersistField Month where
    toPersistValue :: Month -> PersistValue
    toPersistValue (MkMonth x) = PersistInt64 (fromIntegral x)

    fromPersistValue :: PersistValue -> Either Text Month
    fromPersistValue (PersistInt64 x) = Right (MkMonth (fromIntegral x))
    fromPersistValue _ = Left "Invalid Year"


instance PersistFieldSql Month where
    sqlType :: Proxy Month -> SqlType
    sqlType _ = SqlInt32


instance PersistField Year where
    toPersistValue :: Year -> PersistValue
    toPersistValue x = PersistInt64 (fromIntegral x)

    fromPersistValue :: PersistValue -> Either Text Year
    fromPersistValue (PersistInt64 x) = Right (fromIntegral x)
    fromPersistValue _ = Left "Invalid Year"


instance PersistFieldSql Year where
    sqlType :: Proxy Year -> SqlType
    sqlType _ = SqlInt32


instance PersistField TimeZone where
  toPersistValue :: TimeZone -> PersistValue
  toPersistValue x = toPersistValue $ timeZoneMinutes x

  fromPersistValue :: PersistValue -> Either Text TimeZone
  fromPersistValue (PersistInt64 x) = Right $ minutesToTimeZone $ fromIntegral x
  fromPersistValue _ = Left "Invalid TimeZone"

instance PersistFieldSql TimeZone where
    sqlType :: Proxy TimeZone -> SqlType
    sqlType _ = SqlInt64


instance PersistField LocalTime where
    toPersistValue :: LocalTime -> PersistValue
    toPersistValue x = toPersistValue (localTimeToUTC utc x)

    fromPersistValue :: PersistValue -> Either Text LocalTime
    fromPersistValue (PersistUTCTime x) = Right (utcToLocalTime utc x)
    fromPersistValue _ = Left "Invalid LocalTime"

instance PersistFieldSql LocalTime where
    sqlType :: Proxy LocalTime -> SqlType
    sqlType _ = SqlDayTime


instance PersistField NominalDiffTime where
    toPersistValue :: NominalDiffTime -> PersistValue
    toPersistValue x = PersistInt64 (truncate (nominalDiffTimeToSeconds x))

    fromPersistValue :: PersistValue -> Either Text NominalDiffTime
    fromPersistValue (PersistInt64 x) = Right (secondsToNominalDiffTime (fromIntegral x))
    fromPersistValue _ = Left "Invalid NominalDiffTime"


instance PersistFieldSql NominalDiffTime where
    sqlType :: Proxy NominalDiffTime -> SqlType
    sqlType _ = SqlInt64


instance PersistField DiffTime where
    toPersistValue :: DiffTime -> PersistValue
    toPersistValue x = PersistInt64 (fromIntegral (diffTimeToPicoseconds x))

    fromPersistValue :: PersistValue -> Either Text DiffTime
    fromPersistValue (PersistInt64 x) = Right (picosecondsToDiffTime (fromIntegral x))
    fromPersistValue _ = Left "Invalid DiffTime"


instance PersistFieldSql DiffTime where
    sqlType :: Proxy DiffTime -> SqlType
    sqlType _ = SqlInt64


share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


makeLensesFor [ ("offerService", "_offerService")
              , ("offerName", "_offerName")
              , ("offerPublished", "_offerPublished")
              , ("offerPrice", "_offerPrice")
              , ("offerPrefix", "_offerPrefix")
              , ("offerSuffix", "_offerSuffix")
              , ("offerDescr", "_offerDescr")
              ] ''Offer

makeLensesFor [ ("serviceName","_serviceName")
              , ("servicePublished","_servicePublished")
              , ("serviceOverview","_serviceOverview")
              , ("serviceDuration","_serviceDuration")
              , ("serviceDescr","_serviceDescr")
              , ("serviceGroup","_serviceGroup")
              ] ''Service

makeLensesFor [ ("itemOffer","_itemOffer")
              , ("itemInvoice","_itemInvoice")
              , ("itemQuantity","_itemQuantity")
              , ("itemPrice","_itemPrice")
              , ("itemTax","_itemTax")
              , ("itemVat","_itemVat")
              , ("itemAmount","_itemAmount")
              , ("itemCurrency","_itemCurrency")
              ] ''Item


instance PathPiece Month where
    toPathPiece :: Month -> Text
    toPathPiece = pack . show

    fromPathPiece :: Text -> Maybe Month
    fromPathPiece = readMaybe . unpack


newtype Services = Services { unServices :: [ServiceId] }
    deriving (Show, Read, Eq)


instance PathMultiPiece Services where
    toPathMultiPiece :: Services -> [Text]
    toPathMultiPiece (Services xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Services
    fromPathMultiPiece xs = Services <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


instance HashDBUser User where
    userPasswordHash :: User -> Maybe Text
    userPasswordHash = userPassword

    setPasswordHash :: Text -> User -> User
    setPasswordHash h u = u { userPassword = Just h }


data SortOrder = SortOrderAsc | SortOrderDesc
    deriving (Eq, Show, Read)

    
instance SqlString Textarea


instance ToContent (PDF ()) where
    toContent :: PDF () -> Content
    toContent = toContent . pdfByteString
        standardDocInfo { author = "Starciuc Sergiu"
                        , subject = "Invoice"
                        , compressed = True
                        , viewerPreferences = standardViewerPrefs { displayDoctitle = True }
                        }
        (PDFRect 0 0 612 792)


instance ToTypedContent (PDF ()) where
    toTypedContent :: PDF () -> TypedContent
    toTypedContent = TypedContent mimePdf . toContent


instance HasContentType (PDF ()) where
    getContentType :: Monad m => m (PDF ()) -> ContentType
    getContentType _ = mimePdf


ultDestKey :: Text
ultDestKey = "_ULT"


mimePdf :: ContentType
mimePdf = "application/pdf"


gmail :: Text
gmail = "GMAIL_API"


gmailAccessToken :: Text
gmailAccessToken = "gmail_access_token"


gmailRefreshToken :: Text
gmailRefreshToken = "gmail_refresh_token"
