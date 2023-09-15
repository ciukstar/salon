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

import Data.Proxy (Proxy)
import Data.Time
    ( Day, TimeOfDay, LocalTime, DiffTime, diffTimeToPicoseconds
    , picosecondsToDiffTime, localTimeToUTC, utc, utcToLocalTime
    , TimeZone (timeZoneMinutes), minutesToTimeZone
    )
import Prelude (Int, fromIntegral)
import Data.Either (Either (Left, Right))
import Data.Bool (Bool)
import Data.Function ((.),($))
import Data.Maybe (Maybe (Just))
import ClassyPrelude.Yesod
    ( Typeable , Text , ByteString , mkMigrate , mkPersist
    , persistFileWith , share , sqlSettings , Textarea
    , derivePersistField, PersistValue (PersistUTCTime), SqlType (SqlInt64)
    )
import Database.Persist.Quasi (lowerCaseSettings)
import Data.Fixed (Centi)
import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))
import Yesod.Core.Dispatch (PathMultiPiece, toPathMultiPiece, fromPathMultiPiece)
import Data.Text (pack, unpack)
import Text.Show (Show, show)
import Text.Read (Read, readMaybe)
import Data.Eq (Eq)
import Data.Functor ((<$>))
import Control.Monad (mapM)
import Database.Esqueleto.Experimental (SqlString)
import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Types
    ( PersistValue (PersistInt64)
    , SqlType (SqlDayTime)
    )
import Database.Persist.Sql (fromSqlKey, toSqlKey, PersistFieldSql, sqlType)


data ServiceStatus = ServiceStatusPulished | ServiceStatusUnpublished
  deriving (Show, Read, Eq)

data EmplStatus = EmplStatusEmployed | EmplStatusDismissed
    deriving (Show, Read, Eq)
derivePersistField "EmplStatus"


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


newtype Services = Services { unServices :: [ServiceId] }
    deriving (Show, Read, Eq)

instance PathMultiPiece Services where
    toPathMultiPiece :: Services -> [Text]
    toPathMultiPiece (Services xs) = pack . show . fromSqlKey <$> xs

    fromPathMultiPiece :: [Text] -> Maybe Services
    fromPathMultiPiece xs = Services <$> mapM ((toSqlKey <$>) . readMaybe . unpack) xs


instance HashDBUser User where
    userPasswordHash :: User -> Maybe Text
    userPasswordHash = Just . userPassword

    setPasswordHash :: Text -> User -> User
    setPasswordHash h u = u { userPassword = h } 

instance SqlString Textarea


ultDestKey :: Text
ultDestKey = "_ULT"
