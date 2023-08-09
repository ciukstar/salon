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
{-# LANGUAGE InstanceSigs #-}

module Model where

import Data.Bool (Bool)
import Data.Function ((.))
import Data.Maybe (Maybe (Just))
import ClassyPrelude.Yesod
    ( Typeable , Text , ByteString , mkMigrate , mkPersist
    , persistFileWith , share , sqlSettings , Textarea
    )
import Database.Persist.Quasi (lowerCaseSettings)
import Data.Fixed (Centi)
import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))
import Yesod.Core.Dispatch (PathMultiPiece, toPathMultiPiece, fromPathMultiPiece)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Data.Text (pack, unpack)
import Text.Show (Show, show)
import Text.Read (Read, readMaybe)
import Data.Eq (Eq)
import Data.Functor ((<$>))
import Control.Monad (mapM)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
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


ultDestKey :: Text
ultDestKey = "_ULT"
