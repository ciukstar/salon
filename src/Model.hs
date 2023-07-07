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

import Data.Function ((.))
import Data.Maybe (Maybe (Just))
import ClassyPrelude.Yesod
    ( Typeable , Text , ByteString , mkMigrate , mkPersist
    , persistFileWith , share , sqlSettings , Textarea
    )
import Database.Persist.Quasi (lowerCaseSettings)
import Data.Fixed (Centi)
import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


instance HashDBUser User where
    userPasswordHash :: User -> Maybe Text
    userPasswordHash = Just . userPassword

    setPasswordHash :: Text -> User -> User
    setPasswordHash h u = u { userPassword = h } 


sessKeyULT :: Text
sessKeyULT = "_ULT"
