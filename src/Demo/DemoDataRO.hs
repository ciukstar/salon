{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataRO (populateRO) where

import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class (MonadIO (liftIO))
import ClassyPrelude.Yesod (ReaderT)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert_) )

import Model (User(User, userName, userPassword, userAdmin, userEmail, userFullName))
import Yesod.Auth.Util.PasswordStore (makePassword)

populateRO :: MonadIO m => ReaderT SqlBackend m ()
populateRO = do
    pass <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 pass
                   , userAdmin = True
                   , userFullName = Just "Popa Andrei Marin"
                   , userEmail = Just "pamarin@mail.ro"
                   }
    return ()
