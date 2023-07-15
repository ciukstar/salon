{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataRU (populateRU) where

import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class (MonadIO (liftIO))
import ClassyPrelude.Yesod (ReaderT)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert_) )

import Model (User(User, userName, userPassword, userEmail, userFullName))
import Yesod.Auth.Util.PasswordStore (makePassword)

populateRU :: MonadIO m => ReaderT SqlBackend m ()
populateRU = do
    pass <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 pass
                   , userFullName = Just "Иванов Иван Иванович"
                   , userEmail = Just "iiivanov@mail.ru"
                   }
    return ()