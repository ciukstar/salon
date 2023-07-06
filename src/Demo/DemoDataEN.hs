{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataEN (populateEN) where

import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class (MonadIO (liftIO))
import ClassyPrelude.Yesod (ReaderT)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert_) )

import Model (User(User, userIdent, userPassword))
import Yesod.Auth.Util.PasswordStore (makePassword)

populateEN :: MonadIO m => ReaderT SqlBackend m ()
populateEN = do
    pass <- liftIO $ makePassword "root" 17
    insert_ $ User { userIdent = "root", userPassword = Just $ decodeUtf8 pass}
    return ()
