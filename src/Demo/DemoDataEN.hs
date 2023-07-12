{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataEN (populateEN) where

import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class (MonadIO (liftIO))
import ClassyPrelude.Yesod (ReaderT)
import Yesod.Auth.Util.PasswordStore (makePassword)
import Database.Persist.Sql (SqlBackend)
import Database.Persist ( PersistStoreWrite(insert_, insert) )

import Model
    ( User (User, userName, userPassword, userEmail, userFullName)
    , Service (Service, serviceName, servicePrice, serviceMu, serviceDescr, serviceGroup), Category (Category, categoryName, categoryImage, categoryDescr)
    )

populateEN :: MonadIO m => ReaderT SqlBackend m ()
populateEN = do
    password <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 password
                   , userFullName = Just "Smith James"
                   , userEmail = Just "jsmith@mail.en"
                   }

    s0001 <- insert $ Service { serviceName = "Hair care"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Hair Care"
                             , serviceGroup = Nothing
                             }

    s0011 <- insert $ Service { serviceName = "Men hair cuts"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Hair cuts for men"
                             , serviceGroup = Just s0001
                             }

    s0012 <- insert $ Service { serviceName = "Women hair cuts (above shoulders)"
                             , servicePrice = 28
                             , serviceMu = "$"
                             , serviceDescr = Just "Hair cuts above shoulders for women"
                             , serviceGroup = Just s0001
                             }

    s0013 <- insert $ Service { serviceName = "Women hair cuts (below shoulders)"
                             , servicePrice = 35
                             , serviceMu = "$"
                             , serviceDescr = Just "Hair cuts below shoulders for women"
                             , serviceGroup = Just s0001
                             }

    s0014 <- insert $ Service { serviceName = "Children hair cuts"
                             , servicePrice = 16
                             , serviceMu = "$"
                             , serviceDescr = Just "Hair cuts for children"
                             , serviceGroup = Just s0001
                             }

    s0002 <- insert $ Service { serviceName = "Facial Treatments"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Facial Treatments"
                             , serviceGroup = Nothing
                             }

    s0003 <- insert $ Service { serviceName = "Advanced Facial Treatments"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Advanced Facial Treatments"
                             , serviceGroup = Nothing
                             }

    s0004 <- insert $ Service { serviceName = "Anti-Aging Treatments"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Anti-Aging Treatments"
                             , serviceGroup = Nothing
                             }

    s0005 <- insert $ Service { serviceName = "Eye Treatment Center"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Eye Treatment Center"
                             , serviceGroup = Nothing
                             }

    s0006 <- insert $ Service { serviceName = "Body Massage"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Body Massage"
                             , serviceGroup = Nothing
                             }

    s0007 <- insert $ Service { serviceName = "Makeup Services"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Makeup Services"
                             , serviceGroup = Nothing
                             }

    s0008 <- insert $ Service { serviceName = "Waxing"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Waxing"
                             , serviceGroup = Nothing
                             }

    s0009 <- insert $ Service { serviceName = "Nail Care"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Nail Care"
                             , serviceGroup = Nothing
                             }

    s0010 <- insert $ Service { serviceName = "Body Shaping & Fitness"
                             , servicePrice = 26
                             , serviceMu = "$"
                             , serviceDescr = Just "Body Shaping & Fitness"
                             , serviceGroup = Nothing
                             }
        
    return ()
