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
    , Service (Service, serviceName, servicePrice, serviceDescr, serviceGroup, servicePricePrefix, servicePriceSuffix)
    )

populateEN :: MonadIO m => ReaderT SqlBackend m ()
populateEN = do
    password <- liftIO $ makePassword "root" 17
    insert_ $ User { userName = "root"
                   , userPassword = decodeUtf8 password
                   , userFullName = Just "Smith James"
                   , userEmail = Just "jsmith@mail.en"
                   }

    s1 <- insert $ Service { serviceName = "Hair care"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Hair Care"
                             , serviceGroup = Nothing
                             }

    s11 <- insert $ Service { serviceName = "Men hair cuts"
                             , servicePrice = Just 26
                             , servicePricePrefix = Just "$"
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Hair cuts for men"
                             , serviceGroup = Just s1
                             }

    s12 <- insert $ Service { serviceName = "Women hair cuts (above shoulders)"
                             , servicePrice = Just 28
                             , servicePricePrefix = Just "$"
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Hair cuts above shoulders for women"
                             , serviceGroup = Just s1
                             }

    s13 <- insert $ Service { serviceName = "Women hair cuts (below shoulders)"
                             , servicePrice = Just 35
                             , servicePricePrefix = Just "$"
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Hair cuts below shoulders for women"
                             , serviceGroup = Just s1
                             }

    s14 <- insert $ Service { serviceName = "Children hair cuts"
                             , servicePrice = Just 16
                             , servicePricePrefix = Just "$"
                             , servicePriceSuffix = Just "-$20 (depending on the length of their hair)"
                             , serviceDescr = Just "Hair cuts for children"
                             , serviceGroup = Just s1
                             }

    s15 <- insert $ Service { serviceName = "Chemical services"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Prices will vary depending on the length of the client’s hair"
                             , serviceGroup = Just s1
                             }

    s151 <- insert $ Service { serviceName = "Conditioning"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Conditioning"
                             , serviceGroup = Just s15
                             }

    s1511 <- insert $ Service { serviceName = "After Perm Conditioner"
                              , servicePrice = Just 99
                              , servicePricePrefix = Just "$"
                              , servicePriceSuffix = Just " & up"
                              , serviceDescr = Just "After Perm Conditioner"
                              , serviceGroup = Just s151
                              }

    s1512 <- insert $ Service { serviceName = "Before Perm Conditioner"
                              , servicePrice = Just 110
                              , servicePricePrefix = Just "$"
                              , servicePriceSuffix = Just " & up"
                              , serviceDescr = Just "Before Perm Conditioner"
                              , serviceGroup = Just s151
                              }

    s152 <- insert $ Service { serviceName = "Highlights & Color"
                             , servicePrice = Nothing
                              , servicePricePrefix = Nothing
                              , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Highlights & Color"
                             , serviceGroup = Just s15
                             }

    s1521 <- insert $ Service { serviceName = "Full"
                              , servicePrice = Just 130
                              , servicePricePrefix = Just "$"
                              , servicePriceSuffix = Nothing
                              , serviceDescr = Just "Full"
                              , serviceGroup = Just s152
                              }

    s1522 <- insert $ Service { serviceName = "Partial"
                              , servicePrice = Just 68
                              , servicePricePrefix = Just "$"
                              , servicePriceSuffix = Just " & up"
                              , serviceDescr = Just "Partial"
                              , serviceGroup = Just s152
                              }

    s1523 <- insert $ Service { serviceName = "Permanent Color"
                              , servicePrice = Just 68
                              , servicePricePrefix = Just "$"
                              , servicePriceSuffix = Just " & up"
                              , serviceDescr = Just "Permanent Color"
                              , serviceGroup = Just s152
                              }

    s153 <- insert $ Service { serviceName = "Perm"
                             , servicePrice = Nothing
                              , servicePricePrefix = Nothing
                              , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Perm"
                             , serviceGroup = Just s15
                             }

    s1531 <- insert $ Service { serviceName = "Full Perm"
                              , servicePrice = Just 79
                              , servicePricePrefix = Just "$"
                              , servicePriceSuffix = Just " & up"
                              , serviceDescr = Just "Full Perm"
                              , serviceGroup = Just s153
                              }

    s1532 <- insert $ Service { serviceName = "Acid Repair Perm"
                              , servicePrice = Just 89
                              , servicePricePrefix = Just "$"
                              , servicePriceSuffix = Just " & up"
                              , serviceDescr = Just "Acid Repair Perm"
                              , serviceGroup = Just s153
                              }

    s1533 <- insert $ Service { serviceName = "Japanese Straightening Perm"
                              , servicePrice = Just 250
                              , servicePricePrefix = Just "$"
                              , servicePriceSuffix = Just " & up"
                              , serviceDescr = Just "Japanese Straightening Perm"
                              , serviceGroup = Just s153
                              }

    s2 <- insert $ Service { serviceName = "Facial Treatments"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Facial Treatments"
                             , serviceGroup = Nothing
                             }

    s3 <- insert $ Service { serviceName = "Advanced Facial Treatments"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Advanced Facial Treatments"
                             , serviceGroup = Nothing
                             }

    s4 <- insert $ Service { serviceName = "Anti-Aging Treatments"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Anti-Aging Treatments"
                             , serviceGroup = Nothing
                             }

    s5 <- insert $ Service { serviceName = "Eye Treatment Center"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Eye Treatment Center"
                             , serviceGroup = Nothing
                             }

    s6 <- insert $ Service { serviceName = "Body Massage"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Body Massage"
                             , serviceGroup = Nothing
                             }

    s7 <- insert $ Service { serviceName = "Makeup Services"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Makeup Services"
                             , serviceGroup = Nothing
                             }

    s8 <- insert $ Service { serviceName = "Waxing"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Waxing"
                             , serviceGroup = Nothing
                             }

    s9 <- insert $ Service { serviceName = "Nail Care"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Nail Care"
                             , serviceGroup = Nothing
                             }

    s10 <- insert $ Service { serviceName = "Body Shaping & Fitness"
                             , servicePrice = Nothing
                             , servicePricePrefix = Nothing
                             , servicePriceSuffix = Nothing
                             , serviceDescr = Just "Body Shaping & Fitness"
                             , serviceGroup = Nothing
                             }
        
    return ()
