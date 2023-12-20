{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where


import Import
import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Sql
    ( ConnectionPoolConfig
      ( ConnectionPoolConfig, connectionPoolConfigStripes
      , connectionPoolConfigIdleTimeout, connectionPoolConfigSize
      )
    )
import Database.Persist.Sqlite
    ( createSqlitePoolWithConfig, runSqlPool, sqlDatabase, sqlPoolSize )
import Language.Haskell.TH.Syntax (qLocation)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
    ( Settings, defaultSettings, defaultShouldDisplayException,runSettings
    , setHost, setOnException, setPort, getPort
    )
import Network.Wai.Middleware.RequestLogger
    ( Destination (Logger), IPAddrSource (..), OutputFormat (..), destination
    , mkRequestLogger, outputFormat
    )
import Network.Wai.Middleware.Gzip
    ( gzip, GzipSettings (gzipFiles), GzipFiles (GzipCompress) )
import System.Environment.Blank (getEnv)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)



import Demo.DemoDataFR (populateFR)
import Demo.DemoDataRO (populateRO)
import Demo.DemoDataRU (populateRU)
import Demo.DemoDataEN (populateEN)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import Handler.Scratch
    ( getScratchInitR
    , postScratchInitR
    , getScratchOneR
    , getScratchTwoR
    )

import Handler.Requests
    ( getRequestsR, getRequestR, postRequestR
    , getRequestsSearchR, postRequestApproveR
    , postRequestFinishR, postRequestAssignR
    , getRequestRescheduleR, getRequestHistR
    , getTasksCalendarR, getTasksDayListR
    , getTaskItemR, getTaskHistR
    )

import Handler.Appointments
    ( getAppointmentsR, getAppointmentsSearchR, getAppointmentR
    , postAppointmentR, postAppointmentCancelR, getAppointmentHistR
    , getAppointmentRescheduleR, postAppointmentApproveR
    , getBookingsCalendarR, getBookingsDayListR, getBookingItemR
    )

import Handler.Contacts (getContactR)
import Handler.Book
    ( getBookOffersR, postBookOffersR
    , getBookStaffR, postBookStaffR
    , getBookTimeR, postBookTimeR
    , getBookCustomerR, postBookCustomerR
    , getBookPayR, postBookPayR
    , getBookPayNowR, postBookPaymentIntentR
    , postBookPaymentIntentCancelR, getBookPayCompletionR
    , getBookEndR
    , getBookSearchR, postBookSearchR
    )
import Handler.AboutUs (getAboutUsR)
import Handler.Services
    ( getServicesR, getServiceThumbnailR
    , getServiceR, postServiceR
    , getServiceOffersR, getOfferR
    , getServicesSearchR, getServiceSearchR
    , getServiceSearchOffersR, getOfferSearchR
    )
import Handler.Account
    ( getProfileR, getAccountR, postAccountR, getAccountPhotoR
    , getProfileEditR, postProfileR, postProfileRemoveR
    )
import Handler.Home (getHomeR)
import Handler.Resources (getDocsR)

import Handler.Stats
    ( getPopOffersR, getWorkloadsR, getWorkloadEmplMonthR, getWorkloadEmplDayR
    , getStatsAovR, getAovDetailsR
    )

import Admin.Billing
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
    , getAdmInvoiceSendmailR
    , postAdmInvoiceSendmailR
    , getAdmInvoiceMailR
    , postAdmInvoiceMailDeleteR
    , getBillingMailHookR
    )

import Admin.Business
  ( getBusinessR, postBusinessR, getBusinessCreateR
  , getBusinessEditR, postBusinessEditR, postBusinessDeleteR
  , getBusinessHoursR, postBusinessHoursR, getBusinessHoursCreateR
  , getBusinessTimeSlotR, postBusinessTimeSlotDeleteR, getBusinessHoursEditR
  , postBusinessTimeSlotR, getBusinessCalendarR, getBusinessCalendarSlotsR
  , getBusinessCalendarSlotCreateR, postBusinessCalendarSlotCreateR
  , getBusinessCalendarSlotEditR, postBusinessCalendarSlotEditR
  , postBusinessCalendarSlotDeleteR, getBusinessCalendarSlotR
  , getBusinessAboutR, postBusinessAboutR, getBusinessAboutCreateR
  , getBusinessAboutEditR, postBusinessAboutEditR, postBusinessAboutDeleteR
  , getBusinessContactR, postBusinessContactR, getBusinessContactCreateR
  , getBusinessContactEditR, postBusinessContactEditR, postBusinessContactDeleteR
  , getBrandR, getBrandMarkR, getBrandIcoR, postBrandR, getBrandEditR
  , postBrandEditR, postBrandDeleteR, getBrandCreateR
  )

import Admin.Users
    ( getUsersR
    , getUserCreateFormR
    , postUsersR
    , getUserR
    , postUserR
    , getUserEditFormR
    , postUserDeleteR
    , getUserPwdResetR
    , postUserPwdResetR
    , getUsersSearchR
    )

import Admin.Staff
    ( getAdmStaffR, getAdmStaffCreateR, getAdmStaffPhotoR
    , getAdmEmplR, postAdmEmplR, getAdmStaffEditR, getAdmScheduleR
    , postAdmStaffR, postAdmStaffDeleteR, postAdmRolesR, getAdmRolesR
    , getAdmRoleR, postAdmRoleR, getAdmRoleCreateR, getAdmRoleEditR
    , postAdmRoleDeleteR, getAdmEmplUserR, postAdmEmplUserR
    , postAdmEmplUnregR, getAdmStaffSearchR, getAdmScheduleCreateR
    , postAdmScheduleR, getAdmTimeSlotR, postAdmTimeSlotR, getAdmScheduleEditR
    , postAdmScheduleDeleteR, getAdmEmplCalendarR, getEmplCalendarSlotR
    , getEmplCalendarSlotEditR, postEmplCalendarSlotDeleteR
    , getEmplCalendarSlotCreateR, postEmplCalendarSlotsR, postEmplCalendarSlotR
    , getEmplCalendarSlotsR
    )
    
import Admin.Services
    ( getAdmServicesR
    , getAdmServiceCreateFormR
    , postAdmServiceR
    , getAdmServiceEditFormR
    , postAdmServicesR
    , postAdmServiceDeleteR
    , getAdmServiceImageR
    , getAdmOfferCreateR
    , postAdmOfferR
    , getAdmPriceR
    , postAdmPriceR
    , getAdmPriceEditR
    , postAdmPriceDeleteR
    , getAdmServicesSearchR
    , getAdmExpertCreateR
    , postAdmExpertsR
    , getAdmExpertR
    , postAdmExpertR
    , getAdmExpertEditR
    , postAdmExpertDeleteR
    )
import Handler.Common
    ( getFaviconR, getRobotsR, getPhotoPlaceholderR
    )


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePoolWithConfig
        (sqlDatabase $ appDatabaseConf appSettings)
        ConnectionPoolConfig { connectionPoolConfigStripes = 1
                             , connectionPoolConfigIdleTimeout = appIdleTimeout appSettings
                             , connectionPoolConfigSize = sqlPoolSize $ appDatabaseConf appSettings
                             }

    -- Perform database migration using our application's logging settings.
    flip runLoggingT logFunc $ flip runSqlPool pool $ do
        runMigration migrateAll
        demo <- liftIO $ getEnv "YESOD_DEMO_LANG"
        case demo of
          Just "FR" -> populateFR
          Just "RO" -> populateRO
          Just "RU" -> populateRU
          _         -> populateEN

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging $ gzip def { gzipFiles = GzipCompress } appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
