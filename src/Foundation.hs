{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Foundation where

import Import.NoFoundation
import Data.Kind            (Type)
import qualified Data.Text as T (pack)
import Data.Time.Calendar.Month (Month)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Yesod.Auth.HashDB (authHashDBWithForm)
import Yesod.Auth.Message
    ( AuthMessage(InvalidLogin)
    , englishMessage, frenchMessage, russianMessage, defaultMessage
    )
import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.French (frenchFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)
import qualified Data.List.Safe as LS
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey)
import qualified Database.Esqueleto.Experimental as E ((==.), exists)
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, Value (Value), where_
    , (^.), (:&) ((:&))
    , just, orderBy, asc, unionAll_, not_, val
    )

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        langs <- languages
        let lang = fromMaybe "en" . LS.head $ langs
        pc <- widgetToPageContent $ do
            $(widgetFile "default-layout")

        brand <- runDB $ selectOne $ from $ table @Brand

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized PhotoPlaceholderR _ = return Authorized

    isAuthorized (StaticR _) _ = return Authorized

    isAuthorized (StatsR PopOffersR) _ = return Authorized
    isAuthorized (StatsR WorkloadsR) _ = return Authorized
    isAuthorized (StatsR (WorkloadEmplMonthR _ _)) _ = return Authorized
    isAuthorized (StatsR (WorkloadEmplDayR _ _)) _ = return Authorized
    isAuthorized (StatsR StatsAovR) _ = return Authorized
    isAuthorized (StatsR (AovDetailsR {})) _ = return Authorized
    
        
        
    isAuthorized (AdminR UsersR) _ = return Authorized
    isAuthorized (AdminR UserCreateFormR) _ = return Authorized
    isAuthorized (AdminR AdmServicesSearchR) _ = return Authorized
    isAuthorized (AdminR (AdmServicesR _)) _ = return Authorized
    isAuthorized (AdminR (AdmServiceR _)) _ = return Authorized
    isAuthorized (AdminR (AdmServiceDeleteR _)) _ = return Authorized
    isAuthorized (AdminR (AdmServiceCreateFormR _)) _ = return Authorized
    isAuthorized (AdminR (AdmServiceEditFormR _)) _ = return Authorized
    isAuthorized (AdminR (AdmServiceImageR _)) _ = return Authorized
    isAuthorized (AdminR (AdmOfferCreateR _)) _ = return Authorized
    isAuthorized (AdminR (AdmExpertCreateR _)) _ = return Authorized
    isAuthorized (AdminR (AdmExpertsR _)) _ = return Authorized
    isAuthorized (AdminR (AdmExpertR _ _)) _ = return Authorized
    isAuthorized (AdminR (AdmExpertEditR _ _)) _ = return Authorized
    isAuthorized (AdminR (AdmExpertDeleteR _ _)) _ = return Authorized

    isAuthorized (AdminR (AdmOfferR _)) _ = return Authorized
    isAuthorized (AdminR (AdmPriceR _ _)) _ = return Authorized
    isAuthorized (AdminR (AdmPriceEditR _ _)) _ = return Authorized
    isAuthorized (AdminR (AdmPriceDeleteR _ _)) _ = return Authorized
    isAuthorized (AdminR AdmStaffR) _ = return Authorized
    isAuthorized (AdminR AdmStaffCreateR) _ = return Authorized
    isAuthorized (AdminR (AdmStaffPhotoR _)) _ = return Authorized
    isAuthorized (AdminR (AdmEmplR _)) _ = return Authorized
    isAuthorized (AdminR (AdmStaffEditR _)) _ = return Authorized
    isAuthorized (AdminR (AdmStaffDeleteR _)) _ = return Authorized
    isAuthorized (AdminR (AdmRolesR _)) _ = return Authorized
    isAuthorized (AdminR (AdmRoleR _ _)) _ = return Authorized
    isAuthorized (AdminR (AdmRoleCreateR _)) _ = return Authorized
    isAuthorized (AdminR (AdmRoleEditR _ _)) _ = return Authorized
    isAuthorized (AdminR (AdmRoleDeleteR _ _)) _ = return Authorized
    isAuthorized (AdminR (AdmScheduleCreateR _)) _ = return Authorized
    isAuthorized (AdminR (AdmScheduleR _)) _ = return Authorized
    isAuthorized (AdminR (AdmTimeSlotR _ _)) _ = return Authorized
    isAuthorized (AdminR (AdmScheduleEditR _ _)) _ = return Authorized
    isAuthorized (AdminR (AdmScheduleDeleteR _ _)) _ = return Authorized

    isAuthorized (AdminR AdmContactsR) _ = return Authorized
    isAuthorized (AdminR AdmContactsCreateR) _ = return Authorized
    isAuthorized (AdminR (AdmContactsEditR _)) _ = return Authorized
    isAuthorized (AdminR (AdmContactsDeleteR _)) _ = return Authorized
    isAuthorized (AdminR (UserEditFormR _)) _ = return Authorized
    isAuthorized (AdminR (UserR _)) _ = return Authorized
    isAuthorized (AdminR (UserDeleteR _)) _ = return Authorized
    isAuthorized (AdminR (UserPwdResetR _)) _ = return Authorized
    isAuthorized (AdminR UsersSearchR) _ = return Authorized
    isAuthorized (AdminR (AdmEmplUserR _)) _ = return Authorized
    isAuthorized (AdminR (AdmEmplUnregR _ _)) _ = return Authorized
    isAuthorized (AdminR AdmStaffSearchR) _ = return Authorized
    isAuthorized (AdminR (AdmEmplCalendarR _ _)) _ = return Authorized
    isAuthorized (AdminR (EmplCalendarSlotsR _ _)) _ = return Authorized
    isAuthorized (AdminR (EmplCalendarSlotR {})) _ = return Authorized
    isAuthorized (AdminR (EmplCalendarSlotEditR {})) _ = return Authorized
    isAuthorized (AdminR (EmplCalendarSlotDeleteR {})) _ = return Authorized
    isAuthorized (AdminR (EmplCalendarSlotCreateR {})) _ = return Authorized

    
    isAuthorized (AdminR BrandR) _ = return Authorized
    isAuthorized (AdminR BrandDeleteR) _ = return Authorized
    isAuthorized (AdminR (BrandEditR _)) _ = return Authorized
    isAuthorized (AdminR (BrandMarkR _)) _ = return Authorized
    isAuthorized (AdminR (BrandIcoR _)) _ = return Authorized
    isAuthorized (AdminR BrandCreateR) _ = return Authorized

    isAuthorized (AdminR BusinessR) _ = return Authorized
    isAuthorized (AdminR BusinessCreateR) _ = return Authorized
    isAuthorized (AdminR (BusinessEditR _)) _ = return Authorized
    isAuthorized (AdminR BusinessDeleteR) _ = return Authorized
    isAuthorized (AdminR (BusinessHoursR _)) _ = return Authorized
    isAuthorized (AdminR (BusinessCalendarR _ _)) _ = return Authorized
    isAuthorized (AdminR (BusinessHoursCreateR _)) _ = return Authorized
    isAuthorized (AdminR (BusinessTimeSlotR _ _)) _ = return Authorized
    isAuthorized (AdminR (BusinessTimeSlotDeleteR _ _)) _ = return Authorized
    isAuthorized (AdminR (BusinessHoursEditR _ _)) _ = return Authorized
    isAuthorized (AdminR (BusinessCalendarSlotsR _ _)) _ = return Authorized
    isAuthorized (AdminR (BusinessCalendarSlotR {})) _ = return Authorized
    isAuthorized (AdminR (BusinessCalendarSlotCreateR _ _)) _ = return Authorized
    isAuthorized (AdminR (BusinessCalendarSlotEditR {})) _ = return Authorized
    isAuthorized (AdminR (BusinessCalendarSlotDeleteR {})) _ = return Authorized
    isAuthorized (AdminR (BusinessAboutR _)) _ = return Authorized
    isAuthorized (AdminR (BusinessAboutCreateR _)) _ = return Authorized
    isAuthorized (AdminR (BusinessAboutEditR _ _)) _ = return Authorized
    isAuthorized (AdminR (BusinessAboutDeleteR _ _)) _ = return Authorized
    isAuthorized (AdminR (BusinessContactR _)) _ = return Authorized
    
    
    isAuthorized ContactR _ = return Authorized

    isAuthorized BookEndR _ = return Authorized
    isAuthorized BookCustomerR _ = return Authorized
    isAuthorized BookTimeR _ = return Authorized
    isAuthorized BookStaffR _ = return Authorized
    isAuthorized BookOffersR _ = return Authorized
    isAuthorized BookSearchR _ = return Authorized

    isAuthorized AppointmentsR _ = return Authorized
    isAuthorized AppointmentsSearchR _ = return Authorized
    isAuthorized (AppointmentR _) _ = return Authorized
    isAuthorized (AppointmentCancelR _) _ = return Authorized
    isAuthorized (AppointmentHistR _) _ = return Authorized
    isAuthorized (AppointmentRescheduleR _) _ = return Authorized
    isAuthorized (AppointmentApproveR _) _ = return Authorized


    isAuthorized RequestsR _ = return Authorized
    isAuthorized (RequestR _) _ = return Authorized
    isAuthorized RequestsSearchR _ = return Authorized
    isAuthorized (RequestApproveR _) _ = return Authorized
    isAuthorized (RequestFinishR _) _ = return Authorized
    isAuthorized (RequestAssignR _ _) _ = return Authorized
    isAuthorized (RequestRescheduleR _) _ = return Authorized
    isAuthorized (RequestHistR _) _ = return Authorized


    isAuthorized AccountR _ = return Authorized
    isAuthorized (AccountPhotoR _) _ = return Authorized
    isAuthorized ProfileR _ = return Authorized

    isAuthorized ServicesR _ = return Authorized
    isAuthorized (ServiceR _) _ = return Authorized
    isAuthorized (ServiceOffersR _) _ = return Authorized
    isAuthorized (ServiceThumbnailR _) _ = return Authorized
    isAuthorized (OfferR _ _) _ = return Authorized
    isAuthorized ServicesSearchR _ = return Authorized
    isAuthorized (ServiceSearchR _) _ = return Authorized
    isAuthorized (ServiceSearchOffersR _) _ = return Authorized
    isAuthorized (OfferSearchR _ _) _ = return Authorized

    isAuthorized AboutUsR _ = return Authorized
    isAuthorized (ResourcesR DocsR) _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR

    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR

    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App) => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        return $ case x of
            Just (Entity uid _) -> Authenticated uid
            Nothing -> UserError InvalidLogin

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authHashDBWithForm formLogin (Just . UniqueUser)]

    renderAuthMessage :: App -> [Text] -> AuthMessage -> Text
    renderAuthMessage _ [] = defaultMessage
    renderAuthMessage _ ("en":_) = englishMessage
    renderAuthMessage _ ("fr":_) = frenchMessage
    renderAuthMessage _ ("ru":_) = russianMessage
    renderAuthMessage app (_:xs) = renderAuthMessage app xs


-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized


formLogin :: Route App -> Widget
formLogin route = do
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$>  lookupSession ultDestKey
    msgs <- getMessages
    users <- liftHandler $ runDB $ select $ do
        x :& y <- from $
            do x <- from $ table @User
               where_ $ not_ $ E.exists $ do
                   e <- from $ table @Staff
                   where_ $ e ^. StaffUser E.==. just (x ^. UserId)
               where_ $ E.exists $ do
                   b <- from $ table @Book
                   where_ $ b ^. BookCustomer E.==. x ^. UserId
               return $ x :& val False
            `unionAll_`
            do x <- from $ table @User
               where_ $ E.exists $ do
                   e <- from $ table @Staff
                   where_ $ e ^. StaffUser E.==. just (x ^. UserId)
                   where_ $ e ^. StaffStatus E.==. val EmplStatusAvailable
               return $ x :& val True

        orderBy [asc y, asc (x ^. UserName)]
        return (x ^. UserId, x ^. UserName, y)
    loginFormWrapper <- newIdent
    loginForm <- newIdent
    pCreateAccount <- newIdent
    dlgSampleCreds <- newIdent
    $(widgetFile "login")


instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ [] = defaultFormMessage
    renderMessage _ ("en":_) = englishFormMessage
    renderMessage _ ("fr":_) = frenchFormMessage
    renderMessage _ ("ru":_) = russianFormMessage
    renderMessage app (_:xs) = renderMessage app xs


-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
