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

import Control.Lens (filtered, folded, _2, (^?), to)
import Control.Monad.Logger (LogSource)
import Import.NoFoundation
import Data.Aeson.Lens (key, AsValue (_String))
import qualified Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.List.Safe as LS (head)
import Data.Kind            (Type)
import qualified Data.Text as T (pack)
import Data.Time.Calendar.Month (Month)
import qualified Network.Wreq as W (get, responseHeader, responseBody)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

import Yesod.Auth.OAuth2.Google (oauth2GoogleScopedWidget)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Yesod.Auth.HashDB (authHashDBWithForm)
import Yesod.Auth.Message
    ( AuthMessage(InvalidLogin), englishMessage, frenchMessage
    , russianMessage, defaultMessage
    )
import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.French (frenchFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)
import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey)
import qualified Database.Esqueleto.Experimental as E ((==.), exists)
import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, Value (unValue), where_
    , (^.), (:&) ((:&))
    , just, orderBy, asc, unionAll_, not_, val, isNothing_
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

    isAuthorized :: Route App -> Bool -> Handler AuthResult

    isAuthorized (ScratchR ScratchTwoR) _ = return Authorized
    isAuthorized (ScratchR ScratchOneR) _ = return Authorized
    isAuthorized (ScratchR ScratchInitR) _ = return Authorized

    isAuthorized (StaticR _) _ = return Authorized

    isAuthorized (AuthR _) _ = return Authorized

    isAuthorized WebAppManifestR _ = return Authorized
    isAuthorized SitemapR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized PhotoPlaceholderR _ = return Authorized

    isAuthorized r@(StatsR PopOffersR) _               = setUltDest r >> isAnalyst
    isAuthorized r@(StatsR WorkloadsR) _               = setUltDest r >> isAnalyst
    isAuthorized r@(StatsR (WorkloadEmplMonthR _ _)) _ = setUltDest r >> isAnalyst
    isAuthorized r@(StatsR (WorkloadEmplDayR _ _)) _   = setUltDest r >> isAnalyst
    isAuthorized r@(StatsR StatsAovR) _                = setUltDest r >> isAnalyst
    isAuthorized r@(StatsR (AovDetailsR {})) _         = setUltDest r >> isAnalyst


    isAuthorized BillingMailHookR _ = return Authorized
    isAuthorized (AdminR GMailApiHookR) _ = return Authorized

    isAuthorized (AdminR TokensGMailClearR) _ = isAdmin
    isAuthorized (AdminR TokensGMailR) _ = isAdmin
    isAuthorized r@(AdminR TokensR) _ = setUltDest r >> isAdmin

    isAuthorized (AdminR (AdmInvoiceMailDeleteR _ _)) _ = isAdmin
    isAuthorized (AdminR (AdmInvoiceMailR _ _)) _ = isAdmin
    isAuthorized (AdminR (AdmInvoiceSendmailR _)) _ = isAdmin
    isAuthorized (AdminR (AdmInvoiceItemDeleteR _ _)) _ = isAdmin
    isAuthorized r@(AdminR (AdmInvoiceItemEditR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmInvoiceItemCreateR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmInvoiceItemR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmInvoiceItemsR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmInvoiceDeleteR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmInvoiceEditR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmInvoiceReportR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmInvoiceR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR AdmInvoiceCreateR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR AdmInvoicesR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR UsersR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR UserCreateFormR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR AdmServicesSearchR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmServicesR _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (AdmServiceR _)) _ = isAdmin
    isAuthorized (AdminR (AdmServiceDeleteR _)) _ = isAdmin
    isAuthorized r@(AdminR (AdmServiceCreateFormR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmServiceEditFormR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmServiceImageR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmOfferCreateR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmExpertCreateR _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (AdmExpertsR _)) _ = isAdmin
    isAuthorized r@(AdminR (AdmExpertR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmExpertEditR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (AdmExpertDeleteR _ _)) _ = isAdmin

    isAuthorized (AdminR (AdmOfferR _)) _ = isAdmin
    isAuthorized r@(AdminR (AdmPriceR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmPriceEditR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (AdmPriceDeleteR _ _)) _ = isAdmin
    isAuthorized r@(AdminR AdmStaffR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR AdmStaffCreateR) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (AdmStaffPhotoR _)) _ = return Authorized
    isAuthorized r@(AdminR (AdmEmplR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmStaffEditR _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (AdmStaffDeleteR _)) _ = isAdmin
    isAuthorized r@(AdminR (AdmRolesR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmRoleR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmRoleCreateR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmRoleEditR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (AdmRoleDeleteR _ _)) _ = isAdmin
    isAuthorized r@(AdminR (AdmScheduleCreateR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmScheduleR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmTimeSlotR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmScheduleEditR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (AdmScheduleDeleteR _ _)) _ = isAdmin

    isAuthorized r@(AdminR (UserEditFormR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (UserR _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (UserDeleteR _)) _ = isAdmin
    isAuthorized r@(AdminR (UserPwdResetR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR UsersSearchR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmEmplUserR _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (AdmEmplUnregR _ _)) _ = isAdmin
    isAuthorized r@(AdminR AdmStaffSearchR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (AdmEmplCalendarR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (EmplCalendarSlotsR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (EmplCalendarSlotR {})) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (EmplCalendarSlotEditR {})) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (EmplCalendarSlotDeleteR {})) _ = isAdmin
    isAuthorized r@(AdminR (EmplCalendarSlotCreateR {})) _ = setUltDest r >> isAdmin

    isAuthorized r@(AdminR (BrandR _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (BrandDeleteR _ _)) _ = isAdmin
    isAuthorized r@(AdminR (BrandEditR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (BrandMarkR _ _)) _ = isAdmin
    isAuthorized (AdminR (BrandIcoR _ _)) _ = isAdmin
    isAuthorized r@(AdminR (BrandCreateR _)) _ = setUltDest r >> isAdmin

    isAuthorized r@(AdminR BusinessR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR BusinessCreateR) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessEditR _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR BusinessDeleteR) _ = isAdmin
    isAuthorized r@(AdminR (BusinessHoursR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessCalendarR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessHoursCreateR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessTimeSlotR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (BusinessTimeSlotDeleteR _ _)) _ = isAdmin
    isAuthorized r@(AdminR (BusinessHoursEditR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessCalendarSlotsR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessCalendarSlotR {})) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessCalendarSlotCreateR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessCalendarSlotEditR {})) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (BusinessCalendarSlotDeleteR {})) _ = isAdmin
    isAuthorized r@(AdminR (BusinessAboutR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessAboutCreateR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessAboutEditR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (BusinessAboutDeleteR _ _)) _ = isAdmin
    isAuthorized r@(AdminR (BusinessContactR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessContactCreateR _)) _ = setUltDest r >> isAdmin
    isAuthorized r@(AdminR (BusinessContactEditR _ _)) _ = setUltDest r >> isAdmin
    isAuthorized (AdminR (BusinessContactDeleteR _ _)) _ = isAdmin

    isAuthorized ContactR _ = return Authorized

    isAuthorized BookEndR _ = return Authorized
    isAuthorized (BookPaymentIntentCancelR _) _ = isAuthenticated
    isAuthorized (BookPayCompletionR _) _ = isAuthenticated
    isAuthorized (BookPaymentIntentR {}) _ = isAuthenticated
    isAuthorized (BookPayNowR _) _ = isAuthenticated
    isAuthorized (BookPayR _) _ = isAuthenticated
    isAuthorized BookCustomerR _ = return Authorized
    isAuthorized BookTimeR _ = return Authorized
    isAuthorized BookStaffR _ = return Authorized
    isAuthorized BookOffersR _ = return Authorized
    isAuthorized BookSearchR _ = return Authorized

    isAuthorized AppointmentsR _ = setUltDest AppointmentsR >> isAuthenticated
    isAuthorized AppointmentsSearchR _ = setUltDest AppointmentsSearchR >> isAuthenticated
    isAuthorized r@(AppointmentR _) _ = setUltDest r >> isAuthenticated
    isAuthorized (AppointmentCancelR _) _ = isAuthenticated
    isAuthorized r@(AppointmentHistR _) _ = setUltDest r >> isAuthenticated
    isAuthorized (AppointmentRescheduleR _) _ = isAuthenticated
    isAuthorized (AppointmentApproveR _) _ = isAuthenticated
    isAuthorized r@(BookingsCalendarR _) _ = setUltDest r >> isAuthenticated
    isAuthorized r@(BookingsDayListR _ _) _ = setUltDest r >> isAuthenticated
    isAuthorized r@(BookingItemR {}) _ = setUltDest r >> isAuthenticated


    isAuthorized r@(RequestsR {}) _ = setUltDest r >> isEmployee
    isAuthorized r@(RequestR {}) _ = setUltDest r >> isEmployee
    isAuthorized r@(RequestsSearchR {}) _ = setUltDest r >> isEmployee
    isAuthorized (RequestApproveR {}) _ = isEmployee
    isAuthorized (RequestFinishR {}) _ = isEmployee
    isAuthorized (RequestAssignR {}) _ = isEmployee
    isAuthorized (RequestRescheduleR {}) _ = isEmployee
    isAuthorized r@(RequestHistR {}) _ = setUltDest r >> isEmployee
    isAuthorized r@(TasksCalendarR {}) _ = setUltDest r >> isEmployee
    isAuthorized r@(TasksDayListR {}) _ = setUltDest r >> isEmployee
    isAuthorized r@(TaskItemR {}) _ = setUltDest r >> isEmployee
    isAuthorized r@(TaskHistR {}) _ = setUltDest r >> isEmployee

    isAuthorized (ProfileR _) _ = isAuthenticated
    isAuthorized (ProfileEditR _) _ = isAuthenticated
    isAuthorized (ProfileRemoveR _) _ = isAuthenticated


    isAuthorized AccountR _ = return Authorized
    isAuthorized (AccountPhotoR _) _ = return Authorized

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

    authLayout :: (MonadHandler m, HandlerSite m ~ App) => WidgetFor App () -> m Html
    authLayout w = liftHandler $ defaultLayout $ do
        ult <- getUrlRender >>= \rndr -> fromMaybe (rndr HomeR) <$>  lookupSession ultDestKey
        setTitleI MsgAuthentication
        let anError = "error"
        msgs <- getMessages
        $(widgetFile "auth/layout")


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
    authenticate (Creds plugin ident extra) = liftHandler $ do
        case plugin of
          "google" -> do
              let atoken :: Maybe Text
                  atoken = extra ^? folded . filtered ((== "accessToken") . fst) . _2
              let name :: Maybe Text
                  name = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "name" . _String
              let sub :: Maybe Text
                  sub = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "sub" . _String
              let picture :: Maybe Text
                  picture = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "picture" . _String
              let email :: Maybe Text
                  email = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "email" . _String

              case (atoken,sub) of
                (Just at,Just gid) -> do
                    Entity uid _ <- runDB $ upsert User { userName = gid
                                                        , userAuthType = UserAuthTypeGoogle
                                                        , userPassword = Nothing
                                                        , userAdmin = False
                                                        , userAnalyst = False
                                                        , userBlocked = False
                                                        , userRemoved = False
                                                        , userFullName = name
                                                        , userEmail = email
                                                        }
                                    [UserEmail =. email]
                    _ <- runDB $ upsert UserCred { userCredUser = uid
                                                 , userCredName = "google_access_token"
                                                 , userCredVal = at
                                                 }
                         [UserCredVal =. at]

                    case picture of
                      Just src -> do
                          r <- liftIO $ W.get (unpack src)
                          case (r ^? W.responseHeader "Content-Type" . to decodeUtf8, BSL.toStrict <$> r ^? W.responseBody) of
                              (Just mime, Just bs) -> do
                                  liftIO $ print mime
                                  liftIO $ print bs
                                  _ <- runDB $ upsert UserPhoto { userPhotoUser = uid
                                                                , userPhotoMime = mime
                                                                , userPhotoPhoto = bs
                                                                }
                                       [UserPhotoMime =. mime, UserPhotoPhoto =. bs]
                                  return ()
                              _otherwise -> return ()
                          return ()
                      Nothing -> return ()
                    return $ Authenticated uid
                _otherwise -> return $ UserError InvalidLogin

          "hashdb" -> do
              user <- runDB $ selectOne $ do
                  x <- from $ table @User
                  where_ $ x ^. UserName E.==. val ident
                  where_ $ not_ $ x ^. UserBlocked
                  where_ $ not_ $ x ^. UserRemoved
                  return x
              return $ case user of
                Just (Entity uid _) -> Authenticated uid
                Nothing -> UserError InvalidLogin
          _ -> return $ UserError InvalidLogin

    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [ authHashDBWithForm formLogin (Just . UniqueUser)
                      , oauth2GoogleScopedWidget googleButton ["email","openid","profile"]
                            (appGoogleClientId . appSettings $ app)
                            (appGoogleClientSecret . appSettings $ app)
                      ]

    renderAuthMessage :: App -> [Text] -> AuthMessage -> Text
    renderAuthMessage _ [] = defaultMessage
    renderAuthMessage _ ("en":_) = englishMessage
    renderAuthMessage _ ("fr":_) = frenchMessage
    renderAuthMessage _ ("ru":_) = russianMessage
    renderAuthMessage app (_:xs) = renderAuthMessage app xs


isAuthenticated :: Handler AuthResult
isAuthenticated = do
    user <- maybeAuth
    case user of
        Nothing -> do
            r <- defaultLayout $ do
                setTitleI MsgAuthenticationRequired
                msgs <- getMessages
                $(widgetFile "auth/403")
            sendResponseStatus status403 r
        Just _ -> return Authorized


isAdmin :: Handler AuthResult
isAdmin = do
    user <- maybeAuth
    case user of
        Nothing -> do
            r <- defaultLayout $ do
                setTitleI MsgAuthenticationRequired
                msgs <- getMessages
                $(widgetFile "auth/403")
            sendResponseStatus status403 r
        Just (Entity _ (User _ _ _ True _ False False _ _)) -> return Authorized
        _ -> do
            r <- defaultLayout $ do
                setTitleI MsgAuthorizationRequired
                msgs <- getMessages
                $(widgetFile "auth/403admin")
            sendResponseStatus status403 r


isAnalyst :: Handler AuthResult
isAnalyst = do
    user <- maybeAuth
    case user of
        Nothing -> do
            r <- defaultLayout $ do
                setTitleI MsgAuthenticationRequired
                msgs <- getMessages
                $(widgetFile "auth/403")
            sendResponseStatus status403 r
        Just (Entity _ (User _ _ _ _ True False False _ _)) -> return Authorized
        _ -> do
            r <- defaultLayout $ do
                setTitleI MsgAuthorizationRequired
                msgs <- getMessages
                $(widgetFile "auth/403analyst")
            sendResponseStatus status403 r


isEmployee :: Handler AuthResult
isEmployee = do
    user <- maybeAuth
    case user of
      Nothing -> do
          r <- defaultLayout $ do
                setTitleI MsgAuthenticationRequired
                msgs <- getMessages
                $(widgetFile "auth/403")
          sendResponseStatus status403 r
      Just (Entity uid _) -> do
          empl <- runDB $ selectOne $ do
              x <- from $ table @Staff
              where_ $ not_ $ isNothing_ $ x ^. StaffUser
              where_ $ x ^. StaffUser E.==. just (val uid)
              return x
          case empl of
            Nothing -> do
                r <- defaultLayout $ do
                    setTitleI MsgAuthorizationRequired
                    msgs <- getMessages
                    $(widgetFile "auth/403empl")
                sendResponseStatus status403 r
            Just _ -> return Authorized


googleButton :: Widget
googleButton = do
    toWidget [cassius|
.gsi-material-button
    -moz-user-select: none
    -webkit-user-select: none
    -ms-user-select: none
    -webkit-appearance: none
    background-color: WHITE
    background-image: none
    border: 1px solid #747775
    -webkit-border-radius: 4px
    border-radius: 4px
    -webkit-box-sizing: border-box
    box-sizing: border-box
    color: #1f1f1f
    cursor: pointer
    font-family: 'Roboto', arial, sans-serif
    font-size: 14px
    height: 40px
    letter-spacing: 0.25px
    outline: none
    overflow: hidden
    padding: 0 12px
    position: relative
    text-align: center
    -webkit-transition: background-color .218s, border-color .218s, box-shadow .218s
    transition: background-color .218s, border-color .218s, box-shadow .218s
    vertical-align: middle
    white-space: nowrap
    width: auto
    max-width: 400px
    min-width: min-content

.gsi-material-button .gsi-material-button-icon
    height: 20px
    margin-right: 12px
    min-width: 20px
    width: 20px

.gsi-material-button .gsi-material-button-content-wrapper
    -webkit-align-items: center
    align-items: center
    display: flex
    -webkit-flex-direction: row
    flex-direction: row
    -webkit-flex-wrap: nowrap
    flex-wrap: nowrap
    height: 100%
    justify-content: center
    position: relative
    width: 100%

.gsi-material-button .gsi-material-button-contents
    -webkit-flex-grow: 0
    flex-grow: 0
    font-family: 'Roboto', arial, sans-serif
    font-weight: 500
    overflow: hidden
    text-overflow: ellipsis
    vertical-align: top

.gsi-material-button .gsi-material-button-state
    -webkit-transition: opacity .218s
    transition: opacity .218s
    bottom: 0
    left: 0
    opacity: 0
    position: absolute
    right: 0
    top: 0

.gsi-material-button:disabled
    cursor: default
    background-color: #ffffff61
    border-color: #1f1f1f1f

.gsi-material-button:disabled .gsi-material-button-contents
    opacity: 38%

.gsi-material-button:disabled .gsi-material-button-icon
    opacity: 38%

.gsi-material-button:not(:disabled):active .gsi-material-button-state,
.gsi-material-button:not(:disabled):focus .gsi-material-button-state
    background-color: #303030
    opacity: 12%

.gsi-material-button:not(:disabled):hover
    -webkit-box-shadow: 0 1px 2px 0 rgba(60, 64, 67, .30), 0 1px 3px 1px rgba(60, 64, 67, .15)
    box-shadow: 0 1px 2px 0 rgba(60, 64, 67, .30), 0 1px 3px 1px rgba(60, 64, 67, .15)

.gsi-material-button:not(:disabled):hover .gsi-material-button-state
    background-color: #303030
    opacity: 8%

|]
    [whamlet|
<button.gsi-material-button>
  <div.gsi-material-button-state>
  <div.gsi-material-button-content-wrapper>
    <div.gsi-material-button-icon>
      <svg version="1.1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 48 48" xmlns:xlink="http://www.w3.org/1999/xlink" style="display: block;">
        <path fill="#EA4335" d="M24 9.5c3.54 0 6.71 1.22 9.21 3.6l6.85-6.85C35.9 2.38 30.47 0 24 0 14.62 0 6.51 5.38 2.56 13.22l7.98 6.19C12.43 13.72 17.74 9.5 24 9.5z">
        <path fill="#4285F4" d="M46.98 24.55c0-1.57-.15-3.09-.38-4.55H24v9.02h12.94c-.58 2.96-2.26 5.48-4.78 7.18l7.73 6c4.51-4.18 7.09-10.36 7.09-17.65z">
        <path fill="#FBBC05" d="M10.53 28.59c-.48-1.45-.76-2.99-.76-4.59s.27-3.14.76-4.59l-7.98-6.19C.92 16.46 0 20.12 0 24c0 3.88.92 7.54 2.56 10.78l7.97-6.19z">
        <path fill="#34A853" d="M24 48c6.48 0 11.93-2.13 15.89-5.81l-7.73-6c-2.15 1.45-4.92 2.3-8.16 2.3-6.26 0-11.57-4.22-13.47-9.91l-7.98 6.19C6.51 42.62 14.62 48 24 48z">
        <path fill="none" d="M0 0h48v48H0z">
    <span.gsi-material-button-contents>_{MsgSignInWithGoogle}
    <span style="display: none;">_{MsgSignInWithGoogle}
|]


formLogin :: Route App -> Widget
formLogin route = do
    users <- liftHandler $ unval <$> runDB (select $ do
        x :& y <- from $
            do x <- from $ table @User
               where_ $ not_ $ x ^. UserBlocked
               where_ $ not_ $ x ^. UserRemoved
               where_ $ not_ $ E.exists $ do
                   e <- from $ table @Staff
                   where_ $ e ^. StaffUser E.==. just (x ^. UserId)
               where_ $ E.exists $ do
                   b <- from $ table @Book
                   where_ $ b ^. BookCustomer E.==. x ^. UserId
               return $ x :& val False
            `unionAll_`
            do x <- from $ table @User
               where_ $ not_ $ x ^. UserBlocked
               where_ $ not_ $ x ^. UserRemoved
               where_ $ E.exists $ do
                   e <- from $ table @Staff
                   where_ $ e ^. StaffUser E.==. just (x ^. UserId)
                   where_ $ e ^. StaffStatus E.==. val EmplStatusAvailable
               return $ x :& val True

        orderBy [asc y, asc (x ^. UserName)]
        return ((((x ^. UserId, x ^. UserName), x ^. UserAdmin),x ^. UserAnalyst), y) )
    loginFormWrapper <- newIdent
    loginForm <- newIdent
    pCreateAccount <- newIdent
    $(widgetFile "auth/form")

  where
      unval = (bimap (bimap (bimap (bimap unValue unValue) unValue) unValue) unValue <$>)


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
