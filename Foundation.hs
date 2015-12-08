{-# LANGUAGE RankNTypes #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
-- import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Auth.HashDB (authHashDB, getAuthIdHashDB)
import Yesod.Auth.Message   (AuthMessage (InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
import qualified Data.Text as T

import System.Directory
import System.FilePath
import BulkDownloader.Downloader

type FileName = Text
type Path = Text
type FileMap = M.Map FileName Path
type LogFileMap = M.Map FilePath AppianLogMessage
type TLogFileMap = STM.TVar LogFileMap
type WatchDirMap = M.Map FilePath Int
type TWatchDirMap = STM.TVar WatchDirMap

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings       :: AppSettings
    , appStatic         :: Static -- ^ Settings for static file serving.
    , appConnPool       :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager    :: Manager
    , appLogger         :: Logger
    , dbLock            :: STM.TVar Bool
    , currentLogUsers   :: TWatchDirMap
    , logFiles          :: TLogFileMap
    , getDownloader     :: BulkDownload
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (DownloadR (BulkDownloadR AppianInstall)) _ = isAdmin
    isAuthorized (DownloadR (BulkDownloadR _)) _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
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
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    maximumContentLength _ _ = Nothing

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
      master <- getYesod
      runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = PersonId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    -- authenticate creds = runDB $ do
    --     x <- getBy $ UniqueUser $ credsIdent creds
    --     return $ case x of
    --         Just (Entity uid _) -> Authenticated uid
    --         Nothing -> UserError InvalidLogin

    authenticate creds = do -- getAuthIdHashDB AuthR (Just . UniquePerson) creds
       let uniq = Just . UniquePerson
       muid <- maybeAuthId
       case muid of
         Just uid -> return $ Authenticated uid
         Nothing -> do
            x <- case uniq (credsIdent creds) of
                   Nothing -> return Nothing
                   Just u -> runDB (getBy u)
            case x of
              Just (Entity uid _) -> return $ Authenticated uid
              Nothing -> return $ UserError InvalidLogin

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authHashDB (Just . UniquePerson)]

    -- -- You can add other plugins like BrowserID, email or OAuth here
    -- authPlugins _ = [authBrowserId def]

    authHttpManager = getHttpManager

-- instance YesodAuth App where
--     type AuthId App = PersonId

--     -- Where to send a user after successful login
--     loginDest _ = HomeR
--     -- Where to send a user after logout
--     logoutDest _ = HomeR
--     -- Override the above two destinations when a Referer: header is present
--     redirectToReferer _ = True

--     getAuthId creds = getAuthIdHashDB AuthR (Just . UniquePerson) creds
--     -- You can add other plugins like BrowserID, email or OAuth here
--     authPlugins _ = [authHashDB (Just . UniquePerson)]

--     authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

getList :: Handler [Plugin]
getList = do
  vals <- runDB $ selectList [] []
  return $ map entityVal vals

addFile :: Plugin -> Handler ()
addFile plugin =
  runDB $ insert_ plugin

deleteFile :: [FileName] -> Handler ()
deleteFile fileNames = do
  runDB $ mapM_ (\fileName -> do
                   let key = PluginKey fileName
                   res <- get key
                   case res of
                     Nothing -> return ()
                     Just (Plugin _ path) ->
                              liftIO $ removeFile (T.unpack path) `catch` handleExists
                   delete key
                ) fileNames

 where
   handleExists e
       | isDoesNotExistError e = return ()
       | otherwise = throwIO e
-- safeRunDB action = do
--   app <- getYesod
--   let lock = dbLock app
--   atomically $ STM.modifyTVar lock $ \_ -> True
--   runDB action
--   atomically $ STM.modifyTVar lock $ \_ -> False

getNLogUsers path = do
  mLogUsers <- getLogUsers
  tWatchDirMap <- atomically $ STM.readTVar mLogUsers
  return $ M.lookup (takeDirectory path) tWatchDirMap

getLogUsers = do
  app <- getYesod
  return $ currentLogUsers app

incLogUsers path = do
  tWatchDirMap <- getLogUsers
  atomically $ STM.modifyTVar tWatchDirMap $
             \watchDirMap ->
                 insertWith (+) (takeDirectory path) 1 watchDirMap
  return ()

decLogUsers path = do
  $(logInfo) $ "decrementing for path " <> pack path
  tWatchDirMap <- getLogUsers
  atomically $ STM.modifyTVar tWatchDirMap $
             \watchDirMap ->
                 M.alter (\val ->
                            case val of
                              Nothing -> Nothing
                              Just 1 -> Nothing
                              Just n -> Just (n-1)
                       ) (takeDirectory path) watchDirMap
  watchDirMap <- atomically $ STM.readTVar tWatchDirMap
  $(logInfo) $ "tWatchDirMap: " <> pack (show watchDirMap)
  return ()

getLogFilePath = do
  app <- getYesod
  return $ logFiles app

isAdmin :: YesodAuth site => HandlerT site IO AuthResult
-- isAdmin = return Authorized
isAdmin = do
  mu <- maybeAuthId
  case mu of
      Nothing -> return AuthenticationRequired
      Just _ -> return Authorized
      -- Just uid -> do
      --    x <- runDB (getBy uid)
      --    case x of
      --      Nothing -> return $ Unauthorized "You are not allowed to view this"
      --      Just _ -> return Authorized
           -- Just (Entity a b) ->
           --     case personIdent b of
           --       "joshua_mccartney" -> return Authorized
           --       _ -> return $ Unauthorized "You are not allowed to view this"
      -- case mu of
      --   Nothing -> AuthenticationRequired
      --   Just user ->
      --       case personIdent user of
      --         "joshua_mccartney" -> Authorized
      --         _ -> Unauthorized "You are not authorized to view this"
