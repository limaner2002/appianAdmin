module Handler.AppianLog where

import Import hiding ((<>))
import Network.Mime (defaultMimeLookup)
import Yesod.WebSockets
import Control.Concurrent.STM.TChan
import Control.Concurrent (forkIO, threadDelay)
import TailFile
import Data.Monoid ((<>))
import Data.Aeson (encode)
import qualified Data.Map as M

readTChanIO = atomically . readTChan

getAppianLogR :: AppianLog -> Handler Html
getAppianLogR logType = do
  -- channels <- getChannels
  --readChan <- atomically $ dupTChan channel
  tLogFiles <- getLogFilePath
  (channel, path) <- getLogPath logType tLogFiles
  readChan <- atomically $ cloneTChan channel
  webSockets $ chatApp readChan

  users <- getNLogUsers
  $(logInfo) $ "Number of log users " ++ pack (show users)
  tLogUsers <- getLogUsers
  case users of
    0 -> do
      $(logInfo) $ "Forking tailFile"
      liftIO $ forkIO $ tailFile tLogUsers tLogFiles path
      return ()
    n -> return ()

  incLogUsers
  defaultLayout $(widgetFile "logfile")

getLogPath :: MonadIO m => AppianLog -> TLogFileMap -> m (TChan Text, FilePath)
getLogPath logType tLogFiles = do
  case logType of
    JBoss -> do
      channel <- createLog path tLogFiles
      return (channel, path)
     where path = "/opt/jboss-eap-6.4/standalone/log/server.log"
    Application -> do
      channel <- createLog path tLogFiles
      return (channel, path)
     where path = "/opt/appian/logs/application-server.log"

createLog :: MonadIO m => FilePath -> TLogFileMap ->  m (TChan Text)
createLog filePath tLogFiles = do
  liftIO $ atomically $ do
         chan <- newTChan
         modifyTVar tLogFiles $ \logFiles ->
             M.alter (\mVal ->
                      case mVal of
                        Nothing -> Just $ AppianLogMessage chan 0
                        Just val -> Just val
                     ) filePath logFiles

         logFiles <- readTVar tLogFiles
         
         case M.lookup filePath logFiles of
           Just (AppianLogMessage rChan _) -> return rChan
           Nothing -> return chan

chatApp :: TChan Text -> WebSocketsT Handler ()
chatApp channel = do
  $(logInfo) $ "Attempting to read from channel"
  contents <- liftIO $ readTChanIO channel
  res <- sendTextDataE contents
  $(logInfo) $ "Sent " <> pack (show contents) <> " to the websocket."
  case res of
    Right _ -> chatApp channel
    Left _ -> decLogUsers

logWatcher :: TChan ByteString -> TVar Int -> IO ()
logWatcher channel tLogUsers = do
  threadDelay 10000000

  users <- atomically $ readTVar tLogUsers
  case users of
    0 -> return ()
    n -> do
      atomically $ writeTChan channel ("Logfile updated!" :: ByteString)
      liftIO $ putStrLn "Wrote to the channel!"
      logWatcher channel tLogUsers
