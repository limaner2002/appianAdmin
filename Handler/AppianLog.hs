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
  tLogFiles <- getLogFilePath
  (channel, path) <- getLogPath logType tLogFiles
  readChan <- atomically $ cloneTChan channel
  webSockets $ chatApp readChan path

  users <- getNLogUsers path
  $(logInfo) $ "Number of log users " ++ pack (show users)
  tWatchDirMap <- getLogUsers
  let tailConf = TailConf
                 { tailTLogUsers = tWatchDirMap
                 , tailTLogFiles = tLogFiles
                 , tailCurrentPath = path
                 , tailTChan = channel
                 }
  case users of
    Nothing -> do
      $(logInfo) $ "Forking tailFile"
      incLogUsers path
      liftIO $ forkIO $ tailFile tWatchDirMap tLogFiles path tailConf
      return ()
    n -> incLogUsers path

  defaultLayout $(widgetFile "logfile")

getLogPath :: MonadIO m => AppianLog -> TLogFileMap -> m (TChan ChannelMessage, FilePath)
getLogPath logType tLogFiles = do
  case logType of
    JBoss -> do
      channel <- createLog path tLogFiles
      return (channel, path)
     where
       -- path = "/opt/jboss-eap-6.4/standalone/log/server.log"
       path = "/private/tmp/tmp.txt"
    Application -> do
      channel <- createLog path tLogFiles
      return (channel, path)
     where
       -- path = "/opt/appian/logs/application-server.log"
       path = "/Users/josh/Desktop/tmp.txt"

createLog :: MonadIO m => FilePath -> TLogFileMap ->  m (TChan ChannelMessage)
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

chatApp :: TChan ChannelMessage -> FilePath -> WebSocketsT Handler ()
chatApp channel path = do
  $(logInfo) $ "Attempting to read from channel"
  contents <- liftIO $ readTChanIO channel
  res <- sendMessage contents
  case res of
    Right _ -> do
             $(logInfo) $ "Continuing the loop"
             chatApp channel path
    Left _ -> do
        $(logInfo) $ "Exiting the loop\n"
             <> "path is " <> pack path
        decLogUsers path
        return ()

sendMessage :: ChannelMessage -> WebSocketsT Handler (Either SomeException ())
sendMessage Ping = sendTextDataE ("Ping" :: Text)
sendMessage (Data contents) = sendTextDataE contents