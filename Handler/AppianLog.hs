module Handler.AppianLog where

import Import
import Network.Mime (defaultMimeLookup)
import Yesod.WebSockets
import Control.Concurrent.STM.TChan
import Control.Concurrent (forkIO, threadDelay)
import TailFile

-- getAppianLogR :: AppianLog -> Handler TypedContent
-- getAppianLogR logType = do
--   sendFile (defaultMimeLookup fullPath) $ unpack $ fullPath
--  where
--    fullPath = getLogPath logType

readTChanIO = atomically . readTChan

getAppianLogR :: AppianLog -> Handler Html
getAppianLogR logType = do
  channel <- getChannel
  
  readChan <- atomically $ dupTChan channel

  users <- getNLogUsers
  liftIO $ putStrLn $ "Number of log users " ++ pack (show users)
  tLogUsers <- getLogUsers
  case users of
    0 -> do
      liftIO $ putStrLn "Forking tailFile"
      liftIO $ forkIO $ tailFile channel tLogUsers "/private/tmp/tmp.txt"
      return ()
    n -> return ()

  incLogUsers
  webSockets $ chatApp readChan
  defaultLayout $(widgetFile "logfile")

getLogPath :: AppianLog -> Text
getLogPath JBoss = "/opt/jboss-eap-6.4/standalone/log/server.log"
getLogPath Application = "/opt/appian/logs/application-server.log"

chatApp :: TChan ByteString -> WebSocketsT Handler ()
chatApp channel = do
  contents <- liftIO $ readTChanIO channel
  res <- sendTextDataE contents
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

