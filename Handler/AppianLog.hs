module Handler.AppianLog where

import Import hiding ((<>))
import Network.Mime (defaultMimeLookup)
import Yesod.WebSockets
import Control.Concurrent.STM.TChan
import Control.Concurrent (forkIO, threadDelay)
import TailFile
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as C8
import Data.Aeson (encode)

-- getAppianLogR :: AppianLog -> Handler TypedContent
-- getAppianLogR logType = do
--   sendFile (defaultMimeLookup fullPath) $ unpack $ fullPath
--  where
--    fullPath = getLogPath logType

readTChanIO = atomically . readTChan

getAppianLogR :: AppianLog -> Handler Html
getAppianLogR logType = do
  channel <- getChannel
  --readChan <- atomically $ dupTChan channel
  webSockets $ chatApp channel

  users <- getNLogUsers
  $(logInfo) $ "Number of log users " ++ pack (show users)
  tLogUsers <- getLogUsers
  case users of
    0 -> do
      $(logInfo) $ "Forking tailFile"
      liftIO $ forkIO $ tailFile channel tLogUsers "/private/tmp/tmp.txt"
      return ()
    n -> return ()

  incLogUsers
  defaultLayout $(widgetFile "logfile")

getLogPath :: AppianLog -> Text
getLogPath JBoss = "/opt/jboss-eap-6.4/standalone/log/server.log"
getLogPath Application = "/opt/appian/logs/application-server.log"

chatApp :: TChan AppianLogMessage -> WebSocketsT Handler ()
chatApp channel = do
  $(logInfo) $ "Attempting to read from channel"
  contents <- liftIO $ readTChanIO channel
  res <- sendTextDataE $ encode contents
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

