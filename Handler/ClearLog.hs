module Handler.ClearLog where

import Import
import System.IO

getClearLogR :: AppianLog -> Handler Html
getClearLogR logType = do
  let (path, idx) = logFilePath logType
  hdl <- liftIO $ openFile path WriteMode
  liftIO $ hClose hdl
  redirect $ NavPosR idx
  
logFilePath :: AppianLog -> (FilePath, Int)
-- logFilePath JBoss = "/opt/jboss-eap-6.4/standalone/log/server.log"
-- logFilePath Application = "/opt/appian/logs/application-server.log"
logFilePath JBoss = ("/private/tmp/tmp.txt", 1)
logFilePath Application = ("/Users/josh/Desktop/tmp.txt", 2)