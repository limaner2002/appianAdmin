module Handler.AppianLog where

import Import
import Network.Mime (defaultMimeLookup)

getAppianLogR :: AppianLog -> Handler TypedContent
getAppianLogR logType = do
  sendFile (defaultMimeLookup fullPath) $ unpack $ fullPath
 where
   fullPath = getLogPath logType

getLogPath :: AppianLog -> Text
getLogPath JBoss = "/opt/jboss-eap-6.4/standalone/log/server.log"
getLogPath Application = "/opt/appian/logs/application-server.log"