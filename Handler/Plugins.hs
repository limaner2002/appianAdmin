{-# LANGUAGE OverloadedStrings #-}

module Handler.Plugins where

import qualified Data.Text as T
import Import hiding ((<>))
import Yesod.Form.Bootstrap3 ( renderBootstrap3
                             , withSmallInput
                             , BootstrapFormLayout (..)
                             )
import Yesod.Table (Table)
import qualified Yesod.Table as Table
import Data.Monoid
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Julius (rawJS)
import Network.Mime (defaultMimeLookup)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
getPluginsR :: Handler Html
getPluginsR = do
  form <- generateFormPost sampleForm
  let navIdx = 0
  renderPage form

postPluginsR :: Handler Html
postPluginsR = do
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  action <- lookupPostParam "action"
  checked <- lookupPostParams "checkbox"
  let navIdx = 0
  case (result, action) of
    (FormFailure _, Just "delete") -> do
                app <- getYesod
                deleteFile checked
    (FormSuccess fi, Just "upload") -> do
                app <- getYesod
                let filePath = T.concat [appianPluginPath, fName]
                    fName = fileName fi
                runResourceT $ fileSource fi $$ sinkFile (T.unpack filePath)
                addFile (Plugin fName filePath)
    (FormFailure msg, _) -> do
                          $(logInfo) $ T.concat msg

  renderPage (formWidget, formEnctype)

sampleForm :: Form FileInfo
sampleForm = renderBootstrap3 BootstrapBasicForm $
               areq (validate fileField) "Plugin Jar File" Nothing 
    where
      validate = (check checkExtension) . (checkM checkExists)

checkExtension :: FileInfo -> Either Text FileInfo
checkExtension fi
    | T.takeEnd 4 fName == ".jar" = Right fi
    | otherwise = Left msg
  where
    fName = fileName fi
    msg = toMessage ("Please only upload .jar files" :: Text)

checkExists :: FileInfo -> Handler (Either Text FileInfo)
checkExists fi = do
  let fName = fileName fi
  mFileName <- runDB $ selectFirst [PluginFileName ==. fName] []
  case mFileName of
    Nothing -> return $ Right fi
    Just _ -> return $ Left $ toMessage ("File already exists" :: Text)

data Checkbox = Checkbox Html

boxWidget (Checkbox html) = toWidget html

checkbox :: Text -> Checkbox
checkbox = Checkbox . (H.!) (H.input H.! type_ H.! name) . A.value . H.toValue
    where
      type_ = A.type_ "checkbox"
      name = A.name "checkbox"

data Row = Row
    { rowName :: T.Text
    , box :: Checkbox
    }

fileTable :: Table App Row
fileTable = mempty
  <> Table.text "File Name" rowName
  <> Table.widget mempty (boxWidget . box)

--pluginPath = "/opt/appian/_admin/plugins/"
appianPluginPath = "/tmp/"

renderPage :: ToWidget App xml => (xml, Enctype) -> Handler Html
renderPage (formWidget, formEnctype) = do
  fileList <- getList
  let files = zip (map pluginFileName fileList) ([1..] :: [Int])
      fileNames = map pluginFileName fileList
      rows = map (\fileName -> Row fileName $ checkbox fileName) fileNames
  defaultLayout $ do
             let tableWidget = Table.buildBootstrap fileTable rows
                 navIdx = 0
             aDomId <- newIdent
             setTitle "Appian Plugins"
             $(widgetFile "plugins")
